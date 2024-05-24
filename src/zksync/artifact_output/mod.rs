use std::{
    borrow::Cow,
    collections::HashSet,
    ffi::OsString,
    path::{Path, PathBuf},
};

use semver::Version;

use crate::{
    artifact_output::{ArtifactId, Artifacts},
    artifacts::bytecode::BytecodeObject,
    zksync::{
        artifact_output::zk::ZkContractArtifact,
        artifacts::{bytecode::Bytecode, contract::CompactContractBytecodeCow},
        cache::SolFilesCache,
    },
};

pub mod files;
pub mod zk;

pub trait Artifact {
    /// Returns the reference to the `bytecode`
    fn get_bytecode(&self) -> Option<Cow<'_, Bytecode>> {
        self.get_contract_bytecode().bytecode
    }

    /// Returns the reference to the `bytecode` object
    fn get_bytecode_object(&self) -> Option<Cow<'_, BytecodeObject>> {
        let val = match self.get_bytecode()? {
            Cow::Borrowed(b) => Cow::Borrowed(&b.object),
            Cow::Owned(b) => Cow::Owned(b.object),
        };
        Some(val)
    }

    /// Returns the reference of container type for abi, compact bytecode and deployed bytecode if
    /// available
    fn get_contract_bytecode(&self) -> CompactContractBytecodeCow<'_>;
}

impl<T> Artifact for T
where
    for<'a> &'a T: Into<CompactContractBytecodeCow<'a>>,
{
    fn get_contract_bytecode(&self) -> CompactContractBytecodeCow<'_> {
        self.into()
    }
}

// solc Artifacts overrides (for methods that require the
// `ArtifactOutput` trait)

/// Returns an iterator over _all_ artifacts and `<file name:contract name>`.
pub fn artifacts_artifacts(
    artifacts: &Artifacts<ZkContractArtifact>,
) -> impl Iterator<Item = (ArtifactId, &ZkContractArtifact)> + '_ {
    artifacts.0.iter().flat_map(|(file, contract_artifacts)| {
        contract_artifacts.iter().flat_map(move |(_contract_name, artifacts)| {
            let source = PathBuf::from(file.clone());
            artifacts.iter().filter_map(move |artifact| {
                contract_name(&artifact.file).map(|name| {
                    (
                        ArtifactId {
                            path: PathBuf::from(&artifact.file),
                            name,
                            source: source.clone(),
                            version: artifact.version.clone(),
                        }
                        .with_slashed_paths(),
                        &artifact.artifact,
                    )
                })
            })
        })
    })
}

// ArtifactOutput trait methods that don't require self are
// defined as standalone functions here (We don't redefine the
// trait for zksolc)

/// Returns the file name for the contract's artifact
/// `Greeter.json`
fn output_file_name(name: impl AsRef<str>) -> PathBuf {
    format!("{}.json", name.as_ref()).into()
}

/// Returns the file name for the contract's artifact and the given version
/// `Greeter.0.8.11.json`
fn output_file_name_versioned(name: impl AsRef<str>, version: &Version) -> PathBuf {
    format!("{}.{}.{}.{}.json", name.as_ref(), version.major, version.minor, version.patch).into()
}

/// Returns the appropriate file name for the conflicting file.
///
/// This should ensure that the resulting `PathBuf` is conflict free, which could be possible if
/// there are two separate contract files (in different folders) that contain the same contract:
fn conflict_free_output_file(
    already_taken: &HashSet<PathBuf>,
    conflict: PathBuf,
    contract_file: impl AsRef<Path>,
    artifacts_folder: impl AsRef<Path>,
) -> PathBuf {
    let artifacts_folder = artifacts_folder.as_ref();
    let mut rel_candidate = conflict;
    if let Ok(stripped) = rel_candidate.strip_prefix(artifacts_folder) {
        rel_candidate = stripped.to_path_buf();
    }
    #[allow(clippy::redundant_clone)] // false positive
    let mut candidate = rel_candidate.clone();
    let contract_file = contract_file.as_ref();
    let mut current_parent = contract_file.parent();

    while let Some(parent_name) = current_parent.and_then(|f| f.file_name()) {
        // this is problematic if both files are absolute
        candidate = Path::new(parent_name).join(&candidate);
        let out_path = artifacts_folder.join(&candidate);
        if !already_taken.contains(&out_path) {
            trace!("found alternative output file={:?} for {:?}", out_path, contract_file);
            return out_path;
        }
        current_parent = current_parent.and_then(|f| f.parent());
    }

    // this means we haven't found an alternative yet, which shouldn't actually happen since
    // `contract_file` are unique, but just to be safe, handle this case in which case
    // we simply numerate the parent folder

    trace!("no conflict free output file found after traversing the file");

    let mut num = 1;

    loop {
        // this will attempt to find an alternate path by numerating the first component in the
        // path: `<root>+_<num>/....sol`
        let mut components = rel_candidate.components();
        let first = components.next().expect("path not empty");
        let name = first.as_os_str();
        let mut numerated = OsString::with_capacity(name.len() + 2);
        numerated.push(name);
        numerated.push("_");
        numerated.push(num.to_string());

        let candidate: PathBuf = Some(numerated.as_os_str())
            .into_iter()
            .chain(components.map(|c| c.as_os_str()))
            .collect();
        if !already_taken.contains(&candidate) {
            trace!("found alternative output file={:?} for {:?}", candidate, contract_file);
            return candidate;
        }

        num += 1;
    }
}
/// Returns the path to the contract's artifact location based on the contract's file and name
///
/// This returns `contract.sol/contract.json` by default
pub fn output_file(contract_file: impl AsRef<Path>, name: impl AsRef<str>) -> PathBuf {
    let name = name.as_ref();
    contract_file
        .as_ref()
        .file_name()
        .map(Path::new)
        .map(|p| p.join(output_file_name(name)))
        .unwrap_or_else(|| output_file_name(name))
}

/// Returns the path to the contract's artifact location based on the contract's file, name and
/// version
///
/// This returns `contract.sol/contract.0.8.11.json` by default
pub fn output_file_versioned(
    contract_file: impl AsRef<Path>,
    name: impl AsRef<str>,
    version: &Version,
) -> PathBuf {
    let name = name.as_ref();
    contract_file
        .as_ref()
        .file_name()
        .map(Path::new)
        .map(|p| p.join(output_file_name_versioned(name, version)))
        .unwrap_or_else(|| output_file_name_versioned(name, version))
}

pub fn contract_name(file: impl AsRef<Path>) -> Option<String> {
    file.as_ref().file_stem().and_then(|s| s.to_str().map(|s| s.to_string()))
}

// === impl OutputContext
//
/// Additional context to use during [`ArtifactOutput::on_output()`]
#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct OutputContext<'a> {
    /// Cache file of the project or empty if no caching is enabled
    ///
    /// This context is required for partially cached recompile with conflicting files, so that we
    /// can use the same adjusted output path for conflicting files like:
    ///
    /// ```text
    /// src
    /// ├── a.sol
    /// └── inner
    ///     └── a.sol
    /// ```
    pub cache: Cow<'a, SolFilesCache>,
}

impl<'a> OutputContext<'a> {
    /// Create a new context with the given cache file
    pub fn new(cache: &'a SolFilesCache) -> Self {
        Self { cache: Cow::Borrowed(cache) }
    }

    /// Returns the path of the already existing artifact for the `contract` of the `file` compiled
    /// with the `version`.
    ///
    /// Returns `None` if no file exists
    pub fn existing_artifact(
        &self,
        file: impl AsRef<Path>,
        contract: &str,
        version: &Version,
    ) -> Option<&PathBuf> {
        self.cache.entry(file)?.find_artifact(contract, version)
    }
}
