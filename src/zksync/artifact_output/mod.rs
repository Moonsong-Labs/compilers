use std::{
    borrow::Cow,
    collections::HashSet,
    ffi::OsString,
    path::{Path, PathBuf},
};

use semver::Version;

use crate::artifacts::bytecode::BytecodeObject;
use crate::zksync::{
    artifacts::{bytecode::Bytecode, contract::CompactContractBytecodeCow},
    cache::SolFilesCache,
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
/*

pub trait ArtifactOutput {
    /// Represents the artifact that will be stored for a `Contract`
    type Artifact: Artifact + DeserializeOwned + Serialize + fmt::Debug + Send + Sync;

    fn on_output(
        &self,
        contracts: &VersionedContracts,
        sources: &VersionedSourceFiles,
        layout: &ProjectPathsConfig,
        ctx: OutputContext<'_>,
    ) -> Result<Artifacts<Self::Artifact>> {
        let mut artifacts = self.output_to_artifacts(contracts, sources, ctx, layout);
        fs::create_dir_all(&layout.artifacts).map_err(|err| {
            error!(dir=?layout.artifacts, "Failed to create artifacts folder");
            SolcIoError::new(err, &layout.artifacts)
        })?;

        artifacts.join_all(&layout.artifacts);
        artifacts.write_all()?;

        self.write_extras(contracts, &artifacts)?;

        Ok(artifacts)
    }

    /// Write additional files for the contract
    fn write_contract_extras(&self, contract: &Contract, file: &Path) -> Result<()> {
        ExtraOutputFiles::all().write_extras(contract, file)
    }

    /// Writes additional files for the contracts if the included in the `Contract`, such as `ir`,
    /// `ewasm`, `iropt`.
    ///
    /// By default, these fields are _not_ enabled in the [`crate::artifacts::Settings`], see
    /// [`crate::artifacts::output_selection::OutputSelection::default_output_selection()`], and the
    /// respective fields of the [`Contract`] will `None`. If they'll be manually added to the
    /// `output_selection`, then we're also creating individual files for this output, such as
    /// `Greeter.iropt`, `Gretter.ewasm`
    fn write_extras(
        &self,
        contracts: &VersionedContracts,
        artifacts: &Artifacts<Self::Artifact>,
    ) -> Result<()> {
        for (file, contracts) in contracts.as_ref().iter() {
            for (name, versioned_contracts) in contracts {
                for c in versioned_contracts {
                    if let Some(artifact) = artifacts.find_artifact(file, name, &c.version) {
                        let file = &artifact.file;
                        utils::create_parent_dir_all(file)?;
                        self.write_contract_extras(&c.contract, file)?;
                    }
                }
            }
        }

        Ok(())
    }

    /// Returns the file name for the contract's artifact
    /// `Greeter.json`
    fn output_file_name(name: impl AsRef<str>) -> PathBuf {
        format!("{}.json", name.as_ref()).into()
    }

    /// Returns the file name for the contract's artifact and the given version
    /// `Greeter.0.8.11.json`
    fn output_file_name_versioned(name: impl AsRef<str>, version: &Version) -> PathBuf {
        format!("{}.{}.{}.{}.json", name.as_ref(), version.major, version.minor, version.patch)
            .into()
    }

    /// Returns the appropriate file name for the conflicting file.
    ///
    /// This should ensure that the resulting `PathBuf` is conflict free, which could be possible if
    /// there are two separate contract files (in different folders) that contain the same contract:
    ///
    /// `src/A.sol::A`
    /// `src/nested/A.sol::A`
    ///
    /// Which would result in the same `PathBuf` if only the file and contract name is taken into
    /// account, [`Self::output_file`].
    ///
    /// This return a unique output file
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
    fn output_file(contract_file: impl AsRef<Path>, name: impl AsRef<str>) -> PathBuf {
        let name = name.as_ref();
        contract_file
            .as_ref()
            .file_name()
            .map(Path::new)
            .map(|p| p.join(Self::output_file_name(name)))
            .unwrap_or_else(|| Self::output_file_name(name))
    }

    /// Returns the path to the contract's artifact location based on the contract's file, name and
    /// version
    ///
    /// This returns `contract.sol/contract.0.8.11.json` by default
    fn output_file_versioned(
        contract_file: impl AsRef<Path>,
        name: impl AsRef<str>,
        version: &Version,
    ) -> PathBuf {
        let name = name.as_ref();
        contract_file
            .as_ref()
            .file_name()
            .map(Path::new)
            .map(|p| p.join(Self::output_file_name_versioned(name, version)))
            .unwrap_or_else(|| Self::output_file_name_versioned(name, version))
    }

    /// The inverse of `contract_file_name`
    ///
    /// Expected to return the solidity contract's name derived from the file path
    /// `sources/Greeter.sol` -> `Greeter`
    fn contract_name(file: impl AsRef<Path>) -> Option<String> {
        file.as_ref().file_stem().and_then(|s| s.to_str().map(|s| s.to_string()))
    }

    /// Whether the corresponding artifact of the given contract file and name exists
    fn output_exists(
        contract_file: impl AsRef<Path>,
        name: impl AsRef<str>,
        root: impl AsRef<Path>,
    ) -> bool {
        root.as_ref().join(Self::output_file(contract_file, name)).exists()
    }

    /// Read the artifact that's stored at the given path
    ///
    /// # Errors
    ///
    /// Returns an error if
    ///     - The file does not exist
    ///     - The file's content couldn't be deserialized into the `Artifact` type
    fn read_cached_artifact(path: impl AsRef<Path>) -> Result<Self::Artifact> {
        crate::utils::read_json_file(path)
    }

    /// Read the cached artifacts that are located the paths the iterator yields
    ///
    /// See [`Self::read_cached_artifact()`]
    fn read_cached_artifacts<T, I>(files: I) -> Result<BTreeMap<PathBuf, Self::Artifact>>
    where
        I: IntoIterator<Item = T>,
        T: Into<PathBuf>,
    {
        let mut artifacts = BTreeMap::default();
        for path in files.into_iter() {
            let path = path.into();
            let artifact = Self::read_cached_artifact(&path)?;
            artifacts.insert(path, artifact);
        }
        Ok(artifacts)
    }

    /// Convert a contract to the artifact type
    ///
    /// This is the core conversion function that takes care of converting a `Contract` into the
    /// associated `Artifact` type.
    /// The `SourceFile` is also provided
    fn contract_to_artifact(
        &self,
        _file: &str,
        _name: &str,
        contract: Contract,
        source_file: Option<&SourceFile>,
    ) -> Self::Artifact;

    /// Convert the compiler output into a set of artifacts
    ///
    /// **Note:** This does only convert, but _NOT_ write the artifacts to disk, See
    /// [`Self::on_output()`]
    fn output_to_artifacts(
        &self,
        contracts: &VersionedContracts,
        sources: &VersionedSourceFiles,
        ctx: OutputContext<'_>,
        layout: &ProjectPathsConfig,
    ) -> Artifacts<Self::Artifact> {
        let mut artifacts = ArtifactsMap::new();

        // this tracks all the `SourceFile`s that we successfully mapped to a contract
        let mut non_standalone_sources = HashSet::new();

        // this holds all output files and the contract(s) it belongs to
        let artifact_files = contracts.artifact_files::<Self>(&ctx);

        // this tracks the final artifacts, which we use as lookup for checking conflicts when
        // converting stand-alone artifacts in the next step
        let mut final_artifact_paths = HashSet::new();

        for contracts in artifact_files.files.into_values() {
            for (idx, mapped_contract) in contracts.iter().enumerate() {
                let MappedContract { file, name, contract, artifact_path } = mapped_contract;
                // track `SourceFile`s that can be mapped to contracts
                let source_file = sources.find_file_and_version(file, &contract.version);

                if let Some(source) = source_file {
                    non_standalone_sources.insert((source.id, &contract.version));
                }

                let mut artifact_path = artifact_path.clone();

                if contracts.len() > 1 {
                    // naming conflict where the `artifact_path` belongs to two conflicting
                    // contracts need to adjust the paths properly

                    // we keep the top most conflicting file unchanged
                    let is_top_most =
                        contracts.iter().enumerate().filter(|(i, _)| *i != idx).all(|(_, c)| {
                            Path::new(file).components().count()
                                < Path::new(c.file).components().count()
                        });
                    if !is_top_most {
                        // we resolve the conflicting by finding a new unique, alternative path
                        artifact_path = Self::conflict_free_output_file(
                            &final_artifact_paths,
                            artifact_path,
                            file,
                            &layout.artifacts,
                        );
                    }
                }

                final_artifact_paths.insert(artifact_path.clone());

                let artifact =
                    self.contract_to_artifact(file, name, contract.contract.clone(), source_file);

                let artifact = ArtifactFile {
                    artifact,
                    file: artifact_path,
                    version: contract.version.clone(),
                };

                artifacts
                    .entry(file.to_string())
                    .or_default()
                    .entry(name.to_string())
                    .or_default()
                    .push(artifact);
            }
        }

        // extend with standalone source files and convert them to artifacts
        // this is unfortunately necessary, so we can "mock" `Artifacts` for solidity files without
        // any contract definition, which are not included in the `CompilerOutput` but we want to
        // create Artifacts for them regardless
        for (file, sources) in sources.as_ref().iter() {
            for source in sources {
                if !non_standalone_sources.contains(&(source.source_file.id, &source.version)) {
                    // scan the ast as a safe measure to ensure this file does not include any
                    // source units
                    // there's also no need to create a standalone artifact for source files that
                    // don't contain an ast
                    if source.source_file.contains_contract_definition()
                        || source.source_file.ast.is_none()
                    {
                        continue;
                    }

                    // we use file and file stem
                    if let Some(name) = Path::new(file).file_stem().and_then(|stem| stem.to_str()) {
                        if let Some(artifact) =
                            self.standalone_source_file_to_artifact(file, source)
                        {
                            let mut artifact_path = if sources.len() > 1 {
                                Self::output_file_versioned(file, name, &source.version)
                            } else {
                                Self::output_file(file, name)
                            };

                            if final_artifact_paths.contains(&artifact_path) {
                                // preventing conflict
                                artifact_path = Self::conflict_free_output_file(
                                    &final_artifact_paths,
                                    artifact_path,
                                    file,
                                    &layout.artifacts,
                                );
                                final_artifact_paths.insert(artifact_path.clone());
                            }

                            let entries = artifacts
                                .entry(file.to_string())
                                .or_default()
                                .entry(name.to_string())
                                .or_default();

                            if entries.iter().all(|entry| entry.version != source.version) {
                                entries.push(ArtifactFile {
                                    artifact,
                                    file: artifact_path,
                                    version: source.version.clone(),
                                });
                            }
                        }
                    }
                }
            }
        }

        Artifacts(artifacts)
    }

    /// This converts a `SourceFile` that doesn't contain _any_ contract definitions (interfaces,
    /// contracts, libraries) to an artifact.
    ///
    /// We do this because not all `SourceFile`s emitted by solc have at least 1 corresponding entry
    /// in the `contracts`
    /// section of the solc output. For example for an `errors.sol` that only contains custom error
    /// definitions and no contract, no `Contract` object will be generated by solc. However, we
    /// still want to emit an `Artifact` for that file that may include the `ast`, docs etc.,
    /// because other tools depend on this, such as slither.
    fn standalone_source_file_to_artifact(
        &self,
        _path: &str,
        _file: &VersionedSourceFile,
    ) -> Option<Self::Artifact>;
}

*/

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
///
/// `src/A.sol::A`
/// `src/nested/A.sol::A`
///
/// Which would result in the same `PathBuf` if only the file and contract name is taken into
/// account, [`Self::output_file`].
///
/// This return a unique output file
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
