use std::path::{Path, PathBuf};

use semver::Version;

use crate::{
    artifact_output::{ArtifactId, Artifacts},
    zksync::artifact_output::zk::ZkContractArtifact,
};

pub mod files;
pub mod zk;

// solc Artifacts overrides (for methods that require the
// `ArtifactOutput` trait)
/// Returns an iterator over _all_ artifacts and `<file name:contract name>`.
pub fn artifacts_artifacts(
    artifacts: &Artifacts<ZkContractArtifact>,
) -> impl Iterator<Item = (ArtifactId, &ZkContractArtifact)> + '_ {
    artifacts.0.iter().flat_map(|(file, contract_artifacts)| {
        contract_artifacts.iter().flat_map(move |(_contract_name, artifacts)| {
            let source = file;
            artifacts.iter().filter_map(move |artifact| {
                contract_name(&artifact.file).map(|name| {
                    (
                        ArtifactId {
                            path: PathBuf::from(&artifact.file),
                            name,
                            source: source.clone(),
                            version: artifact.version.clone(),
                            build_id: artifact.build_id.clone(),
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
