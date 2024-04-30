use crate::compile::output::sources::VersionedSourceFiles;
use crate::zksync::artifacts::error::Error;
use serde::{Deserialize, Serialize};

pub mod contracts;

/// The aggregated output of (multiple) compile jobs
///
/// This is effectively a solc version aware `CompilerOutput`
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct AggregatedCompilerOutput {
    /// all errors from all `CompilerOutput`
    pub errors: Vec<Error>,
    /// All source files combined with the solc version used to compile them
    pub sources: VersionedSourceFiles,
    /// All compiled contracts combined with the solc version used to compile them
    pub contracts: VersionedContracts,
    // All the `BuildInfo`s of solc invocations.
    pub build_infos: BTreeMap<Version, RawBuildInfo>,
}
