use crate::{
    artifacts::{
        serde_helpers, EvmVersion, FileToContractsMap, Libraries, Source, SourceFile, SourceFiles,
        Sources,
    },
    error::SolcIoError,
    remappings::Remapping,
};

use semver::Version;
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, HashSet},
    fmt,
    path::Path,
    str::FromStr,
};

pub mod bytecode;
pub mod contract;
pub mod error;
pub mod output_selection;

use self::bytecode::Bytecode;
use self::contract::{CompactContractRef, Contract};
use self::error::Error;
use self::output_selection::OutputSelection;

const SOLIDITY: &str = "Solidity";
const YUL: &str = "Yul";

/// file -> (contract name -> Contract)
///
pub type Contracts = FileToContractsMap<Contract>;

/// Input type `solc` expects.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CompilerInput {
    pub language: String,
    pub sources: Sources,
    pub settings: Settings,
}

/// Default `language` field is set to `"Solidity"`.
impl Default for CompilerInput {
    fn default() -> Self {
        CompilerInput {
            language: SOLIDITY.to_string(),
            sources: Sources::default(),
            settings: Settings::default(),
        }
    }
}

impl CompilerInput {
    /// Reads all contracts found under the path
    pub fn new(path: impl AsRef<Path>) -> Result<Vec<Self>, SolcIoError> {
        Source::read_all_from(path.as_ref()).map(Self::with_sources)
    }

    /// Creates a new [CompilerInput]s with default settings and the given sources
    ///
    /// A [CompilerInput] expects a language setting, supported by solc are solidity or yul.
    /// In case the `sources` is a mix of solidity and yul files, 2 CompilerInputs are returned
    pub fn with_sources(sources: Sources) -> Vec<Self> {
        let mut solidity_sources = BTreeMap::new();
        let mut yul_sources = BTreeMap::new();
        for (path, source) in sources {
            if path.extension() == Some(std::ffi::OsStr::new("yul")) {
                yul_sources.insert(path, source);
            } else {
                solidity_sources.insert(path, source);
            }
        }
        let mut res = Vec::new();
        if !solidity_sources.is_empty() {
            res.push(Self {
                language: SOLIDITY.to_string(),
                sources: solidity_sources,
                settings: Default::default(),
            });
        }
        if !yul_sources.is_empty() {
            res.push(Self {
                language: YUL.to_string(),
                sources: yul_sources,
                settings: Default::default(),
            });
        }
        res
    }

    /// Sets the settings for compilation
    #[must_use]
    pub fn settings(mut self, mut settings: Settings) -> Self {
        if self.is_yul() && !settings.remappings.is_empty() {
            warn!("omitting remappings supplied for the yul sources");
            settings.remappings = vec![];
        }
        self.settings = settings;
        self
    }

    /// Normalizes the EVM version used in the settings to be up to the latest one
    /// supported by the provided compiler version.
    #[must_use]
    pub fn normalize_evm_version(mut self, version: &Version) -> Self {
        if let Some(evm_version) = &mut self.settings.evm_version {
            self.settings.evm_version = evm_version.normalize_version(version);
        }
        self
    }

    #[must_use]
    pub fn with_remappings(mut self, remappings: Vec<Remapping>) -> Self {
        if self.is_yul() {
            warn!("omitting remappings supplied for the yul sources");
        } else {
            self.settings.remappings = remappings;
        }
        self
    }

    /// The flag indicating whether the current [CompilerInput] is
    /// constructed for the yul sources
    pub fn is_yul(&self) -> bool {
        self.language == YUL
    }

    /// Removes the `base` path from all source files
    pub fn strip_prefix(mut self, base: impl AsRef<Path>) -> Self {
        let base = base.as_ref();
        self.sources = self
            .sources
            .into_iter()
            .map(|(path, s)| (path.strip_prefix(base).map(Into::into).unwrap_or(path), s))
            .collect();
        self
    }

    /// Similar to `Self::strip_prefix()`. Remove a base path from all
    /// sources _and_ all paths in solc settings such as remappings
    ///
    /// See also `solc --base-path`
    pub fn with_base_path(mut self, base: impl AsRef<Path>) -> Self {
        let base = base.as_ref();
        self.settings = self.settings.with_base_path(base);
        self.strip_prefix(base)
    }
}

/// zksolc standard json input settings. See:
/// https://docs.zksync.io/zk-stack/components/compiler/toolchain/solidity.html#standard-json for differences
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Settings {
    // same
    /// Change compilation pipeline to go through the Yul intermediate representation. This is
    /// false by default.
    #[serde(rename = "viaIR", default, skip_serializing_if = "Option::is_none")]
    pub via_ir: Option<bool>,
    // TODO: era-compiler-solidity uses a BTreeSet of strings. In theory the serialization
    // should be the same but maybe we should double check
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub remappings: Vec<Remapping>,
    #[serde(
        default,
        with = "serde_helpers::display_from_str_opt",
        skip_serializing_if = "Option::is_none"
    )]
    pub evm_version: Option<EvmVersion>,

    // check if the same (and use `compilers version`)
    /// This field can be used to select desired outputs based
    /// on file and contract names.
    /// If this field is omitted, then the compiler loads and does type
    /// checking, but will not generate any outputs apart from errors.
    #[serde(default)]
    pub output_selection: OutputSelection,

    pub optimizer: Optimizer,
    /// Metadata settings
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub metadata: Option<SettingsMetadata>,
    #[serde(default)]
    pub libraries: Libraries,
    /// Switch to missing deployable libraries detection mode.
    /// Contracts are not compiled in this mode, and all compilation artifacts are not included.
    #[serde(default)]
    pub detect_missing_libraries: bool,

    // zksolc arguments
    /// A flag indicating whether to enable the system contract compilation mode.
    #[serde(default)]
    pub system_mode: bool,
    /// A flag indicating whether to forcibly switch to the EVM legacy assembly pipeline.
    #[serde(default)]
    pub force_evmla: bool,
    /// The path to the solc compiler to use along zksolc.
    pub solc: Option<std::path::PathBuf>,
}

impl Settings {
    /// Creates a new `Settings` instance with the given `output_selection`
    pub fn new(output_selection: impl Into<OutputSelection>) -> Self {
        Self { output_selection: output_selection.into(), ..Default::default() }
    }

    /// Strips `base` from all paths
    pub fn with_base_path(mut self, base: impl AsRef<Path>) -> Self {
        let base = base.as_ref();
        self.remappings.iter_mut().for_each(|r| {
            r.strip_prefix(base);
        });

        self.libraries.libs = self
            .libraries
            .libs
            .into_iter()
            .map(|(file, libs)| (file.strip_prefix(base).map(Into::into).unwrap_or(file), libs))
            .collect();

        self
    }
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            optimizer: Default::default(),
            metadata: None,
            output_selection: Default::default(),
            evm_version: Some(EvmVersion::default()),
            via_ir: None,
            libraries: Default::default(),
            remappings: Default::default(),
            system_mode: false,
            force_evmla: false,
            detect_missing_libraries: false,
            solc: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Optimizer {
    // TODO: does this have to be an option?
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    /// Switch optimizer components on or off in detail.
    /// The "enabled" switch above provides two defaults which can be
    /// tweaked here. If "details" is given, "enabled" can be omitted.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub details: Option<OptimizerDetails>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mode: Option<char>,
    /// Whether to try to recompile with -Oz if the bytecode is too large.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fallback_to_optimizing_for_size: Option<bool>,
    /// Whether to disable the system request memoization.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub disable_system_request_memoization: Option<bool>,
    /// Set the jump table density threshold.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub jump_table_density_threshold: Option<u32>,
}

impl Optimizer {
    pub fn disable(&mut self) {
        self.enabled.take();
    }

    pub fn enable(&mut self) {
        self.enabled = Some(true)
    }
}

impl Default for Optimizer {
    fn default() -> Self {
        Self {
            enabled: Some(false),
            mode: None,
            fallback_to_optimizing_for_size: None,
            disable_system_request_memoization: None,
            jump_table_density_threshold: None,
            details: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct OptimizerDetails {
    /// The peephole optimizer is always on if no details are given,
    /// use details to switch it off.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub peephole: Option<bool>,
    /// The inliner is always on if no details are given,
    /// use details to switch it off.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub inliner: Option<bool>,
    /// The unused jumpdest remover is always on if no details are given,
    /// use details to switch it off.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub jumpdest_remover: Option<bool>,
    /// Sometimes re-orders literals in commutative operations.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub order_literals: Option<bool>,
    /// Removes duplicate code blocks
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub deduplicate: Option<bool>,
    /// Common subexpression elimination, this is the most complicated step but
    /// can also provide the largest gain.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cse: Option<bool>,
    /// Optimize representation of literal numbers and strings in code.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub constant_optimizer: Option<bool>,
}

impl OptimizerDetails {
    /// Returns true if no settings are set.
    pub fn is_empty(&self) -> bool {
        self.peephole.is_none()
            && self.inliner.is_none()
            && self.jumpdest_remover.is_none()
            && self.order_literals.is_none()
            && self.deduplicate.is_none()
            && self.cse.is_none()
            && self.constant_optimizer.is_none()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SettingsMetadata {
    /// Use the given hash method for the metadata hash that is appended to the bytecode.
    /// The metadata hash can be removed from the bytecode via option "none".
    /// `zksolc` only supports keccak256
    #[serde(
        default,
        rename = "bytecodeHash",
        skip_serializing_if = "Option::is_none",
        with = "serde_helpers::display_from_str_opt"
    )]
    pub bytecode_hash: Option<BytecodeHash>,
}

impl SettingsMetadata {
    pub fn new(hash: BytecodeHash) -> Self {
        Self { bytecode_hash: Some(hash) }
    }
}

impl From<BytecodeHash> for SettingsMetadata {
    fn from(hash: BytecodeHash) -> Self {
        Self { bytecode_hash: Some(hash) }
    }
}

/// Determines the hash method for the metadata hash that is appended to the bytecode.
/// Zksolc only supports keccak256
#[derive(Clone, Debug, Default, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BytecodeHash {
    /// Do not include bytecode hash.
    #[default]
    #[serde(rename = "none")]
    None,
    /// The default keccak256 hash.
    #[serde(rename = "keccak256")]
    Keccak256,
}

impl FromStr for BytecodeHash {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "none" => Ok(BytecodeHash::None),
            "keccak256" => Ok(BytecodeHash::Keccak256),
            s => Err(format!("Unknown bytecode hash: {s}")),
        }
    }
}

impl fmt::Display for BytecodeHash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BytecodeHash::Keccak256 => "keccak256",
            BytecodeHash::None => "none",
        };
        f.write_str(s)
    }
}

/// Output type `solc` produces
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct CompilerOutput {
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub errors: Vec<Error>,
    #[serde(default)]
    pub sources: BTreeMap<String, SourceFile>,
    #[serde(default)]
    pub contracts: FileToContractsMap<Contract>,
    /// The `solc` compiler version.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
    /// The `solc` compiler long version.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub long_version: Option<String>,
    /// The `zksolc` compiler version.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub zk_version: Option<String>,
}

impl CompilerOutput {
    /// Whether the output contains a compiler error
    pub fn has_error(&self) -> bool {
        self.errors.iter().any(|err| err.severity.is_error())
    }

    /// Retains only those files the given iterator yields
    ///
    /// In other words, removes all contracts for files not included in the iterator
    pub fn retain_files<'a, I>(&mut self, files: I)
    where
        I: IntoIterator<Item = &'a str>,
    {
        // Note: use `to_lowercase` here because zksolc not necessarily emits the exact file name,
        // e.g. `src/utils/upgradeProxy.sol` is emitted as `src/utils/UpgradeProxy.sol`
        let files: HashSet<_> = files.into_iter().map(|s| s.to_lowercase()).collect();
        self.contracts.retain(|f, _| files.contains(f.to_lowercase().as_str()));
        self.sources.retain(|f, _| files.contains(f.to_lowercase().as_str()));
    }

    pub fn merge(&mut self, other: CompilerOutput) {
        self.errors.extend(other.errors);
        self.contracts.extend(other.contracts);
        self.sources.extend(other.sources);
    }

    /// Returns the output's source files and contracts separately, wrapped in helper types that
    /// provide several helper methods
    pub fn split(self) -> (SourceFiles, OutputContracts) {
        (SourceFiles(self.sources), OutputContracts(self.contracts))
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Evm {
    /// The contract EraVM assembly code.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub assembly: Option<String>,
    /// The contract EVM legacy assembly code.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub legacy_assembly: Option<serde_json::Value>,
    /// The contract bytecode.
    /// Is reset by that of EraVM before yielding the compiled project artifacts.
    pub bytecode: Option<Bytecode>,
    /// The list of function hashes
    #[serde(default, skip_serializing_if = "::std::collections::BTreeMap::is_empty")]
    pub method_identifiers: BTreeMap<String, String>,
    /// The extra EVMLA metadata.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extra_metadata: Option<ExtraMetadata>,
}

///
/// The `solc --standard-json` output contract EVM extra metadata.
///
#[derive(Debug, Default, Serialize, Deserialize, Clone, Eq, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ExtraMetadata {
    /// The list of recursive functions.
    #[serde(default = "Vec::new")]
    pub recursive_functions: Vec<RecursiveFunction>,
}

///
/// The `solc --standard-json` output contract EVM recursive function.
///
#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct RecursiveFunction {
    /// The function name.
    pub name: String,
    /// The creation code function block tag.
    pub creation_tag: Option<usize>,
    /// The runtime code function block tag.
    pub runtime_tag: Option<usize>,
    /// The number of input arguments.
    #[serde(rename = "totalParamSize")]
    pub input_size: usize,
    /// The number of output arguments.
    #[serde(rename = "totalRetParamSize")]
    pub output_size: usize,
}

/// A wrapper helper type for the `Contracts` type alias
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct OutputContracts(pub Contracts);

impl OutputContracts {
    /// Returns an iterator over all contracts and their source names.
    pub fn into_contracts(self) -> impl Iterator<Item = (String, Contract)> {
        self.0.into_values().flatten()
    }

    /// Iterate over all contracts and their names
    pub fn contracts_iter(&self) -> impl Iterator<Item = (&String, &Contract)> {
        self.0.values().flatten()
    }

    /// Finds the _first_ contract with the given name
    pub fn find(&self, contract: impl AsRef<str>) -> Option<CompactContractRef<'_>> {
        let contract_name = contract.as_ref();
        self.contracts_iter().find_map(|(name, contract)| {
            (name == contract_name).then(|| CompactContractRef::from(contract))
        })
    }

    /// Finds the first contract with the given name and removes it from the set
    pub fn remove(&mut self, contract: impl AsRef<str>) -> Option<Contract> {
        let contract_name = contract.as_ref();
        self.0.values_mut().find_map(|c| c.remove(contract_name))
    }
}
