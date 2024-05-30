use super::settings::ZkSolcSettings;
use crate::{
    artifacts::{Sources, SOLIDITY, YUL},
    compilers::CompilerInput,
    remappings::Remapping,
};
use semver::Version;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, path::Path};

/// Input type `solc` expects.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ZkSolcInput {
    pub language: String,
    pub sources: Sources,
    pub settings: ZkSolcSettings,
}

/// Default `language` field is set to `"Solidity"`.
impl Default for ZkSolcInput {
    fn default() -> Self {
        ZkSolcInput {
            language: SOLIDITY.to_string(),
            sources: Sources::default(),
            settings: ZkSolcSettings::default(),
        }
    }
}

impl ZkSolcInput {
    /// Removes the `base` path from all source files
    pub fn strip_prefix(&mut self, base: impl AsRef<Path>) {
        let base = base.as_ref();
        self.sources = std::mem::take(&mut self.sources)
            .into_iter()
            .map(|(path, s)| (path.strip_prefix(base).map(Into::into).unwrap_or(path), s))
            .collect();

        self.settings.strip_prefix(base);
    }
    /// The flag indicating whether the current [CompilerInput] is
    /// constructed for the yul sources
    pub fn is_yul(&self) -> bool {
        self.language == YUL
    }
}

impl CompilerInput for ZkSolcInput {
    type Settings = ZkSolcSettings;

    /// Creates a new [CompilerInput]s with default settings and the given sources
    ///
    /// A [CompilerInput] expects a language setting, supported by solc are solidity or yul.
    /// In case the `sources` is a mix of solidity and yul files, 2 CompilerInputs are returned
    // WARN: version here is the solc version not the zksolc one
    fn build(sources: Sources, mut settings: Self::Settings, version: &Version) -> Vec<Self> {
        //settings.sanitize(version);
        if let Some(ref mut evm_version) = settings.evm_version {
            settings.evm_version = evm_version.normalize_version_solc(version);
        }

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
                settings: settings.clone(),
            });
        }
        if !yul_sources.is_empty() {
            if !settings.remappings.is_empty() {
                warn!("omitting remappings supplied for the yul sources");
                settings.remappings = vec![];
            }

            res.push(Self { language: YUL.to_string(), sources: yul_sources, settings });
        }
        res
    }

    fn sources(&self) -> &Sources {
        &self.sources
    }

    fn with_remappings(mut self, remappings: Vec<Remapping>) -> Self {
        if self.language == YUL {
            if !remappings.is_empty() {
                warn!("omitting remappings supplied for the yul sources");
            }
        } else {
            self.settings.remappings = remappings;
        }
        self
    }

    fn compiler_name(&self) -> String {
        "Solc".to_string()
    }

    fn strip_prefix(&mut self, base: &Path) {
        self.strip_prefix(base)
    }
}
