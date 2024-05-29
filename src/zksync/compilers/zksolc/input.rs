use super::settings::ZkSolcSettings;
use crate::artifacts::{Sources, SOLIDITY, YUL};
use serde::{Deserialize, Serialize};
use std::path::Path;

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
    pub fn strip_prefix(mut self, base: impl AsRef<Path>) -> Self {
        let base = base.as_ref();
        self.sources = self
            .sources
            .into_iter()
            .map(|(path, s)| (path.strip_prefix(base).map(Into::into).unwrap_or(path), s))
            .collect();
        self
    }
    /// The flag indicating whether the current [CompilerInput] is
    /// constructed for the yul sources
    pub fn is_yul(&self) -> bool {
        self.language == YUL
    }
}
