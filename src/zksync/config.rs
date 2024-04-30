use crate::zksync::artifacts::Settings;
use serde::{Deserialize, Serialize};

/// The config to use when compiling the contracts
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Default)]
pub struct ZkSolcConfig {
    /// How the file was compiled
    pub settings: Settings,
}

impl From<ZkSolcConfig> for Settings {
    fn from(config: ZkSolcConfig) -> Self {
        config.settings
    }
}
