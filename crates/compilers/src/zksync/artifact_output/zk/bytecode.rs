use std::collections::BTreeMap;

use foundry_compilers_artifacts::{
    zksolc::contract::Contract, BytecodeObject, CompactBytecode, CompactDeployedBytecode, Offsets,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub struct ZkArtifactBytecode {
    pub object: BytecodeObject,

    #[serde(default)]
    pub missing_libraries: Vec<String>,
}

impl ZkArtifactBytecode {
    fn link_references(&self) -> BTreeMap<String, BTreeMap<String, Vec<Offsets>>> {
        Contract::parse_link_references(self.missing_libraries.as_slice())
    }
}

// NOTE: distinction between bytecode and deployed bytecode make no sense of zkEvm, but
// we implement these conversions in order to be able to use the Artifacts trait.
impl From<ZkArtifactBytecode> for CompactBytecode {
    fn from(bcode: ZkArtifactBytecode) -> Self {
        let link_references = bcode.link_references();
        Self { object: bcode.object, source_map: None, link_references }
    }
}

impl From<ZkArtifactBytecode> for CompactDeployedBytecode {
    fn from(bcode: ZkArtifactBytecode) -> Self {
        Self { bytecode: Some(bcode.into()), immutable_references: BTreeMap::default() }
    }
}
