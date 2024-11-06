use std::collections::BTreeMap;

use foundry_compilers_artifacts_solc::{
    BytecodeObject, CompactBytecode, CompactDeployedBytecode, Offsets,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub struct Bytecode {
    pub object: BytecodeObject,

    /// This value is not part of the compiler output json
    ///
    /// It will be populated if the contract has any missing library
    #[serde(default)]
    pub missing_libraries: Vec<String>,
}

impl Bytecode {
    fn link_references(&self) -> BTreeMap<String, BTreeMap<String, Vec<Offsets>>> {
        self.missing_libraries
            .iter()
            .map(|file_and_lib| {
                let mut parts = file_and_lib.split(':');
                let filename = parts.next().expect("missing library contract file (<file>:<name>)");
                let contract = parts.next().expect("missing library contract name (<file>:<name>)");
                (filename.to_owned(), contract.to_owned())
            })
            .fold(BTreeMap::default(), |mut acc, (filename, contract)| {
                acc.entry(filename)
                    .or_default()
                    //empty offsets since we can't patch it anyways
                    .insert(contract, vec![]);
                acc
            })
    }

    pub(crate) fn mark_as_unlinked(&mut self) {
        if let BytecodeObject::Bytecode(inner) = &self.object {
            let encoded = alloy_primitives::hex::encode(inner);
            self.object = BytecodeObject::Unlinked(encoded);
        }
    }
}

// NOTE: distinction between bytecode and deployed bytecode make no sense of zkEvm, but
// we implement these conversions in order to be able to use the Artifacts trait.
impl From<Bytecode> for CompactBytecode {
    fn from(bcode: Bytecode) -> Self {
        let link_references = bcode.link_references();
        Self { object: bcode.object, source_map: None, link_references }
    }
}

impl From<Bytecode> for CompactDeployedBytecode {
    fn from(bcode: Bytecode) -> Self {
        Self { bytecode: Some(bcode.into()), immutable_references: BTreeMap::default() }
    }
}
