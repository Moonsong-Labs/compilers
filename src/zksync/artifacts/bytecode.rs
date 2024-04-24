use crate::artifacts::bytecode::{serialize_bytecode_without_prefix, BytecodeObject};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Bytecode {
    #[serde(serialize_with = "serialize_bytecode_without_prefix")]
    pub object: BytecodeObject,
}
