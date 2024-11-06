//! Contract related types.
use crate::EraVM;
use alloy_json_abi::JsonAbi;
use foundry_compilers_artifacts_solc::{
    CompactContractBytecode, CompactContractBytecodeCow, CompactContractRef, DevDoc, StorageLayout,
    UserDoc,
};
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, collections::BTreeMap};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(transparent)]
pub struct Contract(
    #[serde(deserialize_with = "crate::serde_helpers::maybe_unlinked_contract")] pub RawContract,
);

impl std::ops::Deref for Contract {
    type Target = RawContract;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Contract {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Represents a compiled solidity contract
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct RawContract {
    pub abi: Option<JsonAbi>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub metadata: Option<serde_json::Value>,
    #[serde(default)]
    pub userdoc: UserDoc,
    #[serde(default)]
    pub devdoc: DevDoc,
    /// The contract optimized IR code.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub ir_optimized: Option<String>,
    /// The contract storage layout.
    #[serde(default, skip_serializing_if = "StorageLayout::is_empty")]
    pub storage_layout: StorageLayout,
    /// The contract EraVM bytecode hash.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hash: Option<String>,
    /// The contract factory dependencies.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub factory_dependencies: Option<BTreeMap<String, String>>,
    /// EVM-related outputs
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub eravm: Option<EraVM>,
}

impl RawContract {
    pub fn is_unlinked(&self) -> bool {
        self.hash.is_none()
            || self
                .eravm
                .as_ref()
                .and_then(|eravm| eravm.bytecode.as_ref())
                .map(|bc| !bc.missing_libraries.is_empty())
                .unwrap_or_default()
    }
}

// CompactContract variants
// TODO: for zkEvm, the distinction between bytecode and deployed_bytecode makes little sense,
// and there some fields that the ouptut doesn't provide (e.g: source_map)
// However, we implement these because we get the Artifact trait and can reuse lots of
// the crate's helpers without needing to duplicate everything. Maybe there's a way
// we can get all these without having to add the same bytecode twice on each struct.
// Ideally the Artifacts trait would not be coupled to a specific Contract type
impl<'a> From<&'a Contract> for CompactContractBytecodeCow<'a> {
    fn from(artifact: &'a Contract) -> Self {
        let (bytecode, deployed_bytecode) = if let Some(ref eravm) = artifact.eravm {
            (
                eravm.bytecode.clone().map(Into::into).map(Cow::Owned),
                eravm.bytecode.clone().map(Into::into).map(Cow::Owned),
            )
        } else {
            (None, None)
        };
        CompactContractBytecodeCow {
            abi: artifact.abi.as_ref().map(Cow::Borrowed),
            bytecode,
            deployed_bytecode,
        }
    }
}

impl From<Contract> for CompactContractBytecode {
    fn from(c: Contract) -> Self {
        let c = c.0;
        let bytecode = if let Some(eravm) = c.eravm { eravm.bytecode } else { None };
        Self {
            abi: c.abi.map(Into::into),
            deployed_bytecode: bytecode.clone().map(|b| b.into()),
            bytecode: bytecode.clone().map(|b| b.into()),
        }
    }
}

impl<'a> From<&'a Contract> for CompactContractRef<'a> {
    fn from(c: &'a Contract) -> Self {
        let c = &c.0;
        let (bin, bin_runtime) = if let Some(ref eravm) = c.eravm {
            (eravm.bytecode.as_ref().map(|c| &c.object), eravm.bytecode.as_ref().map(|c| &c.object))
        } else {
            (None, None)
        };

        Self { abi: c.abi.as_ref(), bin, bin_runtime }
    }
}
