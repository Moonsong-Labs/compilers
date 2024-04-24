//! Contract related types.
use crate::artifacts::{DevDoc, StorageLayout, UserDoc};
use crate::zksync::artifacts::Evm;
use alloy_json_abi::JsonAbi;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashSet};

/// Represents a compiled solidity contract
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Contract {
    /// The Ethereum Contract Metadata.
    /// See <https://docs.soliditylang.org/en/develop/metadata.html>
    // CHECKED
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
    /// The contract missing libraries.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub missing_libraries: Option<HashSet<String>>,
    /// EVM-related outputs
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub evm: Option<Evm>,
}
