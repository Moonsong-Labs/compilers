use crate::artifacts::error::{Severity, SourceLocation};

use serde::{Deserialize, Serialize};

/// The `solc --standard-json` output error.
///
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Error {
    /// The component type.
    pub component: String,
    /// The error code.
    pub error_code: Option<String>,
    /// The formatted error message.
    pub formatted_message: String,
    /// The non-formatted error message.
    pub message: String,
    /// The error severity.
    pub severity: Severity,
    /// The error location data.
    pub source_location: Option<SourceLocation>,
    /// The error type.
    pub r#type: String,
}
