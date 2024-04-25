use serde::{Deserialize, Serialize};
use std::str::FromStr;

/// The `solc --standard-json` output error.
///
#[derive(Debug, Serialize, Deserialize, Clone)]
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
    pub severity: String,
    /// The error location data.
    pub source_location: Option<SourceLocation>,
    /// The error type.
    pub r#type: String,
}

///
/// The `solc --standard-json` output error source location.
///
#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct SourceLocation {
    /// The source file path.
    pub file: String,
    /// The start location.
    pub start: isize,
    /// The end location.
    pub end: isize,
}

impl FromStr for SourceLocation {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let mut parts = string.split(':');
        let start = parts
            .next()
            .map(|string| string.parse::<isize>())
            .and_then(Result::ok)
            .unwrap_or_default();
        let length = parts
            .next()
            .map(|string| string.parse::<isize>())
            .and_then(Result::ok)
            .unwrap_or_default();
        let file = parts.next().unwrap_or_default().to_owned();

        Ok(Self { file, start, end: start + length })
    }
}
