use foundry_compilers_artifacts_solc::error::{Severity, SourceLocation};

use serde::{
    de::{self, Deserializer},
    Deserialize, Serialize,
};
use std::{fmt, ops::Range};
use yansi::{Color, Style};

/// The `solc --standard-json` output error.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Error {
    /// The component type.
    pub component: String,
    /// The error code.
    #[serde(deserialize_with = "deserialize_option_u64_from_string")]
    pub error_code: Option<u64>,
    /// The formatted error message.
    pub formatted_message: Option<String>,
    /// The non-formatted error message.
    pub message: String,
    /// The error severity.
    pub severity: Severity,
    /// The error location data.
    pub source_location: Option<SourceLocation>,
    /// The error type.
    pub r#type: String,
}

fn deserialize_option_u64_from_string<'de, D>(deserializer: D) -> Result<Option<u64>, D::Error>
where
    D: Deserializer<'de>,
{
    let value_str: Option<String> = Option::deserialize(deserializer)?;
    match value_str {
        Some(str) => match str.parse() {
            Ok(val) => Ok(Some(val)),
            Err(_) => Err(de::Error::custom("Failed to parse string as u64")),
        },
        None => Ok(None),
    }
}

impl Error {
    /// Returns `true` if the error is an error.
    pub const fn is_error(&self) -> bool {
        self.severity.is_error()
    }

    /// Returns `true` if the error is a warning.
    pub const fn is_warning(&self) -> bool {
        self.severity.is_warning()
    }

    /// Returns `true` if the error is an info.
    pub const fn is_info(&self) -> bool {
        self.severity.is_info()
    }
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut short_msg = self.message.trim();
        let fmtd_msg = self.formatted_message.as_deref().unwrap_or("");

        if short_msg.is_empty() {
            // if the message is empty, try to extract the first line from the formatted message
            if let Some(first_line) = fmtd_msg.lines().next() {
                // this is something like `ParserError: <short_message>`
                if let Some((_, s)) = first_line.split_once(':') {
                    short_msg = s.trim_start();
                } else {
                    short_msg = first_line;
                }
            }
        }

        // Error (XXXX): Error Message
        styled(f, self.severity.color().bold(), |f| self.fmt_severity(f))?;
        fmt_msg(f, short_msg)?;

        let mut lines = fmtd_msg.lines();

        // skip the first line if it contains the same message as the one we just formatted,
        // unless it also contains a source location, in which case the entire error message is an
        // old style error message, like:
        //     path/to/file:line:column: ErrorType: message
        if lines.clone().next().map_or(false, |l| {
            l.contains(short_msg) && l.bytes().filter(|b| *b == b':').count() < 3
        }) {
            let _ = lines.next();
        }

        // format the main source location
        fmt_source_location(f, &mut lines)?;

        // format remaining lines as secondary locations
        while let Some(line) = lines.next() {
            f.write_str("\n")?;

            if let Some((note, msg)) = line.split_once(':') {
                styled(f, Self::secondary_style(), |f| f.write_str(note))?;
                fmt_msg(f, msg)?;
            } else {
                f.write_str(line)?;
            }

            fmt_source_location(f, &mut lines)?;
        }

        Ok(())
    }
}

impl Error {
    /// The style of the diagnostic severity.
    pub fn error_style(&self) -> Style {
        self.severity.color().bold()
    }

    /// The style of the diagnostic message.
    pub fn message_style() -> Style {
        Color::White.bold()
    }

    /// The style of the secondary source location.
    pub fn secondary_style() -> Style {
        Color::Cyan.bold()
    }

    /// The style of the source location highlight.
    pub fn highlight_style() -> Style {
        Style::new().fg(Color::Yellow)
    }

    /// The style of the diagnostics.
    pub fn diag_style() -> Style {
        Color::Yellow.bold()
    }

    /// The style of the source location frame.
    pub fn frame_style() -> Style {
        Style::new().fg(Color::Blue)
    }

    /// Formats the diagnostic severity:
    ///
    /// ```text
    /// Error (XXXX)
    /// ```
    fn fmt_severity(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.severity.as_str())?;
        if let Some(code) = self.error_code {
            write!(f, " ({code})")?;
        }
        Ok(())
    }
}

/// Formats the diagnostic message.
fn fmt_msg(f: &mut fmt::Formatter<'_>, msg: &str) -> fmt::Result {
    styled(f, Error::message_style(), |f| {
        f.write_str(": ")?;
        f.write_str(msg.trim_start())
    })
}

fn fmt_source_location(f: &mut fmt::Formatter<'_>, lines: &mut std::str::Lines<'_>) -> fmt::Result {
    // --> source
    if let Some(line) = lines.next() {
        f.write_str("\n")?;

        let arrow = "-->";
        if let Some((left, loc)) = line.split_once(arrow) {
            f.write_str(left)?;
            styled(f, Error::frame_style(), |f| f.write_str(arrow))?;
            f.write_str(loc)?;
        } else {
            f.write_str(line)?;
        }
    }

    // get the next 3 lines
    let Some(line1) = lines.next() else {
        return Ok(());
    };
    let Some(line2) = lines.next() else {
        f.write_str("\n")?;
        f.write_str(line1)?;
        return Ok(());
    };
    let Some(line3) = lines.next() else {
        f.write_str("\n")?;
        f.write_str(line1)?;
        f.write_str("\n")?;
        f.write_str(line2)?;
        return Ok(());
    };

    // line 1, just a frame
    fmt_framed_location(f, line1, None)?;

    // line 2, frame and code; highlight the text based on line 3's carets
    let hl_start = line3.find('^');
    let highlight = hl_start.map(|start| {
        let end = if line3.contains("^ (") {
            // highlight the entire line because of "spans across multiple lines" diagnostic
            line2.len()
        } else if let Some(carets) = line3[start..].find(|c: char| c != '^') {
            // highlight the text that the carets point to
            start + carets
        } else {
            // the carets span the entire third line
            line3.len()
        }
        // bound in case carets span longer than the code they point to
        .min(line2.len());
        (start.min(end)..end, Error::highlight_style())
    });
    fmt_framed_location(f, line2, highlight)?;

    // line 3, frame and maybe highlight, this time till the end unconditionally
    let highlight = hl_start.map(|i| (i..line3.len(), Error::diag_style()));
    fmt_framed_location(f, line3, highlight)
}

/// Colors a single Solidity framed source location line. Part of [`fmt_source_location`].
fn fmt_framed_location(
    f: &mut fmt::Formatter<'_>,
    line: &str,
    highlight: Option<(Range<usize>, Style)>,
) -> fmt::Result {
    f.write_str("\n")?;

    if let Some((space_or_line_number, rest)) = line.split_once('|') {
        // if the potential frame is not just whitespace or numbers, don't color it
        if !space_or_line_number.chars().all(|c| c.is_whitespace() || c.is_numeric()) {
            return f.write_str(line);
        }

        styled(f, Error::frame_style(), |f| {
            f.write_str(space_or_line_number)?;
            f.write_str("|")
        })?;

        if let Some((range, style)) = highlight {
            let Range { start, end } = range;
            // Skip highlighting if the range is not valid unicode.
            if !line.is_char_boundary(start) || !line.is_char_boundary(end) {
                f.write_str(rest)
            } else {
                let rest_start = line.len() - rest.len();
                f.write_str(&line[rest_start..start])?;
                styled(f, style, |f| f.write_str(&line[range]))?;
                f.write_str(&line[end..])
            }
        } else {
            f.write_str(rest)
        }
    } else {
        f.write_str(line)
    }
}

/// Calls `fun` in between [`Style::fmt_prefix`] and [`Style::fmt_suffix`].
fn styled<F>(f: &mut fmt::Formatter<'_>, style: Style, fun: F) -> fmt::Result
where
    F: FnOnce(&mut fmt::Formatter<'_>) -> fmt::Result,
{
    let enabled = yansi::is_enabled();
    if enabled {
        style.fmt_prefix(f)?;
    }
    fun(f)?;
    if enabled {
        style.fmt_suffix(f)?;
    }
    Ok(())
}
