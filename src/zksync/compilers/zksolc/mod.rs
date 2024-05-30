use self::input::ZkSolcInput;
use crate::{
    compilers::CompilerInput,
    error::{Result, SolcError},
    zksync::artifacts::CompilerOutput,
    Solc, Source,
};

use itertools::Itertools;
use semver::Version;
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
    process::{Command, Output, Stdio},
    str::FromStr,
};

pub mod input;
pub mod settings;
pub use settings::ZkSolcSettings;

#[cfg(feature = "async")]
use std::os::unix::prelude::PermissionsExt;
#[cfg(feature = "async")]
use tokio::{
    fs::{create_dir_all, set_permissions, File},
    io::copy,
};

pub const ZKSOLC: &str = "zksolc";

#[derive(Debug, Clone, Serialize)]
enum ZkSolcOS {
    Linux,
    MacAMD,
    MacARM,
}

fn get_operating_system() -> Result<ZkSolcOS> {
    match std::env::consts::OS {
        "linux" => Ok(ZkSolcOS::Linux),
        "macos" | "darwin" => match std::env::consts::ARCH {
            "aarch64" => Ok(ZkSolcOS::MacARM),
            _ => Ok(ZkSolcOS::MacAMD),
        },
        _ => Err(SolcError::msg(format!("Unsupported operating system {}", std::env::consts::OS))),
    }
}

impl ZkSolcOS {
    fn get_compiler(&self) -> &str {
        match self {
            ZkSolcOS::Linux => "zksolc-linux-amd64-musl-",
            ZkSolcOS::MacAMD => "zksolc-macosx-amd64-",
            ZkSolcOS::MacARM => "zksolc-macosx-arm64-",
        }
    }

    #[cfg(feature = "async")]
    fn get_download_uri(&self) -> &str {
        match self {
            ZkSolcOS::Linux => "linux-amd64-musl",
            ZkSolcOS::MacAMD => "macosx-amd64",
            ZkSolcOS::MacARM => "macosx-arm64",
        }
    }
}

/// Abstraction over `zksolc` command line utility
///
/// Supports sync and async functions.
///
/// By default the zksolc path is configured as follows, with descending priority:
///   1. `ZKSOLC_PATH` environment variable
///   2. `zksolc` otherwise
#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ZkSolc {
    /// Path to the `zksolc` executable
    pub zksolc: PathBuf,
    /// Value for --base path
    pub base_path: Option<PathBuf>,
    /// Value for --allow-paths arg.
    pub allow_paths: BTreeSet<PathBuf>,
    /// Value for --include-paths arg.
    pub include_paths: BTreeSet<PathBuf>,
    /// Value for --solc arg
    pub solc: Option<PathBuf>,
}

impl Default for ZkSolc {
    fn default() -> Self {
        if let Ok(zksolc) = std::env::var("ZKSOLC_PATH") {
            return ZkSolc::new(zksolc);
        }

        ZkSolc::new(ZKSOLC)
    }
}

impl ZkSolc {
    /// A new instance which points to `zksolc`
    pub fn new(path: impl Into<PathBuf>) -> Self {
        ZkSolc {
            zksolc: path.into(),
            base_path: None,
            allow_paths: Default::default(),
            include_paths: Default::default(),
            solc: None,
        }
    }

    /// Associate a template ZkSolc instance with a Solc compiler instance,
    /// creating a new instance that inherits all the config and compiles
    /// using the specific solc path.
    pub fn from_template_and_solc(template: &Self, solc: Solc) -> Result<Self> {
        let mut zksolc = template.clone();

        // TODO: we override args and base_path with the values in solc as we
        // asume they will come with what we want from the
        // `Project::configure_solc_with_version call`. This might not be the case
        // so we need to double check at some point.
        zksolc.base_path = solc.base_path;
        zksolc.allow_paths = solc.allow_paths;
        zksolc.include_paths = solc.include_paths;
        zksolc.solc = Some(solc.solc);

        Ok(zksolc)
    }

    /// Sets zksolc's base path
    pub fn with_base_path(mut self, base_path: impl Into<PathBuf>) -> Self {
        self.base_path = Some(base_path.into());
        self
    }

    /// Convenience function for compiling all sources under the given path
    pub fn compile_source(&self, path: impl AsRef<Path>) -> Result<CompilerOutput> {
        let path = path.as_ref();
        let mut res: CompilerOutput = Default::default();
        for input in ZkSolcInput::build(
            Source::read_sol_yul_from(path)?,
            Default::default(),
            &self.version()?,
        ) {
            let (output, _) = self.compile(&input)?;
            res.merge(output)
        }
        Ok(res)
    }

    /// Compiles with `--standard-json` and deserializes the output as [`CompilerOutput`].
    pub fn compile(&self, input: &ZkSolcInput) -> Result<(CompilerOutput, bool)> {
        let (output, recompiled_with_dml) = self.compile_output(input)?;

        // Only run UTF-8 validation once.
        let output = std::str::from_utf8(&output).map_err(|_| SolcError::InvalidUtf8)?;

        Ok((serde_json::from_str(output)?, recompiled_with_dml))
    }

    /// Compiles with `--standard-json` and returns the raw `stdout` output.
    #[instrument(name = "compile", level = "debug", skip_all)]
    pub fn compile_output(&self, input: &ZkSolcInput) -> Result<(Vec<u8>, bool)> {
        let mut cmd = Command::new(&self.zksolc);
        let mut recompiled_with_dml = false;
        if let Some(base_path) = &self.base_path {
            cmd.current_dir(base_path);
            cmd.arg("--base-path").arg(base_path);
        }

        if !self.allow_paths.is_empty() {
            cmd.arg("--allow-paths");
            cmd.arg(self.allow_paths.iter().map(|p| p.display()).join(","));
        }

        if let Some(base_path) = &self.base_path {
            for path in self.include_paths.iter().filter(|p| p.as_path() != base_path.as_path()) {
                cmd.arg("--include-path").arg(path);
            }

            cmd.arg("--base-path").arg(base_path);

            cmd.current_dir(base_path);
        }

        if let Some(solc) = &self.solc {
            cmd.arg("--solc");
            cmd.arg(solc);
        }

        if input.settings.system_mode {
            cmd.arg("--system-mode");
        }

        if input.settings.force_evmla {
            cmd.arg("--force-evmla");
        }

        if input.settings.detect_missing_libraries {
            cmd.arg("--detect-missing-libraries");
        }

        cmd.arg("--standard-json");
        cmd.stdin(Stdio::piped()).stderr(Stdio::piped()).stdout(Stdio::piped());

        trace!(input=%serde_json::to_string(input).unwrap_or_else(|e| e.to_string()));
        debug!(?cmd, "compiling");

        let mut child = cmd.spawn().map_err(self.map_io_err())?;
        debug!("spawned");

        let stdin = child.stdin.as_mut().unwrap();
        serde_json::to_writer(stdin, input)?;
        debug!("wrote JSON input to stdin");

        let output = child.wait_with_output().map_err(self.map_io_err())?;
        debug!(%output.status, output.stderr = ?String::from_utf8_lossy(&output.stderr), "finished");

        let missing_libs_error: &[u8] = b"not found in the project".as_slice();

        let output = if !output.status.success()
            && output
                .stderr
                .windows(missing_libs_error.len())
                .any(|window| window == missing_libs_error)
        {
            trace!("Re-Running compiler with missing libraries detection");
            recompiled_with_dml = true;
            cmd.arg("--detect-missing-libraries");
            let mut child = cmd.spawn().map_err(self.map_io_err())?;
            debug!("spawned");
            let stdin = child.stdin.as_mut().unwrap();
            serde_json::to_writer(stdin, input)?;
            debug!("wrote JSON input to stdin");

            debug!(%output.status, output.stderr = ?String::from_utf8_lossy(&output.stderr), "finished");
            child.wait_with_output().map_err(self.map_io_err())?
        } else {
            output
        };

        compile_output(output, recompiled_with_dml)
    }

    /// Invokes `zksolc --version` and parses the output as a SemVer [`Version`], stripping the
    /// pre-release and build metadata.
    pub fn version_short(&self) -> Result<Version> {
        let version = self.version()?;
        Ok(Version::new(version.major, version.minor, version.patch))
    }

    /// Invokes `zksolc --version` and parses the output as a SemVer [`Version`].
    #[instrument(level = "debug", skip_all)]
    pub fn version(&self) -> Result<Version> {
        let mut cmd = Command::new(&self.zksolc);
        cmd.arg("--version").stdin(Stdio::piped()).stderr(Stdio::piped()).stdout(Stdio::piped());
        debug!(?cmd, "getting ZkSolc version");
        let output = cmd.output().map_err(self.map_io_err())?;
        trace!(?output);
        let version = version_from_output(output)?;
        debug!(%version);
        Ok(version)
    }

    fn map_io_err(&self) -> impl FnOnce(std::io::Error) -> SolcError + '_ {
        move |err| SolcError::io(err, &self.zksolc)
    }

    fn compilers_dir() -> Result<PathBuf> {
        let mut compilers_dir = dirs::home_dir()
            .ok_or(SolcError::msg("Could not build SolcManager - homedir not found"))?;
        compilers_dir.push(".zksync");
        Ok(compilers_dir)
    }

    fn compiler_path(version: &Version) -> Result<PathBuf> {
        let os = get_operating_system()?;
        Ok(Self::compilers_dir()?.join(format!("{}v{}", os.get_compiler(), version)))
    }

    /// Install zksolc version and block the thread
    // TODO: Maybe this (and the whole module) goes behind a zksync feature installed
    #[cfg(feature = "async")]
    pub fn blocking_install(version: &Version) -> Result<Self> {
        use crate::utils::RuntimeOrHandle;

        trace!("blocking installing solc version \"{}\"", version);
        // TODO: Evaluate report support
        //crate::report::solc_installation_start(version);
        // An async block is used because the underlying `reqwest::blocking::Client` does not behave well
        // inside of a Tokio runtime. See: https://github.com/seanmonstar/reqwest/issues/1017
        let install = RuntimeOrHandle::new().block_on(async {
            let os = get_operating_system()?;
            let download_uri = os.get_download_uri();
            let full_download_url = format!(
                "https://github.com/matter-labs/zksolc-bin/releases/download/v{}/zksolc-{}-v{}",
                version, download_uri, version
            );

            let compiler_path = Self::compiler_path(version)?;

            let client = reqwest::Client::new();
            let response = client
                .get(full_download_url)
                .send()
                .await
                .map_err(|e| SolcError::msg(format!("Failed to download file: {}", e)))?;

            if response.status().is_success() {
                let compilers_dir = Self::compilers_dir()?;
                if !compilers_dir.exists() {
                    create_dir_all(compilers_dir).await.map_err(|e| {
                        SolcError::msg(format!("Could not create compilers path: {}", e))
                    })?;
                }
                let mut output_file = File::create(&compiler_path)
                    .await
                    .map_err(|e| SolcError::msg(format!("Failed to create output file: {}", e)))?;

                let content = response
                    .bytes()
                    .await
                    .map_err(|e| SolcError::msg(format!("failed to download file: {}", e)))?;

                copy(&mut content.as_ref(), &mut output_file).await.map_err(|e| {
                    SolcError::msg(format!("Failed to write the downloaded file: {}", e))
                })?;

                set_permissions(&compiler_path, PermissionsExt::from_mode(0o755)).await.map_err(
                    |e| SolcError::msg(format!("Failed to set zksync compiler permissions: {e}")),
                )?;
            } else {
                return Err(SolcError::msg(format!(
                    "Failed to download file: status code {}",
                    response.status()
                )));
            }
            Ok(compiler_path)
        });

        match install {
            Ok(path) => {
                //crate::report::solc_installation_success(version);
                Ok(ZkSolc::new(path))
            }
            Err(err) => {
                //crate::report::solc_installation_error(version, &err.to_string());
                Err(err)
            }
        }
    }

    pub fn find_installed_version(version: &Version) -> Result<Option<Self>> {
        let zksolc = Self::compiler_path(version)?;

        if !zksolc.is_file() {
            return Ok(None);
        }
        Ok(Some(ZkSolc::new(zksolc)))
    }
}

fn compile_output(output: Output, recompiled_with_dml: bool) -> Result<(Vec<u8>, bool)> {
    if output.status.success() {
        Ok((output.stdout, recompiled_with_dml))
    } else {
        Err(SolcError::solc_output(&output))
    }
}

fn version_from_output(output: Output) -> Result<Version> {
    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let version = stdout
            .lines()
            .filter(|l| !l.trim().is_empty())
            .last()
            .ok_or_else(|| SolcError::msg("Version not found in zksolc output"))?;
        Ok(Version::from_str(
            version
                .split_whitespace()
                .nth(3)
                .ok_or_else(|| SolcError::msg("Unable to retrieve version from zksolc output"))?
                .trim_start_matches('v'),
        )?)
    } else {
        Err(SolcError::solc_output(&output))
    }
}

impl AsRef<Path> for ZkSolc {
    fn as_ref(&self) -> &Path {
        &self.zksolc
    }
}

impl<T: Into<PathBuf>> From<T> for ZkSolc {
    fn from(zksolc: T) -> Self {
        ZkSolc::new(zksolc.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::zksync::artifact_output::Artifact;

    fn zksolc() -> ZkSolc {
        ZkSolc::default()
    }

    #[test]
    fn zksolc_version_works() {
        zksolc().version().unwrap();
    }

    #[test]
    fn zksolc_compile_works() {
        let input = include_str!("../../../../test-data/zksync/in/compiler-in-1.json");
        let input: ZkSolcInput = serde_json::from_str(input).unwrap();
        let (out, rdml) = zksolc().compile(&input).unwrap();
        assert!(!out.has_error());
        assert!(!rdml);
    }

    #[test]
    fn zksolc_can_compile_with_remapped_links() {
        let input: ZkSolcInput = serde_json::from_str(include_str!(
            "../../../../test-data/zksync/library-remapping-in.json"
        ))
        .unwrap();
        let (out, rdml) = zksolc().compile(&input).unwrap();
        let (_, mut contracts) = out.split();
        let contract = contracts.remove("LinkTest").unwrap();
        let bytecode = &contract.get_bytecode().unwrap().object;
        assert!(!bytecode.is_unlinked());
        assert!(!rdml);
    }

    #[test]
    fn zksolc_can_compile_with_remapped_links_temp_dir() {
        let input: ZkSolcInput = serde_json::from_str(include_str!(
            "../../../../test-data/zksync/library-remapping-in-2.json"
        ))
        .unwrap();
        let (out, rdml) = zksolc().compile(&input).unwrap();
        let (_, mut contracts) = out.split();
        let contract = contracts.remove("LinkTest").unwrap();
        let bytecode = &contract.get_bytecode().unwrap().object;
        assert!(!bytecode.is_unlinked());
        assert!(!rdml);
    }
}
