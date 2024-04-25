use crate::zksync::artifacts::{CompilerInput, CompilerOutput};

use serde::{Deserialize, Serialize};
use std::{fmt, path::PathBuf};

pub const ZKSOLC: &str = "zksolc";

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
    /// The base path to set when invoking zksolc
    pub base_path: Option<PathBuf>,
    /// Additional arguments passed to the `zksolc` exectuable
    pub args: Vec<String>,
}

impl Default for ZkSolc {
    fn default() -> Self {
        if let Ok(zksolc) = std::env::var("ZKSOLC_PATH") {
            return ZkSolc::new(zksolc);
        }

        ZkSolc::new(ZKSOLC)
    }
}

impl fmt::Display for ZkSolc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.zksolc.display())?;
        if !self.args.is_empty() {
            write!(f, " {}", self.args.join(" "))?;
        }
        Ok(())
    }
}

impl ZkSolc {
    /// A new instance which points to `zksolc`
    pub fn new(path: impl Into<PathBuf>) -> Self {
        ZkSolc { zksolc: path.into(), base_path: None, args: Vec::new() }
    }

    /// Sets zksolc's base path
    pub fn with_base_path(mut self, base_path: impl Into<PathBuf>) -> Self {
        self.base_path = Some(base_path.into());
        self
    }

    /// Adds an argument to pass to the `zksolc` command.
    #[must_use]
    pub fn arg<T: Into<String>>(mut self, arg: T) -> Self {
        self.args.push(arg.into());
        self
    }

    /// Adds multiple arguments to pass to the `zksolc`.
    #[must_use]
    pub fn args<I, S>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        for arg in args {
            self = self.arg(arg);
        }
        self
    }

    /// Convenience function for compiling all sources under the given path
    pub fn compile_source(&self, path: impl AsRef<Path>) -> Result<CompilerOutput> {
        let path = path.as_ref();
        let mut res: CompilerOutput = Default::default();
        for input in CompilerInput::new(path)? {
            let output = self.compile(&input)?;
            res.merge(output)
        }
        Ok(res)
    }

    /// Same as [`Self::compile()`], but only returns those files which are included in the
    /// `CompilerInput`.
    ///
    /// In other words, this removes those files from the `CompilerOutput` that are __not__ included
    /// in the provided `CompilerInput`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use foundry_compilers::{CompilerInput, ZkSolc};
    ///
    /// let zksolc = ZkSolc::default();
    /// let input = CompilerInput::new("./contracts")?[0].clone();
    /// let output = zksolc.compile_exact(&input)?;
    /// # Ok::<_, Box<dyn std::error::Error>>(())
    /// ```
    pub fn compile_exact(&self, input: &CompilerInput) -> Result<CompilerOutput> {
        let mut out = self.compile(input)?;
        out.retain_files(input.sources.keys().filter_map(|p| p.to_str()));
        Ok(out)
    }

    /// Compiles with `--standard-json` and deserializes the output as [`CompilerOutput`].
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use foundry_compilers::{CompilerInput, ZkSolc};
    ///
    /// let zksolc = ZkSolc::default();
    /// let input = CompilerInput::new("./contracts")?;
    /// let output = zksolc.compile(&input)?;
    /// # Ok::<_, Box<dyn std::error::Error>>(())
    /// ```
    pub fn compile<T: Serialize>(&self, input: &T) -> Result<CompilerOutput> {
        self.compile_as(input)
    }

    /// Compiles with `--standard-json` and deserializes the output as the given `D`.
    pub fn compile_as<T: Serialize, D: DeserializeOwned>(&self, input: &T) -> Result<D> {
        let output = self.compile_output(input)?;

        // Only run UTF-8 validation once.
        let output = std::str::from_utf8(&output).map_err(|_| ZkSolcError::InvalidUtf8)?;

        Ok(serde_json::from_str(output)?)
    }

    /// Compiles with `--standard-json` and returns the raw `stdout` output.
    #[instrument(name = "compile", level = "debug", skip_all)]
    pub fn compile_output<T: Serialize>(&self, input: &T) -> Result<Vec<u8>> {
        let mut cmd = Command::new(&self.zksolc);
        if let Some(base_path) = &self.base_path {
            cmd.current_dir(base_path);
            cmd.arg("--base-path").arg(base_path);
        }
        cmd.args(&self.args).arg("--standard-json");
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

        compile_output(output)
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

    fn map_io_err(&self) -> impl FnOnce(std::io::Error) -> ZkSolcError + '_ {
        move |err| ZkSolcError::io(err, &self.zksolc)
    }
}

#[cfg(feature = "async")]
impl ZkSolc {
    /// Convenience function for compiling all sources under the given path
    pub async fn async_compile_source(&self, path: impl AsRef<Path>) -> Result<CompilerOutput> {
        self.async_compile(&CompilerInput::with_sources(Source::async_read_all_from(path).await?))
            .await
    }

    /// Run `zksolc --stand-json` and return the `zksolc`'s output as
    /// `CompilerOutput`
    pub async fn async_compile<T: Serialize>(&self, input: &T) -> Result<CompilerOutput> {
        self.async_compile_as(input).await
    }

    /// Run `zksolc --stand-json` and return the `zksolc`'s output as the given json
    /// output
    pub async fn async_compile_as<T: Serialize, D: DeserializeOwned>(
        &self,
        input: &T,
    ) -> Result<D> {
        let output = self.async_compile_output(input).await?;
        Ok(serde_json::from_slice(&output)?)
    }

    pub async fn async_compile_output<T: Serialize>(&self, input: &T) -> Result<Vec<u8>> {
        use tokio::io::AsyncWriteExt;
        let content = serde_json::to_vec(input)?;
        let mut cmd = tokio::process::Command::new(&self.zksolc);
        if let Some(ref base_path) = self.base_path {
            cmd.current_dir(base_path);
        }
        let mut child = cmd
            .args(&self.args)
            .arg("--standard-json")
            .stdin(Stdio::piped())
            .stderr(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .map_err(self.map_io_err())?;
        let stdin = child.stdin.as_mut().unwrap();
        stdin.write_all(&content).await.map_err(self.map_io_err())?;
        stdin.flush().await.map_err(self.map_io_err())?;
        compile_output(child.wait_with_output().await.map_err(self.map_io_err())?)
    }

    pub async fn async_version(&self) -> Result<Version> {
        let mut cmd = tokio::process::Command::new(&self.zksolc);
        cmd.arg("--version").stdin(Stdio::piped()).stderr(Stdio::piped()).stdout(Stdio::piped());
        debug!(?cmd, "getting version");
        let output = cmd.output().await.map_err(self.map_io_err())?;
        let version = version_from_output(output)?;
        debug!(%version);
        Ok(version)
    }

    /// Compiles all `CompilerInput`s with their associated `ZkSolc`.
    ///
    /// This will buffer up to `n` `zksolc` processes and then return the `CompilerOutput`s in the
    /// order in which they complete. No more than `n` futures will be buffered at any point in
    /// time, and less than `n` may also be buffered depending on the state of each future.
    ///
    /// # Examples
    ///
    /// Compile 2 `CompilerInput`s at once
    ///
    /// ```no_run
    /// use foundry_compilers::{CompilerInput, ZkSolc};
    ///
    /// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let zksolc1 = ZkSolc::default();
    /// let zksolc2 = ZkSolc::default();
    /// let input1 = CompilerInput::new("contracts")?[0].clone();
    /// let input2 = CompilerInput::new("src")?[0].clone();
    ///
    /// let outputs = ZkSolc::compile_many([(zksolc1, input1), (zksolc2, input2)], 2).await.flattened()?;
    /// # Ok(())
    /// # }
    /// ```
    pub async fn compile_many<I>(jobs: I, n: usize) -> crate::many::CompiledMany
    where
        I: IntoIterator<Item = (ZkSolc, CompilerInput)>,
    {
        use futures_util::stream::StreamExt;

        let outputs =
            futures_util::stream::iter(jobs.into_iter().map(|(zksolc, input)| async {
                (zksolc.async_compile(&input).await, zksolc, input)
            }))
            .buffer_unordered(n)
            .collect::<Vec<_>>()
            .await;

        crate::many::CompiledMany::new(outputs)
    }
}

fn compile_output(output: Output) -> Result<Vec<u8>> {
    if output.status.success() {
        Ok(output.stdout)
    } else {
        Err(ZkSolcError::zksolc_output(&output))
    }
}

fn version_from_output(output: Output) -> Result<Version> {
    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let version = stdout
            .lines()
            .filter(|l| !l.trim().is_empty())
            .last()
            .ok_or_else(|| ZkSolcError::msg("Version not found in zksolc output"))?;
        // NOTE: semver doesn't like `+` in g++ in build metadata which is invalid semver
        Ok(Version::from_str(&version.trim_start_matches("Version: ").replace(".g++", ".gcc"))?)
    } else {
        Err(ZkSolcError::zksolc_output(&output))
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
