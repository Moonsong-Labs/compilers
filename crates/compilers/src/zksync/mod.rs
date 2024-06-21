use std::path::PathBuf;

use crate::{compilers::solc::SolcCompiler, error::Result, ArtifactOutput, Project, Source};

use self::compile::output::ProjectCompileOutput;

pub mod artifact_output;
pub mod cache;
pub mod compile;
pub mod config;

/// Returns the path to the artifacts directory
pub fn project_artifacts_path<T: ArtifactOutput>(project: &Project<SolcCompiler, T>) -> &PathBuf {
    &project.paths.zksync_artifacts
}

/// Returns the path to the cache file
pub fn project_cache_path<T: ArtifactOutput>(project: &Project<SolcCompiler, T>) -> &PathBuf {
    &project.paths.zksync_cache
}

pub fn project_compile(project: &Project<SolcCompiler>) -> Result<ProjectCompileOutput> {
    self::compile::project::ProjectCompiler::new(project)?.compile()
}

pub fn project_compile_files<P, I>(
    project: &Project<SolcCompiler>,
    files: I,
) -> Result<ProjectCompileOutput>
where
    I: IntoIterator<Item = P>,
    P: Into<PathBuf>,
{
    let sources = Source::read_all(files)?;
    self::compile::project::ProjectCompiler::with_sources(project, sources)?.compile()
}
