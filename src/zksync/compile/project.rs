use crate::{
    artifact_output::{ArtifactOutput, Artifacts},
    artifacts::{VersionedFilteredSources, VersionedSources},
    compilers::{CompilerInput, CompilerVersionManager},
    config::ProjectPathsConfig,
    error::Result,
    filter::SparseOutputFilter,
    report,
    resolver::{parse::SolData, GraphEdges},
    zksync::{
        artifact_output::zk::ZkContractArtifact,
        cache::ArtifactsCache,
        compile::output::{AggregatedCompilerOutput, ProjectCompileOutput},
        compilers::zksolc::{input::ZkSolcInput, settings::ZkSolcSettings, ZkSolc},
    },
    Compiler, CompilerConfig, Graph, Project, Solc, Source, Sources,
};
use std::{path::PathBuf, time::Instant};

/// NOTE(We need the root ArtifactOutput because of the Project type
/// but we are not using to compile anything zksync related)
#[derive(Debug)]
pub struct ProjectCompiler<'a, T: ArtifactOutput> {
    /// Contains the relationship of the source files and their imports
    edges: GraphEdges<SolData>,
    project: &'a Project<Solc, T>,
    /// how to compile all the sources
    sources: CompilerSources,
    /// How to select zksolc [`crate::zksync::artifacts::CompilerOutput`] for files
    sparse_output: SparseOutputFilter<SolData>,
}

impl<'a, T: ArtifactOutput> ProjectCompiler<'a, T> {
    /// Create a new `ProjectCompiler` to bootstrap the compilation process of the project's
    /// sources.
    pub fn new(project: &'a Project<Solc, T>) -> Result<Self> {
        let sources = match project.zksync_avoid_contracts {
            Some(ref contracts_to_avoid) => Source::read_all(
                project
                    .paths
                    .input_files()
                    .into_iter()
                    .filter(|p| !contracts_to_avoid.iter().any(|c| c.is_match(p))),
            )?,
            None => project.paths.read_input_files()?,
        };
        Self::with_sources(project, sources)
    }

    /// Bootstraps the compilation process by resolving the dependency graph of all sources and the
    /// appropriate `Solc` -> `Sources` set as well as the compile mode to use (parallel,
    /// sequential)
    ///
    /// Multiple (`Solc` -> `Sources`) pairs can be compiled in parallel if the `Project` allows
    /// multiple `jobs`, see [`crate::Project::set_solc_jobs()`].
    pub fn with_sources(project: &'a Project<Solc, T>, sources: Sources) -> Result<Self> {
        match &project.compiler_config {
            CompilerConfig::<Solc>::Specific(compiler) => {
                Self::with_sources_and_solc(project, sources, compiler.clone())
            }
            CompilerConfig::<Solc>::AutoDetect(vm) => {
                Self::with_sources_and_version_manager(project, sources, vm.clone())
            }
        }
    }

    /// Compiles the sources with a pinned `ZkSolc` instance
    pub fn with_sources_and_solc(
        project: &'a Project<Solc, T>,
        sources: Sources,
        solc: Solc,
    ) -> Result<Self> {
        let solc_version = solc.version().clone();
        let (sources, edges) = Graph::resolve_sources(&project.paths, sources)?.into_sources();

        let sources_by_version = vec![(solc, solc_version.clone(), sources)];
        let sources = CompilerSources::Sequential(sources_by_version);

        Ok(Self { edges, project, sources, sparse_output: Default::default() })
    }

    pub fn with_sources_and_version_manager<VM: CompilerVersionManager<Compiler = Solc>>(
        project: &'a Project<Solc, T>,
        sources: Sources,
        version_manager: VM,
    ) -> Result<Self> {
        let graph = Graph::resolve_sources(&project.paths, sources)?;
        let (versions, edges) = graph.into_sources_by_version(project.offline, &version_manager)?;

        let sources_by_version = versions.get(&version_manager)?;

        /* TODO: Evaluate parallel support
        let sources = if project.solc_jobs > 1 && sources_by_version.len() > 1 {
            // if there are multiple different versions, and we can use multiple jobs we can compile
            // them in parallel
            CompilerSources::Parallel(sources_by_version, project.solc_jobs)
        } else {
            CompilerSources::Sequential(sources_by_version)
        };
        */
        let sources = CompilerSources::Sequential(sources_by_version);

        Ok(Self { edges, project, sources, sparse_output: Default::default() })
    }

    pub fn compile(self) -> Result<ProjectCompileOutput> {
        let slash_paths = self.project.slash_paths;

        // drive the compiler statemachine to completion
        let mut output = self.preprocess()?.compile()?.write_artifacts()?.write_cache()?;

        if slash_paths {
            // ensures we always use `/` paths
            output.slash_paths();
        }

        Ok(output)
    }

    /// Does basic preprocessing
    ///   - sets proper source unit names
    ///   - check cache
    fn preprocess(self) -> Result<PreprocessedState<'a, T>> {
        trace!("preprocessing");
        let Self { edges, project, mut sources, sparse_output } = self;

        // convert paths on windows to ensure consistency with the `CompilerOutput` `solc` emits,
        // which is unix style `/`
        sources.slash_paths();

        let mut cache = ArtifactsCache::new(project, edges)?;
        // retain and compile only dirty sources and all their imports
        let sources = sources.filtered(&mut cache);

        Ok(PreprocessedState { sources, cache, sparse_output })
    }
}

/// A series of states that comprise the [`ProjectCompiler::compile()`] state machine
///
/// The main reason is to debug all states individually
#[derive(Debug)]
struct PreprocessedState<'a, T: ArtifactOutput> {
    /// Contains all the sources to compile.
    sources: FilteredCompilerSources,

    /// Cache that holds `CacheEntry` objects if caching is enabled and the project is recompiled
    cache: ArtifactsCache<'a, T>,

    sparse_output: SparseOutputFilter<SolData>,
}

impl<'a, T: ArtifactOutput> PreprocessedState<'a, T> {
    /// advance to the next state by compiling all sources
    fn compile(self) -> Result<CompiledState<'a, T>> {
        trace!("compiling");
        let PreprocessedState { sources, cache, sparse_output } = self;
        let project = cache.project();

        let mut output = sources.compile(
            &project.zksync_zksolc,
            &project.zksync_zksolc_config.settings,
            &project.paths,
            sparse_output,
            cache.graph(),
            project.build_info,
        )?;

        // source paths get stripped before handing them over to solc, so solc never uses absolute
        // paths, instead `--base-path <root dir>` is set. this way any metadata that's derived from
        // data (paths) is relative to the project dir and should be independent of the current OS
        // disk. However internally we still want to keep absolute paths, so we join the
        // contracts again
        output.join_all(cache.project().root());

        Ok(CompiledState { output, cache })
    }
}

/// Represents the state after `solc` was successfully invoked
#[derive(Debug)]
struct CompiledState<'a, T: ArtifactOutput> {
    output: AggregatedCompilerOutput,
    cache: ArtifactsCache<'a, T>,
}

impl<'a, T: ArtifactOutput> CompiledState<'a, T> {
    /// advance to the next state by handling all artifacts
    ///
    /// Writes all output contracts to disk if enabled in the `Project` and if the build was
    /// successful
    #[instrument(skip_all, name = "write-artifacts")]
    fn write_artifacts(self) -> Result<ArtifactsState<'a, T>> {
        let CompiledState { output, cache } = self;

        let project = cache.project();
        let ctx = cache.output_ctx();
        // write all artifacts via the handler but only if the build succeeded and project wasn't
        // configured with `no_artifacts == true`
        let compiled_artifacts = if project.no_artifacts {
            project.zksync_artifacts.output_to_artifacts(
                &output.contracts,
                &output.sources,
                ctx,
                &project.paths,
            )
        } else if output.has_error(
            &project.ignored_error_codes,
            &project.ignored_file_paths,
            &project.compiler_severity_filter,
        ) {
            trace!("skip writing cache file due to solc errors: {:?}", output.errors);
            project.zksync_artifacts.output_to_artifacts(
                &output.contracts,
                &output.sources,
                ctx,
                &project.paths,
            )
        } else {
            trace!(
                "handling artifact output for {} contracts and {} sources",
                output.contracts.len(),
                output.sources.len()
            );
            // this emits the artifacts via the project's artifacts handler
            project.zksync_artifacts.on_output(
                &output.contracts,
                &output.sources,
                &project.paths,
                ctx,
            )?

            // TODO: evaluate build info support
            // emits all the build infos, if they exist
            //output.write_build_infos(project.build_info_path())?;
            //artifacts
        };

        Ok(ArtifactsState { output, cache, compiled_artifacts })
    }
}

/// Represents the state after all artifacts were written to disk
#[derive(Debug)]
struct ArtifactsState<'a, T: ArtifactOutput> {
    output: AggregatedCompilerOutput,
    cache: ArtifactsCache<'a, T>,
    compiled_artifacts: Artifacts<ZkContractArtifact>,
}

impl<'a, T: ArtifactOutput> ArtifactsState<'a, T> {
    /// Writes the cache file
    ///
    /// this concludes the [`Project::compile()`] statemachine
    fn write_cache(self) -> Result<ProjectCompileOutput> {
        let ArtifactsState { output, cache, compiled_artifacts } = self;
        let project = cache.project();
        let ignored_error_codes = project.ignored_error_codes.clone();
        let ignored_file_paths = project.ignored_file_paths.clone();
        let compiler_severity_filter = project.compiler_severity_filter;
        let has_error =
            output.has_error(&ignored_error_codes, &ignored_file_paths, &compiler_severity_filter);
        // TODO: We do not write cache that was recompiled with --detect-missing-libraries as
        // settings won't match the project's zksolc settings. Ideally we would update the
        // corresponding cache entries adding that setting
        let skip_write_to_disk =
            project.no_artifacts || has_error || output.recompiled_with_detect_missing_libraries;
        trace!(has_error, project.no_artifacts, skip_write_to_disk, cache_path=?project.cache_path(),"prepare writing cache file");

        let cached_artifacts = cache.consume(&compiled_artifacts, !skip_write_to_disk)?;
        Ok(ProjectCompileOutput {
            compiler_output: output,
            compiled_artifacts,
            cached_artifacts,
            ignored_error_codes,
            ignored_file_paths,
            compiler_severity_filter,
        })
    }
}

/// Determines how the `solc <-> sources` pairs are executed
#[derive(Debug, Clone)]
enum CompilerSources {
    /// Compile all these sequentially
    Sequential(VersionedSources<Solc>),
}

impl CompilerSources {
    /// Converts all `\\` separators to `/`
    ///
    /// This effectively ensures that `solc` can find imported files like `/src/Cheats.sol` in the
    /// VFS (the `ZkSolcInput` as json) under `src/Cheats.sol`.
    fn slash_paths(&mut self) {
        #[cfg(windows)]
        {
            use path_slash::PathBufExt;

            fn slash_versioned_sources(v: &mut VersionedSources) {
                for (_, (_, sources)) in v {
                    *sources = std::mem::take(sources)
                        .into_iter()
                        .map(|(path, source)| {
                            (PathBuf::from(path.to_slash_lossy().as_ref()), source)
                        })
                        .collect()
                }
            }

            match self {
                CompilerSources::Sequential(v) => slash_versioned_sources(v),
            };
        }
    }

    /// Filters out all sources that don't need to be compiled, see [`ArtifactsCache::filter`]
    fn filtered<T: ArtifactOutput>(
        self,
        cache: &mut ArtifactsCache<'_, T>,
    ) -> FilteredCompilerSources {
        fn filtered_sources<T: ArtifactOutput>(
            sources: VersionedSources<Solc>,
            cache: &mut ArtifactsCache<'_, T>,
        ) -> VersionedFilteredSources<Solc> {
            cache.remove_dirty_sources();

            sources
                .into_iter()
                .map(|(solc, version, sources)| {
                    trace!("Filtering {} sources for {}", sources.len(), version);
                    let sources_to_compile = cache.filter(sources, &version);
                    trace!(
                        "Detected {} sources to compile {:?}",
                        sources_to_compile.dirty().count(),
                        sources_to_compile.dirty_files().collect::<Vec<_>>()
                    );
                    (solc, version, sources_to_compile)
                })
                .collect()
        }

        match self {
            CompilerSources::Sequential(s) => {
                FilteredCompilerSources::Sequential(filtered_sources(s, cache))
            }
        }
    }
}

/// Determines how the `solc <-> sources` pairs are executed
#[derive(Debug, Clone)]
enum FilteredCompilerSources {
    /// Compile all these sequentially
    Sequential(VersionedFilteredSources<Solc>),
}

impl FilteredCompilerSources {
    /// Compiles all the files with `Solc`
    fn compile(
        self,
        zksolc: &ZkSolc,
        settings: &ZkSolcSettings,
        paths: &ProjectPathsConfig,
        sparse_output: SparseOutputFilter<SolData>,
        graph: &GraphEdges<SolData>,
        create_build_info: bool,
    ) -> Result<AggregatedCompilerOutput> {
        match self {
            FilteredCompilerSources::Sequential(input) => compile_sequential(
                input,
                zksolc,
                settings,
                paths,
                sparse_output,
                graph,
                create_build_info,
            ),
        }
    }
}

/// Compiles the input set sequentially and returns an aggregated set of the solc `CompilerOutput`s
fn compile_sequential(
    input: VersionedFilteredSources<Solc>,
    zksolc: &ZkSolc,
    settings: &ZkSolcSettings,
    paths: &ProjectPathsConfig,
    _sparse_output: SparseOutputFilter<SolData>,
    graph: &GraphEdges<SolData>,
    _create_build_info: bool,
) -> Result<AggregatedCompilerOutput> {
    let mut aggregated = AggregatedCompilerOutput::default();
    trace!("compiling {} jobs sequentially", input.len());

    // Include additional paths collected during graph resolution.
    let mut include_paths = paths.include_paths.clone();
    include_paths.extend(graph.include_paths().clone());

    for (solc, version, filtered_sources) in input {
        if filtered_sources.is_empty() {
            // nothing to compile
            trace!("skip zksolc {} {} for empty sources set", zksolc.as_ref().display(), version);
            continue;
        }

        trace!(
            "compiling {} sources with zksolc and solc\"{}\"",
            filtered_sources.len(),
            solc.version
        );

        let solc = solc
            .with_base_path(paths.root.clone())
            .with_allowed_paths(paths.allowed_paths.clone())
            .with_include_paths(include_paths.clone());

        let zksolc_with_solc = ZkSolc::from_template_and_solc(zksolc, solc)?;

        let dirty_files: Vec<PathBuf> = filtered_sources.dirty_files().cloned().collect();

        // depending on the composition of the filtered sources, the output selection can be
        // optimized
        let opt_settings = settings.clone();
        // TODO: Evaluate using sparse output filter for zksolc.
        // Since it seems we don't have file granularity for output selection
        // yet, it might not make sense to implement for now
        //let sources = sparse_output.sparse_sources(filtered_sources, &mut opt_settings, graph);
        let sources: Sources = filtered_sources.into();

        for input in ZkSolcInput::build(sources, opt_settings, &version) {
            let actually_dirty = input
                .sources
                .keys()
                .filter(|f| dirty_files.contains(f))
                .cloned()
                .collect::<Vec<_>>();
            if actually_dirty.is_empty() {
                // nothing to compile for this particular language, all dirty files are in the other
                // language set
                trace!(
                    "skip zksolc {} {} compilation of {} compiler input due to empty source set",
                    zksolc_with_solc.as_ref().display(),
                    version,
                    input.language
                );
                continue;
            }
            trace!(
                "calling zksolc with solc `{}` with {} sources {:?}",
                version,
                input.sources.len(),
                input.sources.keys()
            );
            let mut input = input.with_remappings(paths.remappings.clone());
            input.strip_prefix(paths.root.as_path());
            //.sanitized(&version); TODO: evaluate sanitizing input depending on version

            let zksolc_version = zksolc_with_solc.version()?;
            let start = Instant::now();
            report::compiler_spawn(
                &input.compiler_name(),
                &zksolc_version,
                actually_dirty.as_slice(),
            );
            let (mut output, recompiled_with_missing_libraries) =
                zksolc_with_solc.compile(&input)?;
            if recompiled_with_missing_libraries {
                aggregated.recompiled_with_detect_missing_libraries = true;
            }
            report::compiler_success(&input.compiler_name(), &zksolc_version, &start.elapsed());
            trace!("compiled input, output has error: {}", output.has_error());
            trace!("received compiler output: {:?}", output.contracts.keys());

            // if configured also create the build info
            /* TODO: Evaluate supporting build info
            if create_build_info {
                let build_info = RawBuildInfo::new(&input, &output, &version)?;
                aggregated.build_infos.insert(version.clone(), build_info);
            }
            */
            output.join_all(paths.root.as_path());

            aggregated.extend(version.clone(), output);
        }
    }
    Ok(aggregated)
}
