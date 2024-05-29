//! Support for compiling contracts.

use crate::{
    artifacts::Sources,
    cache::{CacheEntry, CompilerCache, GroupedSources},
    error::Result,
    filter::{FilteredSources, SourceCompilationKind},
    resolver::{parse::SolData, GraphEdges},
    utils,
    zksync::{
        artifact_output::{zk::ZkContractArtifact, OutputContext},
        config::ZkSolcConfig,
    },
    ArtifactOutput, Artifacts, Compiler, Graph, Project, Source,
};
use semver::Version;
use std::{
    collections::{btree_map::BTreeMap, btree_set::BTreeSet, hash_map, HashMap, HashSet},
    path::{Path, PathBuf},
};

/// ethers-rs format version
///
/// `ethers-solc` uses a different format version id, but the actual format is consistent with
/// hardhat This allows ethers-solc to detect if the cache file was written by hardhat or
/// `ethers-solc`
const ETHERS_FORMAT_VERSION: &str = "ethers-rs-sol-cache-3";

/// The file name of the default cache file
pub const ZKSYNC_SOLIDITY_FILES_CACHE_FILENAME: &str = "zksync-solidity-files-cache.json";

/// A helper abstraction over the [`SolFilesCache`] used to determine what files need to compiled
/// and which `Artifacts` can be reused.
#[derive(Debug)]
pub(crate) struct ArtifactsCacheInner<'a, T: ArtifactOutput, C: Compiler> {
    /// The preexisting cache file.
    pub cache: CompilerCache<ZkSolcConfig>,

    /// All already existing artifacts.
    pub cached_artifacts: Artifacts<ZkContractArtifact>,

    /// Relationship between all the files.
    pub edges: GraphEdges<SolData>,

    /// The project.
    pub project: &'a Project<C, T>,

    /// Files that were invalidated and removed from cache.
    /// Those are not grouped by version and purged completely.
    pub dirty_sources: HashSet<PathBuf>,

    /// Artifact+version pairs which are in scope for each solc version.
    ///
    /// Only those files will be included into cached artifacts list for each version.
    pub sources_in_scope: GroupedSources,

    /// The file hashes.
    pub content_hashes: HashMap<PathBuf, String>,
}

impl<'a, T: ArtifactOutput, C: Compiler> ArtifactsCacheInner<'a, T, C> {
    /// Creates a new cache entry for the file
    fn create_cache_entry(&mut self, file: PathBuf, source: &Source) {
        let imports = self
            .edges
            .imports(&file)
            .into_iter()
            .map(|import| utils::source_name(import, self.project.root()).to_path_buf())
            .collect();

        let entry = CacheEntry {
            last_modification_date: CacheEntry::<C::Settings>::read_last_modification_date(&file)
                .unwrap_or_default(),
            content_hash: source.content_hash(),
            source_name: utils::source_name(&file, self.project.root()).into(),
            compiler_settings: self.project.zksync_zksolc_config.clone(),
            imports,
            version_requirement: self.edges.version_requirement(&file).map(|v| v.to_string()),
            // artifacts remain empty until we received the compiler output
            artifacts: Default::default(),
        };

        self.cache.files.insert(file, entry.clone());
    }

    /// Returns the set of [Source]s that need to be compiled to produce artifacts for requested
    /// input.
    ///
    /// Source file may have one of the two [SourceCompilationKind]s:
    /// 1. [SourceCompilationKind::Complete] - the file has been modified or compiled with different
    ///    settings and its cache is invalidated. For such sources we request full data needed for
    ///    artifact construction.
    /// 2. [SourceCompilationKind::Optimized] - the file is not dirty, but is imported by a dirty
    ///    file and thus will be processed by solc. For such files we don't need full data, so we
    ///    are marking them as clean to optimize output selection later.
    fn filter(&mut self, sources: Sources, version: &Version) -> FilteredSources {
        // sources that should be passed to compiler.
        let mut compile_complete = BTreeSet::new();
        let mut compile_optimized = BTreeSet::new();

        for (file, source) in sources.iter() {
            self.sources_in_scope.insert(file.clone(), version.clone());

            // If we are missing artifact for file, compile it.
            if self.is_missing_artifacts(file, version) {
                compile_complete.insert(file.clone());
            }

            // Ensure that we have a cache entry for all sources.
            if !self.cache.files.contains_key(file) {
                self.create_cache_entry(file.clone(), source);
            }
        }

        // Prepare optimization by collecting sources which are imported by files requiring complete
        // compilation.
        for source in &compile_complete {
            for import in self.edges.imports(source) {
                if !compile_complete.contains(import) {
                    compile_optimized.insert(import.clone());
                }
            }
        }

        let filtered = sources
            .into_iter()
            .filter_map(|(file, source)| {
                if compile_complete.contains(&file) {
                    Some((file, SourceCompilationKind::Complete(source)))
                } else if compile_optimized.contains(&file) {
                    Some((file, SourceCompilationKind::Optimized(source)))
                } else {
                    None
                }
            })
            .collect();

        FilteredSources(filtered)
    }

    /// Returns whether we are missing artifacts for the given file and version.
    fn is_missing_artifacts(&self, file: &Path, version: &Version) -> bool {
        let Some(entry) = self.cache.entry(file) else {
            trace!("missing cache entry");
            return true;
        };

        // only check artifact's existence if the file generated artifacts.
        // e.g. a solidity file consisting only of import statements (like interfaces that
        // re-export) do not create artifacts
        if entry.artifacts.is_empty() {
            trace!("no artifacts");
            return false;
        }

        if !entry.contains_version(version) {
            trace!("missing linked artifacts",);
            return true;
        }

        if entry.artifacts_for_version(version).any(|artifact_path| {
            let missing_artifact = !self.cached_artifacts.has_artifact(artifact_path);
            if missing_artifact {
                trace!("missing artifact \"{}\"", artifact_path.display());
            }
            missing_artifact
        }) {
            return true;
        }

        false
    }

    // Walks over all cache entires, detects dirty files and removes them from cache.
    fn find_and_remove_dirty(&mut self) {
        fn populate_dirty_files<D>(
            file: &Path,
            dirty_files: &mut HashSet<PathBuf>,
            edges: &GraphEdges<D>,
        ) {
            for file in edges.importers(file) {
                // If file is marked as dirty we either have already visited it or it was marked as
                // dirty initially and will be visited at some point later.
                if !dirty_files.contains(file) {
                    dirty_files.insert(file.to_path_buf());
                    populate_dirty_files(file, dirty_files, edges);
                }
            }
        }

        // Iterate over existing cache entries.
        let files = self.cache.files.keys().cloned().collect::<HashSet<_>>();

        let mut sources = BTreeMap::new();

        // Read all sources, marking entries as dirty on I/O errors.
        for file in &files {
            let Ok(source) = Source::read(file) else {
                self.dirty_sources.insert(file.clone());
                continue;
            };
            sources.insert(file.clone(), source);
        }

        // Build a temporary graph for walking imports. We need this because `self.edges`
        // only contains graph data for in-scope sources but we are operating on cache entries.
        if let Ok(graph) = Graph::<C::ParsedSource>::resolve_sources(&self.project.paths, sources) {
            let (sources, edges) = graph.into_sources();

            // Calculate content hashes for later comparison.
            self.fill_hashes(&sources);

            // Pre-add all sources that are guaranteed to be dirty
            for file in sources.keys() {
                if self.is_dirty_impl(file) {
                    self.dirty_sources.insert(file.clone());
                }
            }

            // Perform DFS to find direct/indirect importers of dirty files.
            for file in self.dirty_sources.clone().iter() {
                populate_dirty_files(file, &mut self.dirty_sources, &edges);
            }
        } else {
            // Purge all sources on graph resolution error.
            self.dirty_sources.extend(files);
        }

        // Remove all dirty files from cache.
        for file in &self.dirty_sources {
            debug!("removing dirty file from cache: {}", file.display());
            self.cache.remove(file);
        }
    }

    fn is_dirty_impl(&self, file: &Path) -> bool {
        let Some(hash) = self.content_hashes.get(file) else {
            trace!("missing content hash");
            return true;
        };

        let Some(entry) = self.cache.entry(file) else {
            trace!("missing cache entry");
            return true;
        };

        if entry.content_hash != *hash {
            trace!("content hash changed");
            return true;
        }

        if !self.project.settings.can_use_cached(&entry.compiler_settings) {
            trace!("solc config not compatible");
            return true;
        }

        // If any requested extra files are missing for any artifact, mark source as dirty to
        // generate them
        for artifacts in self.cached_artifacts.values() {
            for artifacts in artifacts.values() {
                for artifact_file in artifacts {
                    if self.project.artifacts_handler().is_dirty(artifact_file).unwrap_or(true) {
                        return true;
                    }
                }
            }
        }

        // all things match, can be reused
        false
    }

    /// Adds the file's hashes to the set if not set yet
    fn fill_hashes(&mut self, sources: &Sources) {
        for (file, source) in sources {
            if let hash_map::Entry::Vacant(entry) = self.content_hashes.entry(file.clone()) {
                entry.insert(source.content_hash());
            }
        }
    }
}

/// Abstraction over configured caching which can be either non-existent or an already loaded cache
#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub(crate) enum ArtifactsCache<'a, T: ArtifactOutput, C: Compiler> {
    /// Cache nothing on disk
    Ephemeral(GraphEdges<SolData>, &'a Project<C, T>),
    /// Handles the actual cached artifacts, detects artifacts that can be reused
    Cached(ArtifactsCacheInner<'a, T, C>),
}

/// Override of CompilerCache::read_joined to use zksync paths
pub fn zksync_override_compiler_cache_read_joined(paths: &ProjectPathsConfig<C>) -> Result<Self> {
    let mut cache = CompilerCache::read(&paths.zksync_cache)?;
    cache.join_entries(&paths.root).join_artifacts_files(&paths.zksync_artifacts);
    Ok(cache)
}

impl<'a, T: ArtifactOutput, C: Compiler> ArtifactsCache<'a, T, C> {
    pub fn new(project: &'a Project<C, T>, edges: GraphEdges<SolData>) -> Result<Self> {
        /// Returns the [SolFilesCache] to use
        ///
        /// Returns a new empty cache if the cache does not exist or `invalidate_cache` is set.
        fn get_cache<T: ArtifactOutput, C: Compiler>(
            project: &Project<C, T>,
            invalidate_cache: bool,
        ) -> CompilerCache<ZkSolcConfig> {
            // the currently configured paths
            let paths = project.paths.zksync_paths_relative();

            if !invalidate_cache && project.zksync_cache_path().exists() {
                if let Ok(cache) = CompilerCache::<ZkSolcConfig>::read_joined(&project.paths) {
                    if cache.paths == paths {
                        // unchanged project paths
                        return cache;
                    }
                }
            }

            // new empty cache
            CompilerCache::<ZkSolcConfig>::new(Default::default(), paths)
        }

        let cache = if project.cached {
            // we only read the existing cache if we were able to resolve the entire graph
            // if we failed to resolve an import we invalidate the cache so don't get any false
            // positives
            let invalidate_cache = !edges.unresolved_imports().is_empty();

            // read the cache file if it already exists
            let mut cache = get_cache(project, invalidate_cache);

            cache.remove_missing_files();

            // read all artifacts
            let cached_artifacts = if project.paths.artifacts.exists() {
                trace!("reading artifacts from cache...");
                // if we failed to read the whole set of artifacts we use an empty set
                let artifacts = cache.read_artifacts().unwrap_or_default();
                trace!("read {} artifacts from cache", artifacts.artifact_files().count());
                artifacts
            } else {
                Default::default()
            };

            let cache = ArtifactsCacheInner {
                cache,
                cached_artifacts,
                edges,
                project,
                filtered: Default::default(),
                dirty_source_files: Default::default(),
                content_hashes: Default::default(),
            };

            ArtifactsCache::Cached(cache)
        } else {
            // nothing to cache
            ArtifactsCache::Ephemeral(edges, project)
        };

        Ok(cache)
    }

    /// Returns the graph data for this project
    pub fn graph(&self) -> &GraphEdges<SolData> {
        match self {
            ArtifactsCache::Ephemeral(graph, _) => graph,
            ArtifactsCache::Cached(inner) => &inner.edges,
        }
    }

    #[cfg(test)]
    #[allow(unused)]
    #[doc(hidden)]
    // only useful for debugging for debugging purposes
    pub fn as_cached(&self) -> Option<&ArtifactsCacheInner<'a, T, C>> {
        match self {
            ArtifactsCache::Ephemeral(_, _) => None,
            ArtifactsCache::Cached(cached) => Some(cached),
        }
    }

    pub fn output_ctx(&self) -> OutputContext<'_> {
        match self {
            ArtifactsCache::Ephemeral(_, _) => Default::default(),
            ArtifactsCache::Cached(inner) => OutputContext::new(&inner.cache),
        }
    }

    pub fn project(&self) -> &'a Project<C, T> {
        match self {
            ArtifactsCache::Ephemeral(_, project) => project,
            ArtifactsCache::Cached(cache) => cache.project,
        }
    }

    /// Adds the file's hashes to the set if not set yet
    pub fn fill_content_hashes(&mut self, sources: &Sources) {
        match self {
            ArtifactsCache::Ephemeral(_, _) => {}
            ArtifactsCache::Cached(cache) => cache.fill_hashes(sources),
        }
    }

    /// Filters out those sources that don't need to be compiled
    pub fn filter(&mut self, sources: Sources, version: &Version) -> FilteredSources {
        match self {
            ArtifactsCache::Ephemeral(_, _) => sources.into(),
            ArtifactsCache::Cached(cache) => cache.filter(sources, version),
        }
    }

    /// Consumes the `Cache`, rebuilds the `SolFileCache` by merging all artifacts that were
    /// filtered out in the previous step (`Cache::filtered`) and the artifacts that were just
    /// compiled and written to disk `written_artifacts`.
    ///
    /// Returns all the _cached_ artifacts.
    pub fn consume(
        self,
        written_artifacts: &Artifacts<ZkContractArtifact>,
        write_to_disk: bool,
    ) -> Result<Artifacts<ZkContractArtifact>> {
        let ArtifactsCache::Cached(cache) = self else {
            trace!("no cache configured, ephemeral");
            return Ok(Default::default());
        };

        let ArtifactsCacheInner {
            mut cache,
            mut cached_artifacts,
            mut dirty_source_files,
            filtered,
            project,
            ..
        } = cache;

        // keep only those files that were previously filtered (not dirty, reused)
        cache.retain(filtered.iter().map(|(p, (_, v))| (p.as_path(), v)));

        // add the written artifacts to the cache entries, this way we can keep a mapping
        // from solidity file to its artifacts
        // this step is necessary because the concrete artifacts are only known after solc
        // was invoked and received as output, before that we merely know the file and
        // the versions, so we add the artifacts on a file by file basis
        for (file, written_artifacts) in written_artifacts.as_ref() {
            let file_path = Path::new(file);
            if let Some((cache_entry, versions)) = dirty_source_files.get_mut(file_path) {
                cache_entry.insert_artifacts(written_artifacts.iter().map(|(name, artifacts)| {
                    let artifacts = artifacts
                        .iter()
                        .filter(|artifact| versions.contains(&artifact.version))
                        .collect::<Vec<_>>();
                    (name, artifacts)
                }));
            }

            // cached artifacts that were overwritten also need to be removed from the
            // `cached_artifacts` set
            if let Some((f, mut cached)) = cached_artifacts.0.remove_entry(file) {
                trace!(file, "checking for obsolete cached artifact entries");
                cached.retain(|name, cached_artifacts| {
                    let Some(written_files) = written_artifacts.get(name) else {
                        return false;
                    };

                    // written artifact clashes with a cached artifact, so we need to decide whether
                    // to keep or to remove the cached
                    cached_artifacts.retain(|f| {
                        // we only keep those artifacts that don't conflict with written artifacts
                        // and which version was a compiler target
                        let same_version =
                            written_files.iter().all(|other| other.version != f.version);
                        let is_filtered = filtered
                            .get(file_path)
                            .map(|(_, versions)| versions.contains(&f.version))
                            .unwrap_or_default();
                        let retain = same_version && is_filtered;
                        if !retain {
                            trace!(
                                artifact=%f.file.display(),
                                contract=%name,
                                version=%f.version,
                                "purging obsolete cached artifact",
                            );
                        }
                        retain
                    });

                    !cached_artifacts.is_empty()
                });

                if !cached.is_empty() {
                    cached_artifacts.0.insert(f, cached);
                }
            }
        }

        // add the new cache entries to the cache file
        cache.extend(dirty_source_files.into_iter().map(|(file, (entry, _))| (file, entry)));

        // write to disk
        if write_to_disk {
            // make all `CacheEntry` paths relative to the project root and all artifact
            // paths relative to the artifact's directory
            cache
                .strip_entries_prefix(project.root())
                .strip_artifact_files_prefixes(project.zksync_artifacts_path());
            cache.write(project.zksync_cache_path())?;
        }

        Ok(cached_artifacts)
    }
}
