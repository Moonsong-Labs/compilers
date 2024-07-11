use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fs::{self},
    io,
    path::{Path, PathBuf, MAIN_SEPARATOR},
    str::FromStr,
};

use foundry_compilers::{
    buildinfo::BuildInfo,
    cache::{CompilerCache, SOLIDITY_FILES_CACHE_FILENAME},
    compilers::{
        solc::{Solc, SolcCompiler, SolcLanguage},
        vyper::{Vyper, VyperLanguage, VyperSettings},
        CompilerOutput,
    },
    flatten::Flattener,
    info::ContractInfo,
    project_util::*,
    take_solc_installer_lock, Artifact, ConfigurableArtifacts, ExtraOutputValues, Graph, Project,
    ProjectBuilder, ProjectCompileOutput, ProjectPathsConfig, TestFileFilter,
};

#[test]
fn can_compile_dapp_sample() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../test-data/dapp-sample");
    let paths = ProjectPathsConfig::builder().sources(root.join("src")).lib(root.join("lib"));
    let project = TempProject::<SolcCompiler, ConfigurableArtifacts>::new(paths).unwrap();

    let compiled = project.compile().unwrap();
    assert!(compiled.find_first("Dapp").is_some());
    compiled.assert_success();

    // nothing to compile
    let compiled = project.compile().unwrap();
    assert!(compiled.find_first("Dapp").is_some());
    assert!(compiled.is_unchanged());

    let cache = CompilerCache::<Settings>::read(project.cache_path()).unwrap();

    // delete artifacts
    std::fs::remove_dir_all(&project.paths().artifacts).unwrap();
    let compiled = project.compile().unwrap();
    assert!(compiled.find_first("Dapp").is_some());
    assert!(!compiled.is_unchanged());

    let updated_cache = CompilerCache::<Settings>::read(project.cache_path()).unwrap();
    assert_eq!(cache, updated_cache);
}
