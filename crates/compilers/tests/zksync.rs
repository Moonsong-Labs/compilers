use std::path::PathBuf;

use foundry_compilers::{
    cache::CompilerCache,
    project_util::*,
    zksolc::{ZkSolc, ZkSolcSettings},
    zksync::artifact_output::zk::ZkArtifactOutput,
    ProjectPathsConfig,
};

#[test]
fn can_compile_dapp_sample() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../test-data/dapp-sample");
    let paths = ProjectPathsConfig::builder().sources(root.join("src")).lib(root.join("lib"));
    let project = TempProject::<ZkSolc, ZkArtifactOutput>::new(paths).unwrap();

    let compiled = project.compile().unwrap();
    assert!(compiled.find_first("Dapp").is_some());
    compiled.assert_success();

    // nothing to compile
    let compiled = project.compile().unwrap();
    assert!(compiled.find_first("Dapp").is_some());
    assert!(compiled.is_unchanged());

    let cache = CompilerCache::<ZkSolcSettings>::read(project.cache_path()).unwrap();

    // delete artifacts
    std::fs::remove_dir_all(&project.paths().artifacts).unwrap();
    let compiled = project.compile().unwrap();
    assert!(compiled.find_first("Dapp").is_some());
    assert!(!compiled.is_unchanged());

    let updated_cache = CompilerCache::<ZkSolcSettings>::read(project.cache_path()).unwrap();
    assert_eq!(cache, updated_cache);
}
