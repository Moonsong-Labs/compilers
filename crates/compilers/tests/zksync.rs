use std::collections::BTreeSet;
use std::{collections::HashMap, fs, path::PathBuf, str::FromStr};

use alloy_primitives::Bytes;
use foundry_compilers::ProjectBuilder;
use foundry_compilers::{
    buildinfo::BuildInfo,
    cache::CompilerCache,
    project_util::*,
    resolver::parse::SolData,
    zksolc::{input::ZkSolcInput, ZkSolcCompiler, ZkSolcSettings},
    zksync::{self, artifact_output::zk::ZkArtifactOutput},
    Graph, ProjectPathsConfig,
};
use foundry_compilers_artifacts::Remapping;

#[test]
fn zksync_can_compile_dapp_sample() {
    let _ = tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .try_init()
        .ok();
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../test-data/dapp-sample");
    let paths = ProjectPathsConfig::builder().sources(root.join("src")).lib(root.join("lib"));
    let project = TempProject::<ZkSolcCompiler, ZkArtifactOutput>::new(paths).unwrap();

    let compiled = zksync::project_compile(project.project()).unwrap();
    assert!(compiled.find_first("Dapp").is_some());
    compiled.assert_success();

    // nothing to compile
    let compiled = zksync::project_compile(project.project()).unwrap();
    assert!(compiled.find_first("Dapp").is_some());
    assert!(compiled.is_unchanged());

    let cache = CompilerCache::<ZkSolcSettings>::read(project.cache_path()).unwrap();

    // delete artifacts
    std::fs::remove_dir_all(&project.paths().artifacts).unwrap();
    let compiled = zksync::project_compile(project.project()).unwrap();
    assert!(compiled.find_first("Dapp").is_some());
    assert!(!compiled.is_unchanged());

    let updated_cache = CompilerCache::<ZkSolcSettings>::read(project.cache_path()).unwrap();
    assert_eq!(cache, updated_cache);
}

#[test]
fn zksync_can_compile_dapp_detect_changes_in_libs() {
    let _ = tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .try_init()
        .ok();
    let mut project = TempProject::<ZkSolcCompiler, ZkArtifactOutput>::dapptools().unwrap();

    let remapping = project.paths().libraries[0].join("remapping");
    project
        .paths_mut()
        .remappings
        .push(Remapping::from_str(&format!("remapping/={}/", remapping.display())).unwrap());

    let src = project
        .add_source(
            "Foo",
            r#"
    pragma solidity ^0.8.10;
    import "remapping/Bar.sol";

    contract Foo {}
   "#,
        )
        .unwrap();

    let lib = project
        .add_lib(
            "remapping/Bar",
            r"
    pragma solidity ^0.8.10;

    contract Bar {}
    ",
        )
        .unwrap();

    let graph = Graph::<SolData>::resolve(project.paths()).unwrap();
    assert_eq!(graph.files().len(), 2);
    assert_eq!(graph.files().clone(), HashMap::from([(src, 0), (lib, 1),]));

    let compiled = zksync::project_compile(project.project()).unwrap();
    assert!(compiled.find_first("Foo").is_some());
    assert!(compiled.find_first("Bar").is_some());
    compiled.assert_success();

    // nothing to compile
    let compiled = zksync::project_compile(project.project()).unwrap();
    assert!(compiled.find_first("Foo").is_some());
    assert!(compiled.is_unchanged());

    let cache = CompilerCache::<ZkSolcSettings>::read(&project.paths().cache).unwrap();
    assert_eq!(cache.files.len(), 2);

    // overwrite lib
    project
        .add_lib(
            "remapping/Bar",
            r"
    pragma solidity ^0.8.10;

    // changed lib
    contract Bar {}
    ",
        )
        .unwrap();

    let graph = Graph::<SolData>::resolve(project.paths()).unwrap();
    assert_eq!(graph.files().len(), 2);

    let compiled = zksync::project_compile(project.project()).unwrap();
    assert!(compiled.find_first("Foo").is_some());
    assert!(compiled.find_first("Bar").is_some());
    // ensure change is detected
    assert!(!compiled.is_unchanged());
}

#[test]
fn zksync_can_compile_dapp_detect_changes_in_sources() {
    let project = TempProject::<ZkSolcCompiler, ZkArtifactOutput>::dapptools().unwrap();

    let src = project
        .add_source(
            "DssSpell.t",
            r#"
    pragma solidity ^0.8.10;
    import "./DssSpell.t.base.sol";

   contract DssSpellTest is DssSpellTestBase { }
   "#,
        )
        .unwrap();

    let base = project
        .add_source(
            "DssSpell.t.base",
            r"
    pragma solidity ^0.8.10;

  contract DssSpellTestBase {
       address deployed_spell;
       function setUp() public {
           deployed_spell = address(0xA867399B43aF7790aC800f2fF3Fa7387dc52Ec5E);
       }
  }
   ",
        )
        .unwrap();

    let graph = Graph::<SolData>::resolve(project.paths()).unwrap();
    assert_eq!(graph.files().len(), 2);
    assert_eq!(graph.files().clone(), HashMap::from([(base, 0), (src, 1),]));
    assert_eq!(graph.imported_nodes(1).to_vec(), vec![0]);

    let compiled = zksync::project_compile(project.project()).unwrap();
    compiled.assert_success();
    assert!(compiled.find_first("DssSpellTest").is_some());
    assert!(compiled.find_first("DssSpellTestBase").is_some());

    // nothing to compile
    let compiled = zksync::project_compile(project.project()).unwrap();
    assert!(compiled.is_unchanged());
    assert!(compiled.find_first("DssSpellTest").is_some());
    assert!(compiled.find_first("DssSpellTestBase").is_some());

    let cache = CompilerCache::<ZkSolcSettings>::read(&project.paths().cache).unwrap();
    assert_eq!(cache.files.len(), 2);

    let artifacts = compiled.into_artifacts().collect::<HashMap<_, _>>();

    // overwrite import
    let _ = project
        .add_source(
            "DssSpell.t.base",
            r"
    pragma solidity ^0.8.10;

  contract DssSpellTestBase {
       address deployed_spell;
       function setUp() public {
           deployed_spell = address(0);
       }
  }
   ",
        )
        .unwrap();
    let graph = Graph::<SolData>::resolve(project.paths()).unwrap();
    assert_eq!(graph.files().len(), 2);

    let compiled = zksync::project_compile(project.project()).unwrap();
    assert!(compiled.find_first("DssSpellTest").is_some());
    assert!(compiled.find_first("DssSpellTestBase").is_some());
    // ensure change is detected
    assert!(!compiled.is_unchanged());

    // and all recompiled artifacts are different
    for (p, artifact) in compiled.into_artifacts() {
        let other = artifacts
            .iter()
            .find(|(id, _)| id.name == p.name && id.version == p.version && id.source == p.source)
            .unwrap()
            .1;
        assert_ne!(artifact, *other);
    }
}

#[test]
fn zksync_can_emit_build_info() {
    let mut project = TempProject::<ZkSolcCompiler, ZkArtifactOutput>::dapptools().unwrap();
    project.project_mut().build_info = true;
    project
        .add_source(
            "A",
            r#"
pragma solidity ^0.8.10;
import "./B.sol";
contract A { }
"#,
        )
        .unwrap();

    project
        .add_source(
            "B",
            r"
pragma solidity ^0.8.10;
contract B { }
",
        )
        .unwrap();

    let compiled = zksync::project_compile(project.project()).unwrap();
    compiled.assert_success();

    let info_dir = project.project().build_info_path();
    assert!(info_dir.exists());

    let mut build_info_count = 0;
    for entry in fs::read_dir(info_dir).unwrap() {
        let _info =
            BuildInfo::<ZkSolcInput, foundry_compilers_artifacts::zksolc::CompilerOutput>::read(
                &entry.unwrap().path(),
            )
            .unwrap();
        build_info_count += 1;
    }
    assert_eq!(build_info_count, 1);
}

#[test]
fn zksync_can_clean_build_info() {
    let mut project = TempProject::<ZkSolcCompiler, ZkArtifactOutput>::dapptools().unwrap();

    project.project_mut().build_info = true;
    project.project_mut().paths.build_infos = project.project_mut().paths.root.join("build-info");
    project
        .add_source(
            "A",
            r#"
pragma solidity ^0.8.10;
import "./B.sol";
contract A { }
"#,
        )
        .unwrap();

    project
        .add_source(
            "B",
            r"
pragma solidity ^0.8.10;
contract B { }
",
        )
        .unwrap();

    let compiled = zksync::project_compile(project.project()).unwrap();
    compiled.assert_success();

    let info_dir = project.project().build_info_path();
    assert!(info_dir.exists());

    let mut build_info_count = 0;
    for entry in fs::read_dir(info_dir).unwrap() {
        let _info =
            BuildInfo::<ZkSolcInput, foundry_compilers_artifacts::zksolc::CompilerOutput>::read(
                &entry.unwrap().path(),
            )
            .unwrap();
        build_info_count += 1;
    }
    assert_eq!(build_info_count, 1);

    project.project().cleanup().unwrap();

    assert!(!project.project().build_info_path().exists());
}

#[test]
fn zksync_can_use_allowed_paths_config() {
    // For this test we should create the following directory structure:
    // project_root/
    // ├── outer/
    // │   ├── Util.sol
    // │   └── Helper.sol
    // └── contracts/
    //     ├── src/
    //     │   └── Main.sol

    let tmp_dir = tempfile::tempdir().unwrap();
    let project_root = tmp_dir.path().to_path_buf();
    let contracts_dir = tempfile::tempdir_in(&project_root).unwrap();

    fs::create_dir_all(contracts_dir.path().join("src")).unwrap();
    fs::create_dir_all(project_root.join("outer")).unwrap();

    fs::write(
        contracts_dir.path().join("src/Main.sol"),
        r#"
pragma solidity ^0.8.0;
import "@outer/Helper.sol";
contract Main {
    Helper helper = new Helper();
    function run() public {}
}
"#,
    )
    .unwrap();

    fs::write(
        project_root.join("outer/Helper.sol"),
        r#"
pragma solidity ^0.8.0;
import "./Util.sol";
contract Helper {
    Util util = new Util();
}
"#,
    )
    .unwrap();

    fs::write(
        project_root.join("outer/Util.sol"),
        r#"
pragma solidity ^0.8.0;
contract Util {}
"#,
    )
    .unwrap();

    let root = contracts_dir.path().to_path_buf();
    let paths = ProjectPathsConfig::builder()
        .root(root.clone())
        .sources(root.join("src"))
        .remappings(vec![Remapping::from_str("@outer/=../outer/").unwrap()])
        .build()
        .unwrap();

    let inner = ProjectBuilder::<ZkSolcCompiler, ZkArtifactOutput>::new(Default::default())
        .paths(paths)
        .build(Default::default())
        .unwrap();
    let project =
        TempProject::<ZkSolcCompiler, ZkArtifactOutput>::create_new(contracts_dir, inner).unwrap();

    let compiled = zksync::project_compile(project.project()).unwrap();
    assert!(compiled.has_compiler_errors());
    assert!(compiled.compiler_output.errors.iter().any(|error| error
        .formatted_message
        .as_ref()
        .map_or(false, |msg| msg.contains("File outside of allowed directories"))));

    let mut project_with_allowed_paths = project.project().clone();
    project_with_allowed_paths.paths.allowed_paths = BTreeSet::from([PathBuf::from("../")]);

    let compiled_with_allowed_paths = zksync::project_compile(&project_with_allowed_paths).unwrap();
    compiled_with_allowed_paths.assert_success();
}

#[test]
fn zksync_can_compile_yul_sample() {
    let _ = tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .try_init()
        .ok();
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../test-data/zksync/yul-sample");
    let paths = ProjectPathsConfig::builder().sources(root);
    let project = TempProject::<ZkSolcCompiler, ZkArtifactOutput>::new(paths).unwrap();

    let compiled = zksync::project_compile(project.project()).unwrap();
    compiled.assert_success();

    let simple_store_artifact = compiled
        .compiled_artifacts
        .values()
        .find_map(|contracts| {
            contracts
                .iter()
                .find(|(name, _)| name.ends_with("SimpleStore.yul"))
                .and_then(|(_, artifacts)| artifacts.first())
        })
        .expect("SimpleStore.yul artifact not found")
        .artifact
        .bytecode
        .clone()
        .unwrap();

    let yul_bytecode = simple_store_artifact.object.as_bytes().unwrap();

    let expected_bytecode = Bytes::from_str("0x00000001002001900000000c0000c13d000000002101043c000000000010043f000000200100043d0000000701100197000000000202043b0000000802200197000000000112019f000000200010043f0000000001000019000000000001042d000000240000043f0000002001000039000001000010044300000120000004430000000601000041000000130001042e0000001200000432000000130001042e0000001400010430000000000000000000000000000000000000000000000000000000020000000000000000000000000000004000000100000000000000000000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000869af3b64c0f10f69e9ef1748219c7f959b7bc4ed89d052c7fde90836eff2a92").unwrap();

    // Assert bytecode equality
    assert_eq!(*yul_bytecode, expected_bytecode, "SimpleStore.yul bytecode mismatch");
}
