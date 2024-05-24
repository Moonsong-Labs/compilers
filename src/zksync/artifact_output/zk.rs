use crate::{
    artifact_output::{ArtifactFile, Artifacts, ArtifactsMap},
    artifacts::{DevDoc, SourceFile, StorageLayout, UserDoc},
    compile::output::sources::{VersionedSourceFile, VersionedSourceFiles},
    config::ProjectPathsConfig,
    error::{Result, SolcIoError},
    zksync::{
        artifact_output::{
            conflict_free_output_file, files::MappedContract, output_file, output_file_versioned,
            OutputContext,
        },
        artifacts::{
            bytecode::Bytecode,
            contract::{CompactContractBytecodeCow, Contract},
            Evm,
        },
        compile::output::contracts::VersionedContracts,
    },
};
use alloy_json_abi::JsonAbi;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    collections::{BTreeMap, HashSet},
    fs,
    path::Path,
};

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ZkContractArtifact {
    pub abi: Option<JsonAbi>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bytecode: Option<Bytecode>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub assembly: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub method_identifiers: Option<BTreeMap<String, String>>,
    //#[serde(default, skip_serializing_if = "Vec::is_empty")]
    //pub generated_sources: Vec<GeneratedSource>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub metadata: Option<serde_json::Value>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub storage_layout: Option<StorageLayout>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub userdoc: Option<UserDoc>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub devdoc: Option<DevDoc>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub ir_optimized: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hash: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub factory_dependencies: Option<BTreeMap<String, String>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub missing_libraries: Option<Vec<String>>,
    /// The identifier of the source file
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub id: Option<u32>,
}

impl<'a> From<&'a ZkContractArtifact> for CompactContractBytecodeCow<'a> {
    fn from(artifact: &'a ZkContractArtifact) -> Self {
        CompactContractBytecodeCow {
            abi: artifact.abi.as_ref().map(Cow::Borrowed),
            bytecode: artifact.bytecode.as_ref().map(Cow::Borrowed),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub struct ZkArtifactOutput();

impl ZkArtifactOutput {
    fn contract_to_artifact(
        &self,
        _file: &str,
        _name: &str,
        contract: Contract,
        source_file: Option<&SourceFile>,
    ) -> ZkContractArtifact {
        let mut artifact_bytecode = None;
        let mut artifact_method_identifiers = None;
        let mut artifact_assembly = None;

        let Contract {
            abi,
            metadata,
            userdoc,
            devdoc,
            storage_layout,
            evm,
            ir_optimized,
            hash,
            factory_dependencies,
            missing_libraries,
        } = contract;

        if let Some(evm) = evm {
            let Evm {
                assembly,
                bytecode,
                method_identifiers,
                extra_metadata: _,
                legacy_assembly: _,
            } = evm;

            artifact_bytecode = bytecode.map(Into::into);
            artifact_method_identifiers = Some(method_identifiers);
            artifact_assembly = assembly;
        }

        ZkContractArtifact {
            abi,
            hash,
            factory_dependencies,
            missing_libraries,
            storage_layout: Some(storage_layout),
            bytecode: artifact_bytecode,
            assembly: artifact_assembly,
            method_identifiers: artifact_method_identifiers,
            metadata,
            userdoc: Some(userdoc),
            devdoc: Some(devdoc),
            ir_optimized,
            id: source_file.as_ref().map(|s| s.id),
        }
    }

    pub fn on_output(
        &self,
        contracts: &VersionedContracts,
        sources: &VersionedSourceFiles,
        layout: &ProjectPathsConfig,
        ctx: OutputContext<'_>,
    ) -> Result<Artifacts<ZkContractArtifact>> {
        let mut artifacts = self.output_to_artifacts(contracts, sources, ctx, layout);
        fs::create_dir_all(&layout.zksync_artifacts).map_err(|err| {
            error!(dir=?layout.zksync_artifacts, "Failed to create artifacts folder");
            SolcIoError::new(err, &layout.zksync_artifacts)
        })?;

        artifacts.join_all(&layout.zksync_artifacts);
        artifacts.write_all()?;

        Ok(artifacts)
    }

    /// Convert the compiler output into a set of artifacts
    ///
    /// **Note:** This does only convert, but _NOT_ write the artifacts to disk, See
    /// [`Self::on_output()`]
    pub fn output_to_artifacts(
        &self,
        contracts: &VersionedContracts,
        sources: &VersionedSourceFiles,
        ctx: OutputContext<'_>,
        layout: &ProjectPathsConfig,
    ) -> Artifacts<ZkContractArtifact> {
        let mut artifacts = ArtifactsMap::new();

        // this tracks all the `SourceFile`s that we successfully mapped to a contract
        let mut non_standalone_sources = HashSet::new();

        // this holds all output files and the contract(s) it belongs to
        let artifact_files = contracts.artifact_files(&ctx);

        // this tracks the final artifacts, which we use as lookup for checking conflicts when
        // converting stand-alone artifacts in the next step
        let mut final_artifact_paths = HashSet::new();

        for contracts in artifact_files.files.into_values() {
            for (idx, mapped_contract) in contracts.iter().enumerate() {
                let MappedContract { file, name, contract, artifact_path } = mapped_contract;
                // track `SourceFile`s that can be mapped to contracts
                let source_file = sources.find_file_and_version(file, &contract.version);

                if let Some(source) = source_file {
                    non_standalone_sources.insert((source.id, &contract.version));
                }

                let mut artifact_path = artifact_path.clone();

                if contracts.len() > 1 {
                    // naming conflict where the `artifact_path` belongs to two conflicting
                    // contracts need to adjust the paths properly

                    // we keep the top most conflicting file unchanged
                    let is_top_most =
                        contracts.iter().enumerate().filter(|(i, _)| *i != idx).all(|(_, c)| {
                            Path::new(file).components().count()
                                < Path::new(c.file).components().count()
                        });
                    if !is_top_most {
                        // we resolve the conflicting by finding a new unique, alternative path
                        artifact_path = conflict_free_output_file(
                            &final_artifact_paths,
                            artifact_path,
                            file,
                            &layout.zksync_artifacts,
                        );
                    }
                }

                final_artifact_paths.insert(artifact_path.clone());

                let artifact =
                    self.contract_to_artifact(file, name, contract.contract.clone(), source_file);

                let artifact = ArtifactFile {
                    artifact,
                    file: artifact_path,
                    version: contract.version.clone(),
                };

                artifacts
                    .entry(file.to_string())
                    .or_default()
                    .entry(name.to_string())
                    .or_default()
                    .push(artifact);
            }
        }

        // extend with standalone source files and convert them to artifacts
        // this is unfortunately necessary, so we can "mock" `Artifacts` for solidity files without
        // any contract definition, which are not included in the `CompilerOutput` but we want to
        // create Artifacts for them regardless
        for (file, sources) in sources.as_ref().iter() {
            for source in sources {
                if !non_standalone_sources.contains(&(source.source_file.id, &source.version)) {
                    // scan the ast as a safe measure to ensure this file does not include any
                    // source units
                    // there's also no need to create a standalone artifact for source files that
                    // don't contain an ast
                    if source.source_file.contains_contract_definition()
                        || source.source_file.ast.is_none()
                    {
                        continue;
                    }

                    // we use file and file stem
                    if let Some(name) = Path::new(file).file_stem().and_then(|stem| stem.to_str()) {
                        if let Some(artifact) =
                            self.standalone_source_file_to_artifact(file, source)
                        {
                            let mut artifact_path = if sources.len() > 1 {
                                output_file_versioned(file, name, &source.version)
                            } else {
                                output_file(file, name)
                            };

                            if final_artifact_paths.contains(&artifact_path) {
                                // preventing conflict
                                artifact_path = conflict_free_output_file(
                                    &final_artifact_paths,
                                    artifact_path,
                                    file,
                                    &layout.zksync_artifacts,
                                );
                                final_artifact_paths.insert(artifact_path.clone());
                            }

                            let entries = artifacts
                                .entry(file.to_string())
                                .or_default()
                                .entry(name.to_string())
                                .or_default();

                            if entries.iter().all(|entry| entry.version != source.version) {
                                entries.push(ArtifactFile {
                                    artifact,
                                    file: artifact_path,
                                    version: source.version.clone(),
                                });
                            }
                        }
                    }
                }
            }
        }

        Artifacts(artifacts)
    }

    fn standalone_source_file_to_artifact(
        &self,
        _path: &str,
        file: &VersionedSourceFile,
    ) -> Option<ZkContractArtifact> {
        file.source_file.ast.clone().map(|_ast| ZkContractArtifact {
            abi: Some(JsonAbi::default()),
            id: Some(file.source_file.id),
            ..Default::default()
        })
    }
}
