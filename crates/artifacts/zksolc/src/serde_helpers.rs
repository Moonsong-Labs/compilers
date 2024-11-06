use foundry_compilers_artifacts_solc::BytecodeObject;
use serde::{Deserialize, Deserializer};

use crate::{bytecode::Bytecode, contract::RawContract};

/// Deserialize a [`RawContract`] with added `missingLibraries` field,
/// to populate [`Bytecode`]'s `missing_libraries` with it
pub fn maybe_unlinked_contract<'de, D>(deserializer: D) -> Result<RawContract, D::Error>
where
    D: Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(rename_all = "camelCase")]
    struct RawContractWithLibs {
        #[serde(default)]
        pub missing_libraries: Vec<String>,
        #[serde(flatten)]
        pub contract: RawContract,
    }

    let RawContractWithLibs { missing_libraries, mut contract } =
        RawContractWithLibs::deserialize(deserializer)?;

    if !missing_libraries.is_empty() {
        if let Some(bc) = contract.eravm.as_mut().and_then(|eravm| eravm.bytecode.as_mut()) {
            bc.missing_libraries = missing_libraries;
            bc.mark_as_unlinked();
        }
    }

    Ok(contract)
}

/// Deserialize a [ `Bytecode` ] by either the raw bytecode object (from the compiler output),
/// or from a wrapped bytecode object from a stored artifact
pub fn opt_maybe_unwrapped_bytecode<'de, D>(deserializer: D) -> Result<Option<Bytecode>, D::Error>
where
    D: Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum CompilerBytecodeOrStored {
        // what we store in our artifacts
        Stored(Bytecode),
        // what the compiler returns
        Compiler(BytecodeObject),
    }

    impl Into<Bytecode> for CompilerBytecodeOrStored {
        fn into(self) -> Bytecode {
            match self {
                CompilerBytecodeOrStored::Stored(bc) => bc,
                CompilerBytecodeOrStored::Compiler(object) => {
                    Bytecode { object, missing_libraries: Default::default() }
                }
            }
        }
    }

    Ok(<Option<CompilerBytecodeOrStored>>::deserialize(deserializer)?.map(Into::into))
}
