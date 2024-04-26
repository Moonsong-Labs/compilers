use std::borrow::Cow;

use crate::artifacts::bytecode::BytecodeObject;
use crate::zksync::artifacts::{bytecode::Bytecode, contract::CompactContractBytecodeCow};

pub trait Artifact {
    /// Returns the reference to the `bytecode`
    fn get_bytecode(&self) -> Option<Cow<'_, Bytecode>> {
        self.get_contract_bytecode().bytecode
    }

    /// Returns the reference to the `bytecode` object
    fn get_bytecode_object(&self) -> Option<Cow<'_, BytecodeObject>> {
        let val = match self.get_bytecode()? {
            Cow::Borrowed(b) => Cow::Borrowed(&b.object),
            Cow::Owned(b) => Cow::Owned(b.object),
        };
        Some(val)
    }

    /// Returns the reference of container type for abi, compact bytecode and deployed bytecode if
    /// available
    fn get_contract_bytecode(&self) -> CompactContractBytecodeCow<'_>;
}

impl<T> Artifact for T
where
    for<'a> &'a T: Into<CompactContractBytecodeCow<'a>>,
{
    fn get_contract_bytecode(&self) -> CompactContractBytecodeCow<'_> {
        self.into()
    }
}
