melior::dialect! {
    name: "ludi",
    table_gen:
r#"
include "mlir/IR/OpBase.td"
include "mlir/Interfaces/FunctionInterfaces.td"
include "mlir/IR/SymbolInterfaces.td"
include "mlir/Interfaces/SideEffectInterfaces.td"

// Provide a definition of the 'toy' dialect in the ODS framework so that we
// can define our operations.
def Ludi_Dialect : Dialect {
  let name = "ludi";

  // A short one-line summary of our dialect.
  let summary = "A high-level dialect for analyzing and optimizing the SHVM virtual machine for the"
                "Ludi language and compiler";

  // A much longer description of our dialect.
  let description = [{
    The Ludi language is a multidimensional array language based on Remora that allows you to define
    functions, perform some math computation, and print results. This dialect
    provides a representation of the language that is amenable to analysis and
    optimization.
  }];

  // The C++ namespace that the dialect class definition resides in.
  let cppNamespace = "ludi";
}

class Ludi_Op<string mnemonic, list<Trait> traits = []> :
    Op<Ludi_Dialect, mnemonic, traits>;
"#
}
