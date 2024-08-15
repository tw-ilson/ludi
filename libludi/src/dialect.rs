melior::dialect! {
    name: "ludi",
    table_gen:
r#"
include "mlir/IR/OpBase.td"
include "mlir/Interfaces/FunctionInterfaces.td"
include "mlir/IR/CommonAttrConstraints.td"


def Ludi_Dialect : Dialect {
  let name = "ludi";
  // let cppNamespace = "::mlir::ludi";
  let isExtensible = 1;
  let summary = "A high-level dialect for analyzing and optimizing the "
                "Ludi language and compiler";
  let description = [{
    The Ludi language is a multidimensional array language based on Remora that allows you to define
    functions, perform some math computation, and print results. This dialect
    provides a representation of the language that is amenable to analysis and
    optimization.
  }];

  let dependentDialects = [
    "tensor::TensorDialect",
    "arith::ArithDialect",
    "func::FuncDialect"
  ];

}

class Ludi_Op<string mnemonic, list<Trait> traits = []> :
    Op<Ludi_Dialect, mnemonic, traits>;

def AtomicF64ConstantOp: Ludi_Op<"atomicf64constant"> {
    let arguments = (ins F64Attr:$value);
    let results = (outs F64);
    let hasVerifier = 1;
    let builders = [
        OpBuilder<(ins "double":$value)>
    ];
}

def ArrayF64ConstantOp: Ludi_Op<"arrayf64constant"> {
    let arguments = (ins DenseF64ArrayAttr:$value);
    let results = (outs F64Tensor);
    let hasVerifier = 1;
    let builders = [ 
        OpBuilder<(ins "DenseF64ArrayAttr":$value)>
    ];
}

def AtomicF32ConstantOp: Ludi_Op<"atomicf32constant"> {
    let arguments = (ins F32Attr:$value);
    let results = (outs F32);
    let hasVerifier = 1;
    let builders = [
        OpBuilder<(ins "float":$value)>
    ];
}

def ArrayF32ConstantOp: Ludi_Op<"arrayf32constant"> {
    let arguments = (ins DenseF32ArrayAttr:$value);
    let results = (outs F32Tensor);
    let hasVerifier = 1;
    let builders = [ 
        OpBuilder<(ins "DenseF32ArrayAttr":$value)>
    ];
}

def FuncOp: Ludi_Op<"func", [
        FunctionOpInterface, IsolatedFromAbove
    ]> {
    let arguments = (ins 
                        SymbolNameAttr:$sym_name,
                        TypeAttrOf<FunctionType>: $function_type,
                        OptionalAttr<DictArrayAttr>: $arg_attrs,
                        OptionalAttr<DictArrayAttr>: $res_attrs
                        );
    let regions = (regions AnyRegion:$body);
    let builders = [OpBuilder<(ins
                        "StringRef":$name, "FunctionType":$type,
                        CArg<"ArrayRef<NamedAttribute>", "{}">:$attrs)
                        >];
    let extraClassDeclaration = [{
        ArrayRef<Type> getArgumentTypes() { return getFunctionType().getInputs(); }
        ArrayRef<Type> getResultTypes() { return getFunctionType().getResults(); }
        Region *getCallableRegion() { return &getBody(); }
    }];

    let hasCustomAssemblyFormat = 1;
    let skipDefaultBuilders = 1;
}

def AddF64Op: Ludi_Op<"addf64"> {
    let arguments = (ins F64Tensor:$lhs, F64Tensor:$rhs);
    let results = (outs F64Tensor);
}

def SubF64Op: Ludi_Op<"subf64"> {
    let arguments = (ins F64Tensor:$lhs, F64Tensor:$rhs);
    let results = (outs F64Tensor);
}

def PrintOp: Ludi_Op<"print"> {
    let arguments = (ins F64Tensor:$value);
    let assemblyFormat = "$input attr-dict `:` type($input)";
}
"#
}
