
#[derive(Debug, Clone)]
#[repr(u8)]
pub enum OpCode {
    // Control flow
    CALL,
    RETURN,

    // two address
    LOAD,
    LOADCONST,
    STORE,
    NEG,

    // Three address
    ADD,
    SUB,
    MUL,
    DIV,

    // Shape ops
    RESHAPE,
    CONCAT,

    //Other 
    SYS
}

pub struct Operation {
    code: OpCode,
}

// #[derive(Debug, Clone)]
// #[repr(u8)]
// pub enum Operation {
//     // Control flow
//     CALL { label: u8 /*placeholder*/ },
//     RETURN,
//
//     // two address
//     LOAD { src: VAddr, dest: VReg },
//     LOADCONST { value: Value, dest: VReg }, // change this to a raw pointer eventually
//     STORE { src: VReg, dest: VReg },
//     NEG { src: VReg, dest: VReg },
//
//     // Three address
//     ADD { src1: VReg, src2: VReg, dest: VReg },
//     SUB { src1: VReg, src2: VReg, dest: VReg },
//     MUL { src1: VReg, src2: VReg, dest: VReg },
//     DIV { src1: VReg, src2: VReg, dest: VReg },
//
//     // Shape ops
//     RESHAPE,
//     CONCAT,
// }
