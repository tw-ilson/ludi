use libludi::data::DataType;
use libludi::parser::Parser;
use libludi::ast::*;
use shvm::vm::OpCode;

macro_rules! chunk {
    ($vm:tt, [$($op:expr),*]) => {
    use OpCode::*;
    $(    
        let op = $op;
        $vm.push(op);
    )*
    };
}

#[test]
fn op_negate() {
    let mut vm = ChunkMachine::new_debug("test");
    chunk!(vm, [
           LOADCONST { value: 123.into(), dest: 0 },
           NEG {src: 0, dest: 0},
           RETURN]
           );
    assert_eq!(vm.evaluate().unwrap().unwrap(), (-123).into());
}

#[test]
fn op_add() {
    let mut vm = ChunkMachine::new_debug("test");
    chunk!(vm,
        [
            LOADCONST {value:123.into(), dest: 0},
            LOADCONST { value: 101.into(), dest: 1 },
            ADD { src1: 0, src2: 1, dest: 0 }, 
            RETURN
        ]);
    assert_eq!(vm.evaluate().unwrap().unwrap(), 224.into());
}
//
#[test]
fn op_subtract() {
    let mut vm = ChunkMachine::new_debug("test");
    chunk!(vm,
        [
            LOADCONST { value: 101.into(), dest: 0 },
            LOADCONST {value:123.into(), dest: 1},
            SUB { src1: 1, src2: 0, dest: 0 } , 
            RETURN
        ]);
    assert_eq!(vm.evaluate().unwrap().unwrap(), 22.into());
}
//
#[test]
fn op_multiply() {
    let mut vm = ChunkMachine::new_debug("test");
    chunk!(vm,
        [
            LOADCONST { value: 8.into(), dest:0 },
            LOADCONST{value:9.into(), dest:1},
            MUL { src1: 0, src2: 1, dest: 0 }, 
            RETURN
        ]);
    assert_eq!(vm.evaluate().unwrap().unwrap(), 72.into());
}
//
#[test]
fn op_divide() {
    let mut vm = ChunkMachine::new_debug("test");
    chunk!(vm,
        [
            LOADCONST { value: 4.into(), dest: 0 },
            LOADCONST{value: 16.into(), dest: 1},
            DIV { src1: 1, src2: 0, dest: 0 }, 
            RETURN
        ]);
    assert_eq!(vm.evaluate().unwrap().unwrap(), (4.0).into());
}

// #[test]
// fn basic_compile() {
//     let mut src = String::from("-123");
//     let asm = Chunk::new();
//     let asm = src.parse().unwrap().compile(asm).unwrap();
//     let mut vm = ChunkMachine::from(asm);
//     let expect: Option<AtomicData> = Some((-123).into());
//     assert_eq!(vm.evaluate().unwrap(), expect);
// }
