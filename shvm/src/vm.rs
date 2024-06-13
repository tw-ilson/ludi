use libludi::{
    atomic::NumberType,
    data::{Data, DataType},
    err::{LangError, Result},
    ops::*,
};

use std::cell::OnceCell;
use std::env;
use std::fs::write;
use std::ops::{Index, IndexMut};

type Value = DataType;
pub struct Stack(Vec<Value>); // values on our stack have unbounded size
pub struct Chunk(Vec<OpCode>);

const N_REGS: usize = 4;
type VReg = u8;
type VAddr = isize; // virtual address space

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum OpCode {
    // Control flow
    BRANCH { cond: VReg },
    CALL { label: u8 /*placeholder*/ },
    RETURN,

    // two address
    LOAD { src: VAddr, dest: VReg },
    LOADCONST { value: Value, dest: VReg }, // change this to a raw pointer eventually
    STORE { src: VReg, dest: VReg },
    NEG { src: VReg, dest: VReg },
    INV { src: VReg, dest: VReg },

    // Three address
    ADD { src1: VReg, src2: VReg, dest: VReg },
    SUB { src1: VReg, src2: VReg, dest: VReg },
    MUL { src1: VReg, src2: VReg, dest: VReg },
    DIV { src1: VReg, src2: VReg, dest: VReg },

    // Array/Rank ops
    EXP,
    REDUCE,
    GEMM,

    // Shape ops
    RESHAPE,
    CONCAT,
}

// the virtual machine
pub struct ChunkMachine {
    name: String,
    registers: [Option<Value>; N_REGS],
    constants: OnceCell<Box<[Value]>>,
    program: Chunk,
    stack: Stack,
    trace_flag: bool,
}

impl ChunkMachine {
    fn eval_op(&mut self, op: OpCode) -> Result<Option<Value>> {
        use OpCode::*;
        match op {
            LOADCONST { value, dest } => {
                // this will use constants instead
                self.registers[dest as usize] = Some(value);
                Ok(None)
            }
            LOAD { src, dest } => {
                self.registers[dest as usize] = Some(self.stack[src].clone());
                Ok(None)
            }
            STORE { src, dest } => {
                if let Some(v) = &self.registers[src as usize] {
                    self.stack[dest as isize] = v.clone();
                } // do nothing if register is empty?
                Ok(None)
            }
            NEG { src, dest } => {
                if let Some(v) = &self.registers[src as usize] {
                    self.registers[dest as usize] = Some(v.neg())
                }
                Ok(None)
            }
            ADD { src1, src2, dest } => {
                if let (Some(v1), Some(v2)) = (
                    &self.registers[src1 as usize],
                    &self.registers[src2 as usize],
                ) {
                    self.registers[dest as usize] = Some(v1.add(v2))
                }
                Ok(None)
            }
            SUB { src1, src2, dest } => {
                if let (Some(v1), Some(v2)) = (
                    &self.registers[src1 as usize],
                    &self.registers[src2 as usize],
                ) {
                    self.registers[dest as usize] = Some(v1.sub(v2))
                }
                Ok(None)
            }
            MUL { src1, src2, dest } => {
                if let (Some(v1), Some(v2)) = (
                    &self.registers[src1 as usize],
                    &self.registers[src2 as usize],
                ) {
                    self.registers[dest as usize] = Some(v1.mul(v2))
                }
                Ok(None)
            }
            DIV { src1, src2, dest } => {
                if let (Some(v1), Some(v2)) = (
                    &self.registers[src1 as usize],
                    &self.registers[src2 as usize],
                ) {
                    self.registers[dest as usize] = Some(v1.div(v2))
                }
                Ok(None)
            }
            RETURN => Ok(self.registers[0].clone()),
            _ => Err(LangError::RuntimeErr(op, "unsupported op!".into())),
        }
    }
    pub fn evaluate(&mut self) -> Result<Option<Value>> {
        let mut r = None;
        for op in self.program.0.clone().into_iter() {
            if self.trace_flag {
                println!("-- REGISTERS --");
                println!("{:?}", self.registers);
                println!("-- Stack --");
                println!("{:?}\n-->{:?}\n", self.stack, op);
            }
            r = self.eval_op(op)?;
        }
        return Ok(r);
    }
    pub fn push(&mut self, instr: OpCode) {
        self.program.0.push(instr)
    }
    pub fn new(name: &str) -> Self {
        let mut r = Self::default();
        r.name = name.into();
        r
    }
    pub fn new_debug(name: &str) -> Self {
        let mut r = Self::new(name);
        r.debug();
        r
    }
    pub fn debug(&mut self) {
        self.trace_flag = true;
    }
    pub fn disassemble(&self) -> String {
        format!(
            "\n== {} ==\n{}",
            self.name,
            self.program
                .0
                .iter()
                .map(|op| format!("{:?}\n", op))
                .collect::<String>()
        )
    }
}

impl Chunk {
    pub fn new() -> Self {
        Chunk(vec![])
    }
    fn push(mut self, rhs: OpCode) -> Self {
        self.0.push(rhs);
        self
    }
    fn concat(self, rhs: Self) -> Self {
        Chunk([self.0, rhs.0].concat())
    }
}

impl From<Chunk> for ChunkMachine {
    fn from(value: Chunk) -> Self {
        let mut r = Self::default();
        r.program = value;
        r
    }
}

impl Default for ChunkMachine {
    fn default() -> Self {
        Self {
            name: String::from("vm"),
            registers: std::array::from_fn(|_| None),
            program: Chunk::new(),
            constants: OnceCell::new(),
            stack: Stack::default(),
            trace_flag: false,
        }
    }
}

impl std::fmt::Debug for ChunkMachine {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.disassemble())
    }
}
impl Stack {
    fn push(&mut self, v: Value) {
        self.0.push(v);
    }
    fn peek(&mut self) -> &Value {
        self.0.last().expect("nothing on stack!")
    }
    fn pop(&mut self) -> Value {
        self.0.pop().expect("nothing on the stack!")
    }
}
impl Index<isize> for Stack {
    type Output = Value;
    fn index(&self, index: isize) -> &Self::Output {
        if index < 0 {
            panic!("expects positive index for values on stack")
        }
        self.0
            .get(index as usize)
            .expect("indexing beyond stack pointer!")
    }
}
impl IndexMut<isize> for Stack {
    fn index_mut(&mut self, index: isize) -> &mut Self::Output {
        if index < 0 {
            panic!("expects positive index for values on stack")
        }
        self.0
            .get_mut(index as usize)
            .expect("indexing beyond stack pointer!")
    }
}
impl Default for Stack {
    fn default() -> Self {
        Stack(vec![])
    }
}
impl std::fmt::Debug for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "[{}\n]",
            self.0
                .iter()
                .fold("".to_string(), |acc, v| format!("{}\n{}", acc, v))
        )
    }
}
