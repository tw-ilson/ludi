use libludi::{
    data::DataType,
    err::{LangError, Result},
    ops::*,
};

use std::cell::OnceCell;

type Value = DataType;

struct Module {
    funcs: Vec<FnData>
}
#[derive(Clone)]
pub struct BasicBlock(Vec<Operation>);


trait Evaluate {
    fn evaluate(self) -> Result<()>;
}

// #[derive(Clone)]
// pub struct Stack(Vec<Value>); // values on our stack have unbounded size

// the virtual machine
pub struct ChunkMachine {
    name: String,
    // registers: &'static [Value],
    constants: OnceCell<Box<[Value]>>,
    // program: Chunk,
    // stack: Stack,
    trace_flag: bool,
}

impl ChunkMachine {
    fn eval_op(&mut self, op: OpCode, registers: &[Value]) -> Result<Option<Value>> {
        use OpCode::*;
        match op {
            LOADCONST { value, dest } => {
                // this will use constants instead
                self.registers[dest as usize] = value;
                Ok(None)
            }
            LOAD { src, dest } => {
                self.registers[dest as usize] = self.stack[src].clone();
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
                    self.registers[dest as usize] = Some(libludi::ops::Neg::neg(*v)?)
                }
                Ok(None)
            }
            ADD { src1, src2, dest } => {
                if let (Some(v1), Some(v2)) = (
                    &self.registers[src1 as usize],
                    &self.registers[src2 as usize],
                ) {
                    self.registers[dest as usize] = Some(Add::add(v1.clone(),v2.clone())?)
                }
                Ok(None)
            }
            SUB { src1, src2, dest } => {
                if let (Some(v1), Some(v2)) = (
                    &self.registers[src1 as usize],
                    &self.registers[src2 as usize],
                ) {
                    self.registers[dest as usize] = Some(Sub::sub(v1.clone(), v2.clone())?)
                }
                Ok(None)
            }
            MUL { src1, src2, dest } => {
                if let (Some(v1), Some(v2)) = (
                    &self.registers[src1 as usize],
                    &self.registers[src2 as usize],
                ) {
                    self.registers[dest as usize] = Some(Mul::mul(v1.clone(), v2.clone())?)
                }
                Ok(None)
            }
            DIV { src1, src2, dest } => {
                if let (Some(v1), Some(v2)) = (
                    &self.registers[src1 as usize],
                    &self.registers[src2 as usize],
                ) {
                    self.registers[dest as usize] = Some(Div::div(v1.clone(), v2.clone())?)
                }
                Ok(None)
            }
            RETURN => Ok(self.registers[0].clone()),
            _ => Err(LangError::RuntimeErr(format!("unsupported op: {:?}", op))),
        }
    }
    pub fn print_trace(&self, op: OpCode) {
        if self.trace_flag {
            println!("-- REGISTERS --");
            println!("{:?}", self.registers);
            println!("-- Stack --");
            println!("{:?}\n-->{:?}\n", self.stack, op);
        }
    }
    pub fn evaluate(&mut self) -> Result<Option<Value>> {
        let mut r = None;
        for op in self.program.0.clone().into_iter() {
            self.print_trace(op.clone());
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
            // registers: std::array::from_fn(|_| None),
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
impl std::ops::Index<isize> for Stack {
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
impl std::ops::IndexMut<isize> for Stack {
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
