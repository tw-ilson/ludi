use libludi::lex::Lex;
use libludi::parser::Parser;
use libludi::codegen::WriteMLIR;
use libludi::types::typecheck::TypeCheck;
use libludi::err::Result;
use libludi::types::TypeEnv;

pub trait Pipeline<Source, Target> {
    fn apply(source: Source) -> Result<Target>;
}

pub struct BasicCompiler;

impl Pipeline<String, String> for BasicCompiler {
    fn apply(source: String) -> Result<String> {
        Ok(source
            .lex()
            .parse()?
            // .optimize()
            .type_check(&mut TypeEnv::new())?
            .write())
    }
}
