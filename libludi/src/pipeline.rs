use crate::ast::ParseTree;
use crate::err::{Error, Result};
use crate::lex::TokenStream;

trait Transform<Target>: TryInto<Target, Error = Error> {
    fn transform(self) -> Result<Target>;
}

impl<S, T> Transform<T> for S
where
    S: TryInto<T, Error = Error>,
{
    fn transform(self) -> Result<T> {
        self.try_into()
    }
}
// // apparently something like this cant't work without ![feature(specialization)]
// impl<T_1, T_2, T_3> Transform<T_3> for T_1
// where
//     T_1: Transform<T_2>,
//     T_2: Transform<T_3>,
// {
//     fn transform(self) -> Result<T_3> {
//
//     }
// }

// Something like this eventually:

// given input T_1, T_2 ... T_n where Conversion<T_i+1> for T_i
// output type: Conversion<T_n> for T_1
macro_rules! pipeline {
    (
        $($source:ident -> $target:ident),*
        ) => {
        $(
            paste::paste! {
                trait [< $source To $target >] : Transform<$source, $target> {}
            }
        )*
    };
}

// pipeline! {
//     String -> TokenStream, 
//     TokenStream -> ParseTree,
//     ParseTree -> NormalizedAST,
// }
