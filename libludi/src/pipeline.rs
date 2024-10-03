use crate::ast::ParseTree;
use crate::lex::TokenStream;
use crate::err::{Error, Result};

trait Conversion<Target> : TryInto<Target, Error = Error> {
    fn apply_conversion(self) -> Result<Target> {
        self.try_into()
    }
}

// macro_rules! define_pipeline {
//     ($($source:ident -> $target:ident),*) => {
//         $(
//             paste::paste! {
//                 trait [< $source To $target>] : Conversion<$source, $target> {}
//             }
//         )*
//     };
// }
//
// define_pipeline! {
//     String -> TokenStream,
//     TokenStream -> ParseTree,
//     ParseTree -> NormalizedAST,
// }
