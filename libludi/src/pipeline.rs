use crate::ast::ParseTree;
use crate::lex::TokenStream;

trait Conversion<Source, Target> {
    fn apply_conversion(source: Source) -> Target;
}

macro_rules! define_pipeline {
    ($($source:ident -> $target:ident),*) => {
        $(
            trait ${concat($source,To,$target)} : Conversion<$source, $target> {}
        )*
    };
}

define_pipeline! {
    String -> TokenStream,
    TokenStream -> ParseTree
    ParseTree -> NormalizedAST
}
