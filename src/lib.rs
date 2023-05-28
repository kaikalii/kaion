use lex::BinOp;

mod lex;
mod parse;

pub use lex::{Ident, LexError, Span};
pub use parse::parse;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
    Nil,
    Bool(bool),
    Ident(Ident),
    Number(Ident),
    BinOp(Box<Self>, BinOp, Box<Self>),
    Paren(Box<Self>),
    List(Vec<Self>),
    Object(Vec<(Ident, Self)>),
    If(Box<Self>, Box<Self>, Option<Box<Self>>),
    Call(Box<Self>, Vec<Self>),
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parsing() {
        println!("{:?}", parse("assert(3 == 1 + 2)").unwrap());
    }
}
