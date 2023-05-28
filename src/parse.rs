use std::iter::once;

use crate::{
    lex::{lex, BinOp, Token},
    Expr, Ident, LexError, Span,
};

#[derive(Debug, Clone)]
pub enum ParseError {
    Lex(LexError),
    Expected(Vec<Expectation>, Option<Token>, Span),
}

#[derive(Debug, Clone)]
pub enum Expectation {
    Exact(Token),
    Expression,
}

pub type ParseResult<T = ()> = Result<T, ParseError>;

pub fn parse(input: &str) -> ParseResult<Expr> {
    let tokens = lex(input).map_err(ParseError::Lex)?;
    Parser { tokens, curr: 0 }
        .expr()
        .map(|expr| expr.unwrap_or(Expr::Nil))
}

struct Parser {
    tokens: Vec<(Token, Span)>,
    curr: usize,
}

impl Parser {
    fn expr(&mut self) -> ParseResult<Option<Expr>> {
        self.cmp_expr()
    }
    fn cmp_expr(&mut self) -> ParseResult<Option<Expr>> {
        let Some(mut left) = self.add_sub_expr()? else {
            return Ok(None);
        };
        while let Some((
            Token::BinOp(
                op @ (BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge),
            ),
            _,
        )) = self.curr()
        {
            self.curr += 1;
            let right = self
                .add_sub_expr()?
                .ok_or_else(|| self.expected([Expectation::Expression]))?;
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        Ok(Some(left))
    }
    fn add_sub_expr(&mut self) -> ParseResult<Option<Expr>> {
        let Some(mut left) = self.mul_div_rem_expr()? else {
            return Ok(None);
        };
        while let Some((Token::BinOp(op @ (BinOp::Add | BinOp::Sub)), _)) = self.curr() {
            self.curr += 1;
            let right = self
                .mul_div_rem_expr()?
                .ok_or_else(|| self.expected([Expectation::Expression]))?;
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        Ok(Some(left))
    }
    fn mul_div_rem_expr(&mut self) -> ParseResult<Option<Expr>> {
        let Some(mut left) = self.call_expr()? else {
            return Ok(None);
        };
        while let Some((Token::BinOp(op @ (BinOp::Mul | BinOp::Div | BinOp::Rem)), _)) = self.curr()
        {
            self.curr += 1;
            let right = self
                .call_expr()?
                .ok_or_else(|| self.expected([Expectation::Expression]))?;
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        Ok(Some(left))
    }
    fn call_expr(&mut self) -> ParseResult<Option<Expr>> {
        let Some (mut left) = self.term()? else {
            return Ok(None);
        };
        if self.token(Token::OpenParen).is_some() {
            let mut args = Vec::new();
            while let Some(expr) = self.expr()? {
                args.push(expr);
                self.token(Token::Comma);
            }
            self.expect(Token::CloseParen, [Expectation::Expression])?;
            left = Expr::Call(Box::new(left), args);
        }
        Ok(Some(left))
    }
    fn term(&mut self) -> ParseResult<Option<Expr>> {
        Ok(Some(if self.token(Token::Nil).is_some() {
            Expr::Nil
        } else if let Some(b) = self.bool() {
            Expr::Bool(b)
        } else if let Some(ident) = self.ident() {
            Expr::Ident(ident)
        } else if let Some(number) = self.number() {
            Expr::Number(number)
        } else if self.token(Token::OpenParen).is_some() {
            let inner = self.expr()?.unwrap_or(Expr::Nil);
            self.expect(Token::CloseParen, [Expectation::Expression])?;
            Expr::Paren(Box::new(inner))
        } else if self.token(Token::OpenBracket).is_some() {
            let mut list = Vec::new();
            while let Some(expr) = self.expr()? {
                list.push(expr);
                self.token(Token::Comma);
            }
            self.expect(Token::CloseBracket, [Expectation::Expression])?;
            Expr::List(list)
        } else {
            return Ok(None);
        }))
    }
    fn bool(&mut self) -> Option<bool> {
        if let Some((Token::Bool(b), _)) = self.curr() {
            self.curr += 1;
            Some(b)
        } else {
            None
        }
    }
    fn ident(&mut self) -> Option<Ident> {
        if let Some((Token::Ident(ident), _)) = self.curr() {
            self.curr += 1;
            Some(ident)
        } else {
            None
        }
    }
    fn number(&mut self) -> Option<Ident> {
        if let Some((Token::Number(number), _)) = self.curr() {
            self.curr += 1;
            Some(number)
        } else {
            None
        }
    }
    fn token(&mut self, token: Token) -> Option<Span> {
        if let Some((t, span)) = self.curr() {
            if t == token {
                self.curr += 1;
                Some(span)
            } else {
                None
            }
        } else {
            None
        }
    }
    fn expect(
        &mut self,
        token: Token,
        expected: impl IntoIterator<Item = Expectation>,
    ) -> ParseResult<Span> {
        if let Some((t, span)) = self.curr() {
            if t == token {
                self.curr += 1;
                Ok(span)
            } else {
                Err(ParseError::Expected(
                    expected
                        .into_iter()
                        .chain(once(Expectation::Exact(token)))
                        .collect(),
                    Some(t),
                    span,
                ))
            }
        } else {
            Err(ParseError::Expected(
                expected
                    .into_iter()
                    .chain(once(Expectation::Exact(token)))
                    .collect(),
                None,
                self.tokens.last().unwrap().1,
            ))
        }
    }
    fn curr(&self) -> Option<(Token, Span)> {
        self.tokens.get(self.curr).cloned()
    }
    fn expected(&mut self, expected: impl IntoIterator<Item = Expectation>) -> ParseError {
        let (token, span) = if let Some((token, span)) = self.curr() {
            (Some(token), span)
        } else {
            (None, self.tokens.last().unwrap().1)
        };
        ParseError::Expected(expected.into_iter().collect(), token, span)
    }
}
