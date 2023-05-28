use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loc {
    pub pos: usize,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}

impl Span {
    pub fn new(start: Loc, end: Loc) -> Self {
        Self { start, end }
    }
    pub fn sp<T>(self, value: T) -> Sp<T> {
        Sp { span: self, value }
    }
    pub fn merge(self, other: Self) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Sp<T> {
    pub span: Span,
    pub value: T,
}

pub type Ident = Rc<str>;

#[derive(Debug)]
pub enum LexError {
    InvalidChar(char),
}

#[derive(Debug, Clone)]
pub enum Token {
    Ident(Ident),
    BinOp(BinOp),
    Number(Ident),
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,
    Comma,
    Dot,
    Colon,
    Equals,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

pub fn lex(input: &str) -> Result<Vec<Sp<Token>>, LexError> {
    Lexer {
        chars: input.chars().collect(),
        loc: Loc {
            pos: 0,
            line: 1,
            col: 1,
        },
        start: Loc {
            pos: 0,
            line: 1,
            col: 1,
        },
        tokens: Vec::new(),
    }
    .lex()
}

struct Lexer {
    chars: Vec<char>,
    loc: Loc,
    start: Loc,
    tokens: Vec<Sp<Token>>,
}

impl Lexer {
    fn next_if(&mut self, f: impl FnOnce(char) -> bool) -> Option<char> {
        let c = *self.chars.get(self.loc.pos)?;
        if f(c) {
            self.loc.pos += 1;
            match c {
                '\n' => {
                    self.loc.line += 1;
                    self.loc.col = 1;
                }
                _ => self.loc.col += 1,
            }
            Some(c)
        } else {
            None
        }
    }
    fn next(&mut self) -> Option<char> {
        self.next_if(|_| true)
    }
    fn next_is(&mut self, c: char) -> bool {
        self.next_if(|x| x == c).is_some()
    }
    fn add(&mut self, token: Token) {
        self.tokens.push(Sp {
            span: Span::new(self.start, self.loc),
            value: token,
        });
    }
    fn lex(mut self) -> Result<Vec<Sp<Token>>, LexError> {
        loop {
            self.start = self.loc;
            if let Some(c) = self.next() {
                match c {
                    '(' => self.add(Token::OpenParen),
                    ')' => self.add(Token::CloseParen),
                    '[' => self.add(Token::OpenBracket),
                    ']' => self.add(Token::CloseBracket),
                    '{' => self.add(Token::OpenCurly),
                    '}' => self.add(Token::CloseCurly),
                    ',' => self.add(Token::Comma),
                    '.' => self.add(Token::Dot),
                    ':' => self.add(Token::Colon),
                    '=' if self.next_is('=') => self.add(Token::BinOp(BinOp::Eq)),
                    '=' => self.add(Token::Equals),
                    '!' if self.next_is('=') => self.add(Token::BinOp(BinOp::Ne)),
                    '<' if self.next_is('=') => self.add(Token::BinOp(BinOp::Le)),
                    '<' => self.add(Token::BinOp(BinOp::Lt)),
                    '>' if self.next_is('=') => self.add(Token::BinOp(BinOp::Ge)),
                    '>' => self.add(Token::BinOp(BinOp::Gt)),
                    '+' => self.add(Token::BinOp(BinOp::Add)),
                    '*' => self.add(Token::BinOp(BinOp::Mul)),
                    '/' => self.add(Token::BinOp(BinOp::Div)),
                    '%' => self.add(Token::BinOp(BinOp::Rem)),
                    '-' => {
                        if let Some(c) = self.next_if(|c| c.is_ascii_digit()) {
                            self.number(true, c);
                        } else {
                            self.add(Token::BinOp(BinOp::Sub));
                        }
                    }
                    // Numbers
                    c if c.is_ascii_digit() => self.number(false, c),
                    // Idents and keywords
                    c if c.is_ascii_alphabetic() || c == '_' || c as u32 > 127 => {
                        let mut ident = String::from(c);
                        while let Some(c) = self
                            .next_if(|c| c.is_ascii_alphanumeric() || c == '_' || c as u32 > 127)
                        {
                            ident.push(c);
                        }
                        self.add(Token::Ident(Rc::from(ident)));
                    }
                    c if c.is_whitespace() => {}
                    c => return Err(LexError::InvalidChar(c)),
                }
            } else {
                break;
            }
        }
        Ok(self.tokens)
    }
    fn number(&mut self, negative: bool, start: char) {
        let mut number = String::new();
        if negative {
            number.push('-');
        }
        number.push(start);
        // Whole part
        while let Some(c) = self.next_if(|c| c.is_ascii_digit()) {
            number.push(c);
        }
        // Decimal part
        let whole_end = self.loc;
        if self.next_is('.') {
            number.push('.');
            let mut has_decimal = false;
            while let Some(c) = self.next_if(|c| c.is_ascii_digit()) {
                number.push(c);
                has_decimal = true;
            }
            if !has_decimal {
                self.loc = whole_end;
            }
        }
        // Exponent part
        if self.next_is('e') || self.next_is('E') {
            number.push('e');
            if self.next_is('-') {
                number.push('-');
            }
            while let Some(c) = self.next_if(|c| c.is_ascii_digit()) {
                number.push(c);
            }
        }

        self.add(Token::Number(Rc::from(number)));
    }
}
