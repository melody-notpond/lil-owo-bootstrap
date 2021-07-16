#[derive(Copy, Clone, Debug)]
pub enum Token<'a> {
    Err,
    LParen,
    RParen,
    LBrack,
    RBrack,
    Symbol(&'a str),
    Number(f64)
}

#[derive(Copy, Clone, Debug)]
pub struct TokenValue<'a> {
    pub value: Token<'a>,
    pub start: usize,
    pub end: usize
}

#[derive(Copy, Clone, Debug)]
pub struct LexerState {
    token_pos: usize,
    s_index: usize
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub filename: String,
    pub s: &'a str,
    tokens: Vec<(usize, TokenValue<'a>)>,
    state: LexerState,
}

impl<'a> Lexer<'a> {
    pub fn new(filename: &str, s: &'a str) -> Lexer<'a> {
        Lexer {
            filename: String::from(filename),
            s,
            tokens: vec![],
            state: LexerState {
                s_index: 0,
                token_pos: 0
            }
        }
    }

    pub fn push_state(&self) -> LexerState {
        self.state
    }

    pub fn pop_state(&mut self, state: LexerState) {
        self.state = state;
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = TokenValue<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.state.token_pos < self.tokens.len() {
            let (i, v) = self.tokens[self.state.token_pos];
            self.state.token_pos += 1;
            self.state.s_index = i;
            Some(v)
        } else {
            let mut start = self.state.s_index;
            let mut end = self.s.len();
            for (i, b) in self.s.bytes().enumerate().skip(start) {
                if b == b' ' || b == b'\t' || b == b'\n' || b == b'\r' {
                    if i == start {
                        start = i + 1;
                    } else {
                        end = i;
                        break;
                    }
                } else if b == b'(' || b == b')' || b == b'[' || b == b']' {
                    end = i + if i == start { 1 } else { 0 };
                    break;
                }
            }

            let token = if start != end {
                let bytes = self.s.as_bytes();
                if b'0' <= bytes[start] && bytes[start] <= b'9' {
                    if let Ok(v) = self.s[start..end].parse() {
                        Some(TokenValue {
                            value: Token::Number(v),
                            start, end
                        })
                    } else {
                        Some(TokenValue {
                            value: Token::Err,
                            start, end
                        })
                    }
                } else if end - start == 1 {
                    Some(TokenValue {
                        value: match self.s.as_bytes()[start] {
                            b'(' => Token::LParen,
                            b')' => Token::RParen,
                            b'[' => Token::LBrack,
                            b']' => Token::RBrack,
                            _ => Token::Symbol(&self.s[start..end])
                        },
                        start, end
                    })
                } else {
                    Some(TokenValue {
                        value: Token::Symbol(&self.s[start..end]),
                        start, end
                    })
                }
            } else {
                None
            };

            if let Some(token) = token {
                self.tokens.push((end, token));
                self.state.token_pos += 1;
            }
            self.state.s_index = end;

            token
        }
    }
}

