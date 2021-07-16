use super::lexer::{Lexer, Token, TokenValue};

#[derive(Debug)]
pub enum Ast<'a> {
    Number(f64),
    Symbol(&'a str),
    SExpr(Box<Ast<'a>>, Vec<Ast<'a>>),
    List(Vec<Ast<'a>>)
}

#[derive(Debug)]
pub struct ParseError;

fn parse_value<'a>(lexer: &mut Lexer<'a>) -> Result<Ast<'a>, ParseError> {
    let state = lexer.push_state();

    let token = match lexer.next() {
        Some(TokenValue { value: Token::Err, start: _, end: _ }) => return Err(ParseError),
        Some(v) => v,
        None => return Err(ParseError)
    };

    match token.value {
        Token::Err => {
            lexer.pop_state(state);
            Err(ParseError)
        }

        Token::LParen => {
            let sexpr = match parse_sexpr(lexer) {
                Ok(v) => v,
                Err(e) => {
                    lexer.pop_state(state);
                    return Err(e);
                }
            };

            if let Some(TokenValue { value: Token::RParen, start: _, end: _ }) = lexer.next() {
                Ok(sexpr)
            } else {
                lexer.pop_state(state);
                Err(ParseError)
            }
        }

        Token::RParen => {
            lexer.pop_state(state);
            Err(ParseError)
        }

        Token::LBrack => {
            let mut asts = vec![];
            while let Ok(ast) = parse_value(lexer) {
                asts.push(ast);
            }

            if let Some(TokenValue { value: Token::RBrack, start: _, end: _ }) = lexer.next() {
                Ok(Ast::List(asts))
            } else {
                lexer.pop_state(state);
                Err(ParseError)
            }
        }

        Token::RBrack => {
            lexer.pop_state(state);
            Err(ParseError)
        }

        Token::Symbol(s) => Ok(Ast::Symbol(s)),
        Token::Number(v) => Ok(Ast::Number(v)),
    }
}

fn parse_sexpr<'a>(lexer: &mut Lexer<'a>) -> Result<Ast<'a>, ParseError> {
    let first = parse_value(lexer)?;
    let mut asts = vec![];

    while let Ok(ast) = parse_value(lexer) {
        asts.push(ast);
    }

    Ok(Ast::SExpr(Box::new(first), asts))
}

pub fn parse<'a>(filename: &str, contents: &'a str) -> Result<Ast<'a>, ParseError> {
    let mut lexer = Lexer::new(filename, contents);
    let result = parse_sexpr(&mut lexer)?;
    if lexer.next().is_some() {
        Err(ParseError)
    } else {
        Ok(result)
    }
}
