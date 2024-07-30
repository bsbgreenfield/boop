#![allow(unused)]
use core::panic;
use std::{
    error::Error,
    fmt::Debug,
    iter::Peekable,
    str::{Chars, SplitWhitespace},
};

use crate::value::{ValData, ValType, Value};

pub enum Token {
    TkNum(Value),
    TkEquals,
    TkPlus,
    TkMinus,
    TkStar,
    TkSlash,
    TkSemicolon,
    TkOpenParen,
    TkCloseParen,
    TkEof,
    TkErr,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::TkNum(val), Self::TkNum(other_val)) => val == other_val,
            (Self::TkEquals, Self::TkEquals)
            | (Self::TkPlus, Self::TkPlus)
            | (Self::TkMinus, Self::TkMinus)
            | (Self::TkStar, Self::TkStar)
            | (Self::TkSlash, Self::TkSlash)
            | (Self::TkSemicolon, Self::TkSemicolon)
            | (Self::TkOpenParen, Self::TkOpenParen)
            | (Self::TkCloseParen, Self::TkCloseParen)
            | (Self::TkEof, Self::TkEof)
            | (Self::TkErr, Self::TkErr) => true,
            _ => false,
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::TkNum(val) => f.debug_struct("TkNum").field("val", &val.data).finish(),
            Token::TkEquals => write!(f, "TkEquals"),
            Token::TkPlus => write!(f, "TKPlus"),
            Token::TkMinus => write!(f, "TkMInus"),
            Token::TkStar => write!(f, "TkStar"),
            Token::TkSlash => write!(f, "TkSlash"),
            Token::TkSemicolon => write!(f, "TkSemicolon"),
            Token::TkOpenParen => write!(f, "TkOpenParen"),
            Token::TkCloseParen => write!(f, "TkCloseParen"),
            Token::TkEof => write!(f, "TkEof"),
            Token::TkErr => write!(f, "TkErr"),
        }
    }
}

fn get_num_from_chars(first_char: char, char_iter: &mut Peekable<Chars>) -> Option<i32> {
    let mut result_string = String::from(first_char);
    while let Some(next_char) = char_iter.peek() {
        // if the next char is numeric, add it to the number
        // otherwise, break and evaluate the number
        match next_char {
            '0'..='9' => result_string.push(char_iter.next().unwrap()),
            _ => break,
        }
    }
    match result_string.parse::<i32>() {
        Ok(number) => return Some(number),
        Err(_) => return None,
    }
}

fn try_parse_num(char: char, char_iter: &mut Peekable<Chars>) -> Option<i32> {
    match char {
        '0' => {
            if let Some(next_char) = char_iter.peek() {
                match next_char {
                    ' ' => Some(0),
                    '0'..='9' => panic!("remove the leading 0 in the number"),
                    _ => panic!("unexpected token {}", next_char),
                }
            } else {
                None
            }
        }
        '1'..='9' => {
            return get_num_from_chars(char, char_iter);
        }
        '-' => {
            if let Some(number) = get_num_from_chars(char, char_iter) {
                let result: i32 = number * -1;
                return Some(number * -1);
            } else {
                return None;
            }
        }
        _ => None,
    }
}

fn match_keyword(ending: &str, char_iter: &mut Peekable<Chars>) -> Option<Token> {
    for i in 1..(ending.len()) {
        if char_iter.next() != ending.chars().nth(i) {
            return None;
        }
    }
    None
}

fn try_parse_keyword(char: char, char_iter: &mut Peekable<Chars>) -> Option<Token> {
    match char {
        't' => {
            return match_keyword("true", char_iter);
        }
        'f' => match char_iter.peek() {
            Some(next_char) => match next_char {
                'a' => return match_keyword("false", char_iter),
                'o' => return match_keyword("for", char_iter),
                _ => return None,
            },
            None => None,
        },
        _ => None,
    }
}

fn skip_whitespace(iter: &mut Peekable<Chars>) -> Option<char> {
    while let Some(char) = iter.next() {
        if char == ' ' {
            continue;
        } else {
            return Some(char);
        }
    }
    return None;
}

pub struct Parser<'a> {
    code_text: &'a str,
    code_iter: Peekable<Chars<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            code_text: input,
            code_iter: input.chars().peekable(),
        }
    }
    pub fn parse_next(&mut self) -> Option<Token> {
        use Token::*;
        // call code_iter.next() until either we get a character or we get None
        let maybe_next_char: Option<char> = skip_whitespace(&mut self.code_iter);
        // if we get a character, check if its a number, then check if its something else
        if let Some(char) = maybe_next_char {
            if let Some(number) = try_parse_num(char, &mut self.code_iter) {
                println!("Token: {}", number);
                return Some(TkNum(Value::from_num(number)));
            } else {
                let token = match char {
                    '+' => TkPlus,
                    '-' => TkMinus,
                    ';' => TkSemicolon,
                    '=' => TkEquals,
                    '*' => TkStar,
                    '/' => TkSlash,
                    '(' => TkOpenParen,
                    ')' => TkCloseParen,
                    _ => {
                        println!("tried to parse unknown symbol: {}", char);
                        TkErr
                    }
                };
                println!("Token: {:?}", token);
                return Some(token);
            }
        }
        return None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_everything(parser: &mut Parser, token_buffer: &mut Vec<Token>) {
        loop {
            match parser.parse_next() {
                Some(token) => token_buffer.push(token),
                None => break,
            }
        }
    }

    #[test]
    fn parse_a_single_number() {
        let mut token_buffer: Vec<Token> = Vec::new();
        let mut parser = Parser::new("1");
        parse_everything(&mut parser, &mut token_buffer);
        assert_eq!(&vec![Token::TkNum(Value::from_num(1))], &token_buffer);
    }

    #[test]
    fn parse_a_longer_number() {
        let mut token_buffer: Vec<Token> = Vec::new();
        let mut parser = Parser::new("12345");
        parse_everything(&mut parser, &mut token_buffer);

        assert_eq!(&vec![Token::TkNum(Value::from_num(12345))], &token_buffer);
    }

    #[test]
    fn parse_num_with_parens() {
        let mut token_buffer: Vec<Token> = Vec::new();
        let mut parser = Parser::new("(123)");
        parse_everything(&mut parser, &mut token_buffer);

        assert_eq!(
            &vec![
                Token::TkOpenParen,
                Token::TkNum(Value::from_num(123)),
                Token::TkCloseParen
            ],
            &token_buffer
        );
    }

    #[test]
    fn parse_an_arithmatic_expression() {
        let mut token_buffer: Vec<Token> = Vec::new();
        let mut parser = Parser::new("0 * (1 * 2 * (3 + 4))");
        parse_everything(&mut parser, &mut token_buffer);
        use Token::*;
        assert_eq!(
            &vec![
                TkNum(Value::from_num(0)),
                TkStar,
                TkOpenParen,
                TkNum(Value::from_num(1)),
                TkStar,
                TkNum(Value::from_num(2)),
                TkStar,
                TkOpenParen,
                TkNum(Value::from_num(3)),
                TkPlus,
                TkNum(Value::from_num(4)),
                TkCloseParen,
                TkCloseParen,
            ],
            &token_buffer,
        )
    }
}
