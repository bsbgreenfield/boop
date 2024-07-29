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
                return Some(TkNum(Value::new(
                    ValType::ValNumType,
                    ValData::ValNum(number),
                )));
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
                return Some(token);
            }
        }
        return None;
    }
}
