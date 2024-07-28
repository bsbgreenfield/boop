#![allow(unused)]
use std::{error::Error, fmt::Debug, str::SplitWhitespace};

use crate::value::{ValData, ValType, Value};

pub enum Token {
    TkNum(Value),
    TkEquals,
    TkPlus,
    TkMinus,
    TkStar,
    TkSlash,
    TkSemicolon,
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
            Token::TkEof => write!(f, "TkEof"),
            Token::TkErr => write!(f, "TkErr"),
        }
    }
}

pub struct Parser<'a> {
    code_text: &'a str,
    code_iter: SplitWhitespace<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            code_text: input,
            code_iter: input.split_whitespace(),
        }
    }

    pub fn parse_next(&mut self) -> Option<Token> {
        use Token::*;
        let maybe_next_item = self.code_iter.next();
        if let Some(item) = maybe_next_item {
            // if there is a next item, try and parse a number from it.
            if let Some(num) = Parser::<'a>::try_parse_num(item) {
                return Some(TkNum(Value::new(ValType::ValNumType, ValData::ValNum(num))));
                // othewise, parse the token normally
            } else {
                let mut token = TkErr;
                for char in item.chars() {
                    token = match char {
                        '+' => TkPlus,
                        '-' => TkMinus,
                        ';' => TkSemicolon,
                        '=' => TkEquals,
                        '*' => TkStar,
                        '/' => TkSlash,
                        _ => TkEof,
                    }
                }
                return Some(token);
            }
        } else {
            return None;
        }
    }

    fn try_parse_num(item: &str) -> Option<i32> {
        match item.parse::<i32>() {
            Ok(num) => Some(num),
            _ => None,
        }
    }
}
