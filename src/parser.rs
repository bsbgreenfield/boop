#![allow(unused)]
use core::panic;
use std::{
    cell::LazyCell,
    collections::HashMap,
    error::Error,
    fmt::Debug,
    iter::Peekable,
    str::{Chars, SplitWhitespace},
    sync::LazyLock,
};

use crate::value::{ValData, ValType, Value};

fn generate_keyword_hash() -> HashMap<&'static str, Token> {
    let mut result = HashMap::new();
    result.insert("true", Token::TkTrue);
    result.insert("false", Token::TkFalse);
    result.insert("for", Token::TkFor);

    result
}

#[derive(Debug)]
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
    TkTrue,
    TkFalse,
    TkFor,
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

pub struct Parser<'a> {
    code_text: &'a str,
    code_iter: Peekable<Chars<'a>>,
    start: usize,
    end: usize,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            code_text: input,
            code_iter: input.chars().peekable(),
            start: 0,
            end: 0,
        }
    }

    fn get_num_from_chars(&mut self, first_char: char) -> Option<i32> {
        let mut result_string = String::from(first_char);
        while let Some(next_char) = self.code_iter.peek() {
            // if the next char is numeric, add it to the number
            // otherwise, break and evaluate the number
            match next_char {
                '0'..='9' => result_string.push(Self::next_char(self).unwrap()), // we can safely
                // unwrap here because we already peeked!
                _ => break,
            }
        }
        match result_string.parse::<i32>() {
            Ok(number) => return Some(number),
            Err(_) => return None,
        }
    }

    fn try_parse_num(&mut self, char: char) -> Option<i32> {
        match char {
            '0' => {
                if let Some(next_char) = self.code_iter.peek() {
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
                return Self::get_num_from_chars(self, char);
            }
            '-' => {
                if let Some(number) = Self::get_num_from_chars(self, char) {
                    let result: i32 = number * -1;
                    return Some(number * -1);
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn match_keyword(&mut self, ending: &str) -> Option<Token> {
        for i in 1..(ending.len()) {
            if Self::next_char(self) != ending.chars().nth(i) {
                return None;
            }
        }
        None
    }

    fn try_parse_keyword(&mut self, char: char) -> Option<Token> {
        match char {
            't' => {
                return Self::match_keyword(self, "true");
            }
            'f' => match self.code_iter.peek() {
                Some(next_char) => match next_char {
                    'a' => return Self::match_keyword(self, "false"),
                    'o' => return Self::match_keyword(self, "for"),
                    _ => return None,
                },
                None => None,
            },
            _ => None,
        }
    }

    fn get_curr_slice(&self) -> &str {
        &self.code_text[self.start..self.end]
    }

    fn next_char(&mut self) -> Option<char> {
        // return next and also increment the end index
        if let Some(char) = self.code_iter.next() {
            self.end += 1;
            Some(char)
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) -> Option<char> {
        while let Some(char) = self.code_iter.next() {
            if char == ' ' {
                // while searching for the next item, increment end and start
                self.start += 1;
                continue;
            } else {
                self.end = self.start + 1;
                return Some(char);
            }
        }
        return None;
    }

    pub fn parse_next(&mut self) -> Option<Token> {
        use Token::*;

        self.start = self.end;
        // call code_iter.next() until either we get a character or we get None
        let maybe_next_char: Option<char> = Self::skip_whitespace(self);
        // if we get a character, check if its a number, then check if its something else
        if let Some(char) = maybe_next_char {
            if let Some(number) = Self::try_parse_num(self, char) {
                println!("Current slice is {}..{}", self.start, self.end);
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
                println!("Current slice is {}..{}", self.start, self.end);
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
