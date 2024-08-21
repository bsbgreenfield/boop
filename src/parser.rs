#![allow(unused)]
use core::panic;
use std::{
    cell::LazyCell,
    collections::{HashMap, HashSet},
    error::Error,
    fmt::Debug,
    iter::Peekable,
    rc::Rc,
    str::{Chars, SplitWhitespace},
    sync::LazyLock,
};

use crate::value::{ValData, ValType, Value};

fn generate_keyword_hash() -> HashMap<&'static str, Token> {
    let mut result = HashMap::new();
    result.insert("true", Token::TkTrue);
    result.insert("false", Token::TkFalse);
    result.insert("for", Token::TkFor);
    result.insert("and", Token::TkAnd);
    result.insert("or", Token::TkOr);
    result.insert("if", Token::TkIf);
    result.insert("else", Token::TkElse);
    result.insert("print", Token::TkPrint);

    result
}

static KEYWORD_HASH: LazyLock<HashMap<&str, Token>> = LazyLock::new(|| generate_keyword_hash());

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Token {
    TkNum,
    TkEquals,
    TkDoubleEquals,
    TkGreaterThan,
    TkLessThan,
    TkPlus,
    TkMinus,
    TkStar,
    TkSlash,
    TkSemicolon,
    TkOpenParen,
    TkCloseParen,
    TkOpenBracket,
    TkCloseBracket,
    TkTrue,
    TkFalse,
    TkFor,
    TkIf,
    TkElse,
    TkAnd,
    TkOr,
    TkString,
    TkIdentifier,
    TkPrint,
    TkEof,
    TkErr,
}

pub struct Parser<'a> {
    code_text: &'a str,
    code_iter: Peekable<Chars<'a>>,
    peeked_start: usize,
    peeked_end: usize,
    start: usize,
    end: usize,
    peeked: Option<Option<Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            code_text: input,
            code_iter: input.chars().peekable(),
            peeked_start: 0,
            peeked_end: 0,
            start: 0,
            end: 0,
            peeked: None,
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

    fn match_keyword(&mut self, keyword: &str, offset: usize) -> Option<Token> {
        for i in offset..(keyword.len()) {
            if self.code_iter.peek() != keyword.chars().nth(i).as_ref() {
                return None;
            }
            self.next_char();
        }
        return KEYWORD_HASH.get(keyword).copied();
    }

    fn try_parse_keyword(&mut self, char: char) -> Option<Token> {
        match char {
            'e' => return self.match_keyword("else", 1),
            't' => {
                return self.match_keyword("true", 1);
            }
            'f' => match self.next_char() {
                Some(next_char) => match next_char {
                    'a' => return self.match_keyword("false", 2),
                    'o' => return self.match_keyword("for", 2),
                    _ => return None,
                },
                None => None,
            },
            'a' => return self.match_keyword("and", 1),
            'o' => return self.match_keyword("or", 1),
            'p' => return self.match_keyword("print", 1),
            'i' => return self.match_keyword("if", 1),
            _ => None,
        }
    }

    pub fn get_curr_slice(&self) -> &str {
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
            if char::is_whitespace(char) {
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

    fn try_parse_string(&mut self) -> Option<Token> {
        while let Some(char) = self.next_char() {
            if char == '"' {
                return Some(Token::TkString);
            } else {
                continue;
            }
        }
        return None;
    }

    fn try_parse_identifier(&mut self) -> Option<Token> {
        while let Some(char) = self.code_iter.peek() {
            match char {
                '"' | '+' | '-' | '*' | '/' | '(' | ')' | ' ' | ';' => {
                    return Some(Token::TkIdentifier);
                }
                _ => self.next_char(),
            };
        }
        return None;
    }

    pub fn peek(&mut self) -> Option<Token> {
        if let Some(maybe_token) = self.peeked {
            return maybe_token;
        } else {
            // save the current slice indices
            let start = self.start;
            let end = self.end;
            self.peeked = Some(self.next_token());
            // store the new slice indices in peeked_start/ peeked_end
            self.peeked_start = self.start;
            self.peeked_end = self.end;

            // set start and end back to what they were before
            self.start = start;
            self.end = end;
            return self.peeked.unwrap();
        }
    }

    pub fn parse_next(&mut self) -> Option<Token> {
        let maybe_token = self.next_token();
        match maybe_token {
            Some(token) => match token {
                Token::TkIdentifier => println!("{:?}: {}", token, self.get_curr_slice()),
                _ => println!("{:?}", token),
            },

            None => println!("EOF"),
        }
        return maybe_token;
    }

    fn next_token(&mut self) -> Option<Token> {
        use Token::*;

        let mut return_token: Option<Token> = None;

        // if we have already peeked, return that value
        if let Some(peeked_token) = self.peeked.take() {
            self.start = self.peeked_start;
            self.end = self.peeked_end;
            return_token.insert(peeked_token?);
        } else {
            self.start = self.end;
            // call code_iter.next() until either we get a character or we get None
            let maybe_next_char: Option<char> = self.skip_whitespace();
            // if we get a character, check if its a number, then check if its something else
            if let Some(char) = maybe_next_char {
                if let Some(number) = self.try_parse_num(char) {
                    return_token.insert(TkNum);
                } else if let Some(token) = self.try_parse_keyword(char) {
                    return_token.insert(token);
                } else {
                    let some_token = match char {
                        '"' => self.try_parse_string(),
                        '+' => Some(TkPlus),
                        '-' => Some(TkMinus),
                        ';' => Some(TkSemicolon),
                        '=' => {
                            if let Some(maybe_equals) = self.code_iter.peek() {
                                match maybe_equals {
                                    '=' => {
                                        self.next_char();
                                        return Some(TkDoubleEquals);
                                    }
                                    _ => Some(TkEquals),
                                }
                            } else {
                                return None;
                            }
                        }
                        '>' => Some(TkGreaterThan),
                        '<' => Some(TkLessThan),
                        '*' => Some(TkStar),
                        '/' => Some(TkSlash),
                        '(' => Some(TkOpenParen),
                        '{' => Some(TkOpenBracket),
                        '}' => Some(TkCloseBracket),
                        ')' => Some(TkCloseParen),
                        ';' => Some(TkSemicolon),
                        _ => self.try_parse_identifier(),
                    };
                    return_token.insert(some_token?);
                }
            }
        }
        return return_token;
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;

    use super::*;
    use Token::*;
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
        assert_eq!(&vec![Token::TkNum], &token_buffer);
    }

    #[test]
    fn parse_a_longer_number() {
        let mut token_buffer: Vec<Token> = Vec::new();
        let mut parser = Parser::new("12345");
        parse_everything(&mut parser, &mut token_buffer);

        assert_eq!(&vec![Token::TkNum], &token_buffer);
    }

    #[test]
    fn parse_num_with_parens() {
        let mut token_buffer: Vec<Token> = Vec::new();
        let mut parser = Parser::new("(123)");
        parse_everything(&mut parser, &mut token_buffer);

        assert_eq!(
            &vec![Token::TkOpenParen, Token::TkNum, Token::TkCloseParen],
            &token_buffer
        );
    }

    #[test]
    fn parse_an_arithmatic_expression() {
        let mut token_buffer: Vec<Token> = Vec::new();
        let mut parser = Parser::new("0 * (1 * 2 * (3 + 4))");
        parse_everything(&mut parser, &mut token_buffer);
        assert_eq!(
            &vec![
                TkNum,
                TkStar,
                TkOpenParen,
                TkNum,
                TkStar,
                TkNum,
                TkStar,
                TkOpenParen,
                TkNum,
                TkPlus,
                TkNum,
                TkCloseParen,
                TkCloseParen,
            ],
            &token_buffer,
        )
    }

    #[test]
    fn parse_bool() {
        let mut token_buffer: Vec<Token> = Vec::new();
        let mut parser = Parser::new("true false 123");
        parse_everything(&mut parser, &mut token_buffer);
        assert_eq!(&vec![TkTrue, TkFalse, TkNum], &token_buffer,);
    }

    #[test]
    fn parse_boolean_operation() {
        let mut token_buffer: Vec<Token> = Vec::new();
        let mut parser = Parser::new("true and false or false");
        parse_everything(&mut parser, &mut token_buffer);
        assert_eq!(&vec![TkTrue, TkAnd, TkFalse, TkOr, TkFalse,], &token_buffer,);
    }

    #[test]
    fn parse_a_string_literal() {
        let mut token_buffer: Vec<Token> = Vec::new();
        let mut my_string = String::from('"');
        my_string.push_str("hello");
        my_string.write_char('"').unwrap();
        let mut parser = Parser::new(&my_string);
        parse_everything(&mut parser, &mut token_buffer);
        assert_eq!(&vec![TkString], &token_buffer,);
    }

    #[test]
    fn parse_string_concat() {
        let mut token_buffer: Vec<Token> = Vec::new();
        let mut my_string_1 = String::from('"');
        my_string_1.push_str("hello");
        my_string_1.write_char('"').unwrap();
        let mut my_string_2 = String::from('"');
        my_string_2.push_str("world");
        my_string_2.write_char('"').unwrap();
        let my_string_3 = String::from(" + ");
        my_string_1.push_str(&my_string_3);
        my_string_1.push_str(&my_string_2);
        let mut parser = Parser::new(&my_string_1);
        parse_everything(&mut parser, &mut token_buffer);

        assert_eq!(&vec![TkString, TkPlus, TkString], &token_buffer,);
    }

    #[test]
    fn peek_a_token() {
        let mut token_buffer: Vec<Token> = Vec::new();
        let mut parser = Parser::new("1 + 2");
        let maybe_peeked = parser.peek();
        assert_eq!(maybe_peeked, Some(TkNum)); // should be token for 1
        assert_eq!(parser.peeked, Some(Some(TkNum)));
        let peeked_again = parser.peek();
        assert_eq!(peeked_again, Some(TkNum)); // should still be token for 1
        let maybe_next = parser.parse_next();
        assert_eq!(maybe_next, Some(TkNum)); // should have returned peeked value
        assert_eq!(parser.peeked, None); // peeked should now be none
        let maybe_operator = parser.parse_next();
        assert_eq!(maybe_operator, Some(TkPlus));
        let maybe_peeked_last = parser.peek();
        assert_eq!(maybe_peeked_last, Some(TkNum));
    }
}
