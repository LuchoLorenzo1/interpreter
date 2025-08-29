#![allow(dead_code, unused_variables)]

use std::{iter::Peekable, str::Chars};

#[derive(PartialEq, Debug)]
pub enum Keyword {
    Let,
    Fn,
    Return,
}

#[derive(PartialEq, Debug)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    Integer(u32),
    Illegal(char),
    EqualSign,
    DoubleEqualSign,
    NotEqualSign,
    PlusSign,
    MinusSign,
    Comma,
    Semicolon,
    OpenParenthesis,
    CloseParenthesis,
    OpenBrace,
    CloseBrace,
    Quote,
    NotSign,
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(string: &'a str) -> Self {
        return Lexer {
            chars: string.chars().peekable(),
        };
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let char = match self.chars.next() {
            Some(c) => c,
            None => return None,
        };

        Some(match char {
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '+' => Token::PlusSign,
            '-' => Token::MinusSign,
            '(' => Token::OpenParenthesis,
            ')' => Token::CloseParenthesis,
            '{' => Token::OpenBrace,
            '}' => Token::CloseBrace,
            '"' => Token::Quote,
            '!' => {
                if let Some('=') = self.chars.peek() {
                    self.chars.next();
                    Token::NotEqualSign
                } else {
                    Token::NotSign
                }
            }
            '=' => {
                if let Some('=') = self.chars.peek() {
                    self.chars.next();
                    Token::DoubleEqualSign
                } else {
                    Token::EqualSign
                }
            }
            ' ' | '\t' => {
                while let Some(i) = self.chars.peek() {
                    match i {
                        ' ' | '\t' => {
                            self.chars.next();
                        }
                        _ => break,
                    }
                }
                if let None = self.chars.peek() {
                    return None;
                }
                self.next()?
            }
            '0'..'9' => {
                let mut number: String = String::from(char);
                while let Some(i) = self.chars.peek() {
                    match i {
                        '0'..'9' => {
                            number.push(*i);
                            self.chars.next();
                        }
                        _ => break,
                    }
                }
                Token::Integer(u32::from_str_radix(&number, 10).unwrap_or(0))
            }
            'a'..='z' | 'A'..='Z' => {
                let mut word: String = String::from(char);
                while let Some(i) = self.chars.peek() {
                    match i {
                        'a'..='z' | 'A'..='Z' => {
                            word.push(*i);
                            self.chars.next();
                        }
                        _ => break,
                    }
                }

                match word.as_str() {
                    "let" => Token::Keyword(Keyword::Let),
                    "return" => Token::Keyword(Keyword::Return),
                    "fn" => Token::Keyword(Keyword::Fn),
                    _ => Token::Identifier(word),
                }
            }
            _ => Token::Illegal(char),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexing_signs() {
        let s = String::from("+ = ; , -");
        let mut l = Lexer::new(&s);
        assert_eq!(l.next().unwrap(), Token::PlusSign);
        assert_eq!(l.next().unwrap(), Token::EqualSign);
        assert_eq!(l.next().unwrap(), Token::Semicolon);
        assert_eq!(l.next().unwrap(), Token::Comma);
        assert_eq!(l.next().unwrap(), Token::MinusSign);
        assert_eq!(l.next(), None);
    }

    #[test]
    fn lexing_numbers() {
        let s = String::from("1 2; 3; 44,,558");
        let mut l = Lexer::new(&s);
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::Integer(2));
        assert_eq!(l.next().unwrap(), Token::Semicolon);
        assert_eq!(l.next().unwrap(), Token::Integer(3));
        assert_eq!(l.next().unwrap(), Token::Semicolon);
        assert_eq!(l.next().unwrap(), Token::Integer(44));
        assert_eq!(l.next().unwrap(), Token::Comma);
        assert_eq!(l.next().unwrap(), Token::Comma);
        assert_eq!(l.next().unwrap(), Token::Integer(558));
        assert_eq!(l.next(), None);
        assert_eq!(l.next(), None);
    }

    #[test]
    fn multiple_whitespaces() {
        let s = String::from("1 \t\t  2   \t\t  55");
        let mut l = Lexer::new(&s);
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::Integer(2));
        assert_eq!(l.next().unwrap(), Token::Integer(55));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn lexing_basic_statement() {
        let s = String::from("let a = 71-1+1;");
        let mut l = Lexer::new(&s);
        assert_eq!(l.next().unwrap(), Token::Keyword(Keyword::Let));
        assert_eq!(l.next().unwrap(), Token::Identifier(String::from("a")));
        assert_eq!(l.next().unwrap(), Token::EqualSign);
        assert_eq!(l.next().unwrap(), Token::Integer(71));
        assert_eq!(l.next().unwrap(), Token::MinusSign);
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::PlusSign);
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::Semicolon);
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_illegal_chars() {
        let s = String::from("1?2%3$");
        let mut l = Lexer::new(&s).into_iter();
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::Illegal('?'));
        assert_eq!(l.next().unwrap(), Token::Integer(2));
        assert_eq!(l.next().unwrap(), Token::Illegal('%'));
        assert_eq!(l.next().unwrap(), Token::Integer(3));
        assert_eq!(l.next().unwrap(), Token::Illegal('$'));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_string() {
        let s = String::from("let xyz = \"abcdefghijklmnopqrstuvwxyz\";");
        let mut l = Lexer::new(&s).into_iter();
        assert_eq!(l.next().unwrap(), Token::Keyword(Keyword::Let));
        assert_eq!(l.next().unwrap(), Token::Identifier(String::from("xyz")));
        assert_eq!(l.next().unwrap(), Token::EqualSign);
        assert_eq!(l.next().unwrap(), Token::Quote);
        assert_eq!(
            l.next().unwrap(),
            Token::Identifier(String::from("abcdefghijklmnopqrstuvwxyz"))
        );
        assert_eq!(l.next().unwrap(), Token::Quote);
        assert_eq!(l.next().unwrap(), Token::Semicolon);
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_double_equals() {
        let s = String::from("1 == 1;");
        let mut l = Lexer::new(&s).into_iter();
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::DoubleEqualSign);
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::Semicolon);
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_unequal() {
        let s = String::from("1 != 1; !===!=");
        let mut l = Lexer::new(&s).into_iter();
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::NotEqualSign);
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::Semicolon);
        assert_eq!(l.next().unwrap(), Token::NotEqualSign);
        assert_eq!(l.next().unwrap(), Token::DoubleEqualSign);
        assert_eq!(l.next().unwrap(), Token::NotEqualSign);
        assert_eq!(l.next(), None);
    }
}
