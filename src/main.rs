#![allow(dead_code, unused_variables)]

#[derive(PartialEq, Debug)]
enum Keyword {
    Let,
    Fn,
    Return,
}

#[derive(PartialEq, Debug)]
enum Token {
    Identifier(String),
    Keyword(Keyword),
    Integer(usize),
    Illegal,
    EqualSign,
    PlusSign,
    MinusSign,
    Comma,
    Semicolon,
    EOF,
}

struct Lexer {
    string: String,
    curr_index: usize,
    curr_reading: usize,
}

impl Lexer {
    fn new(string: String) -> Lexer {
        return Lexer {
            string,
            curr_index: 0,
            curr_reading: 0,
        };
    }

    fn next_token(&mut self) -> Token {
        if self.curr_index >= self.string.len() {
            return Token::EOF;
        }

        let mut chars = self.string.chars();
        let char = chars.nth(self.curr_index).unwrap();

        self.curr_index += 1;
        let token = match char {
            ',' => Token::Comma,
            '=' => Token::EqualSign,
            ';' => Token::Semicolon,
            '+' => Token::PlusSign,
            '-' => Token::MinusSign,
            ' ' => {
                while let Some(i) = chars.next() {
                    match i {
                        ' ' => self.curr_index += 1,
                        _ => break,
                    }
                }
                let t = self.next_token();
                t
            }
            '0'..'9' => {
                let mut number: String = String::from(char);
                while let Some(i) = chars.next() {
                    match i {
                        '0'..'9' => {
                            number.push(i);
                            self.curr_index += 1;
                        }
                        _ => break,
                    }
                }
                Token::Integer(usize::from_str_radix(&number, 10).unwrap_or(0))
            }
            _ => Token::Illegal,
        };
        token
    }
}

fn main() {
    let s = String::from("let a = 0;");
    let mut lexer = Lexer::new(s);
    let token: Token = lexer.next_token();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexing_signs() {
        let s = String::from("+ = ; , -");
        let mut lexer = Lexer::new(s);
        let mut token: Token = lexer.next_token();
        assert_eq!(token, Token::PlusSign);

        token = lexer.next_token();
        assert_eq!(token, Token::EqualSign);

        token = lexer.next_token();
        assert_eq!(token, Token::Semicolon);

        token = lexer.next_token();
        assert_eq!(token, Token::Comma);

        token = lexer.next_token();
        assert_eq!(token, Token::MinusSign);
    }

    #[test]
    fn lexing_numbers() {
        let s = String::from("1 2; 3; 44,,558");
        let mut lexer = Lexer::new(s);
        let mut token: Token = lexer.next_token();
        assert_eq!(token, Token::Integer(1));

        token = lexer.next_token();
        assert_eq!(token, Token::Integer(2));

        token = lexer.next_token();
        assert_eq!(token, Token::Semicolon);

        token = lexer.next_token();
        assert_eq!(token, Token::Integer(3));

        token = lexer.next_token();
        assert_eq!(token, Token::Semicolon);

        token = lexer.next_token();
        assert_eq!(token, Token::Integer(44));

        let comma1 = lexer.next_token();
        let comma2 = lexer.next_token();
        assert_eq!(comma1, Token::Comma);
        assert_eq!(comma2, Token::Comma);

        token = lexer.next_token();
        assert_eq!(token, Token::Integer(558));
    }

    #[test]
    fn lexing_basic_text() {
        let s = String::from("let a = 0;");
        let mut lexer = Lexer::new(s);
        let mut token: Token = lexer.next_token();
        assert_eq!(token, Token::Keyword(Keyword::Let));

        token = lexer.next_token();
        assert_eq!(token, Token::Identifier(String::from("a")));
    }
}
