#![allow(dead_code, unused_variables)]

#[derive(PartialEq, Debug)]
enum Keyword {
    Let,
    Fn,
    Return,
}

#[derive(PartialEq, Debug)]
enum Token {
    Identifier,
    Keyword(Keyword),
    Integer(u32),
    Illegal,
}

struct Lexer {
    string: String,
    curr_index: usize,
}

impl Lexer {
    fn new(string: String) -> Lexer {
        return Lexer { string, curr_index: 0 };
    }

    fn next_token(&self) -> Token {
        if self.curr_index >= self.string.len() {
            return Token::Illegal;
        }

        Token::Keyword(Keyword::Let)
    }
}

fn main() {
    let s = String::from("let a = 0;");
    let lexer = Lexer::new(s);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexing_basic_text() {
        let s = String::from("let a = 0;");
        let lexer = Lexer::new(s);
        let token: Token = lexer.next_token();
        assert_eq!(token, Token::Keyword(Keyword::Let))
    }
}
