use interpreter::{lexer::Lexer, parser::Parser};

fn main() {
    let s = String::from("1?2!3$");
    let l = Lexer::new(&s).into_iter();
    let mut parser = Parser::new(l);

    for statement in parser.statements {
        println!("{:?}", statement);
    }
}
