use interpreter::{lexer::Lexer, parser::Parser};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let s = String::from("let messi = --1;");
    let l = Lexer::new(&s).into_iter();
    let mut parser = Parser::new(l);
    parser.parse_ast()?;
    for statement in parser.statements {
        println!("{:?}", statement);
    }
    Ok(())
}
