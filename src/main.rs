use std::io::Write;

use clap::Parser;

#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Filename to execute
    #[arg(value_name = "FILENAME", index = 1)]
    filename: Option<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    if let Some(f) = args.filename {
        let str = std::fs::read_to_string(f)?;
        let lexer = interpreter::lexer::Lexer::new(&str);
        let mut parser = interpreter::parser::Parser::new(lexer);
        parser.parse_ast()?;
        println!("{:?}", parser.statements);
        return Ok(());
    }

    loop {
        print!(">> ");
        std::io::stdout().flush().unwrap();

        let mut line = String::new();
        if std::io::stdin().read_line(&mut line).is_err() {
            break;
        }

        let line = line.trim_end();
        if line.trim() == "exit" {
            break;
        }

        let lexer = interpreter::lexer::Lexer::new(line);
        let mut parser = interpreter::parser::Parser::new(lexer);

        match parser.parse_ast() {
            Ok(_) => println!("{:?}", parser.statements),
            Err(e) => eprintln!("Error: {}", e),
        }
    }

    Ok(())
}
