use std::io::Write;

use clap::Parser;
use interpreter::{char_reader::CharReader, parser::Statement};

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
        let f = std::fs::File::open(f)?;
        let reader = std::io::BufReader::new(f);

        let char_reader = CharReader::new(reader);
        let lexer = interpreter::lexer::Lexer::new(char_reader);
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

        let lexer = interpreter::lexer::Lexer::new_from_str(line);
        let mut parser = interpreter::parser::Parser::new(lexer);
        parser.parse_ast()?;

        // match parser.parse_ast() {
        //     Ok(_) => println!("{:?}", parser.statements),
        //     Err(e) => eprintln!("Error: {}", e),
        // }

        for s in parser.statements {
            match s {
                Statement::Expression(e) => println!("{:?}", e.exec()),
                Statement::Let(name, value) => println!("{name}={:?}", value.exec()),
                _ => {}
            };
        }
    }

    Ok(())
}
