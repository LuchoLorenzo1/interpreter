use clap::Parser;
use interpreter::char_reader::CharReader;
use interpreter::repl;

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

    repl::repl()
}
