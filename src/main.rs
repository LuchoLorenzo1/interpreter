use interpreter::lexer::Lexer;

fn main() {
    let s = String::from("let messi = 1234-1234;");
    let l = Lexer::new(&s).into_iter();

    for i in l.into_iter() {
        println!("{:?}", i);
    }
}
