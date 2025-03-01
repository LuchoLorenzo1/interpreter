use interpreter::lexer::Lexer;

fn main() {
    let s = String::from("1?2!3$");
    let l = Lexer::new(&s).into_iter();

    for i in l.into_iter() {
        println!("{:?}", i);
    }
}
