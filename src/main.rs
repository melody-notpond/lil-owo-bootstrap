use lil_owo_bootstrap::lexer::Lexer;
use lil_owo_bootstrap::parser;

fn main() {
    let parse = "[a b] c 2.0 (d e f (g h))";
    let lexer = Lexer::new("stdin", parse);
    for token in lexer {
        println!("{:?}", token);
    }

    println!("{:?}", parser::parse("stdin", parse));
}
