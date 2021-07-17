use lil_owo_bootstrap::parser;
use lil_owo_bootstrap::ir;

fn main() {
    let parse = "
        (func factorial (n)
            (begin
                ((func factorial-tail (n acc) 
                    (cond ((<= n 1) n)
                        (else (factorial-tail (- n 1) (* acc n)))))
                    (factorial-tail n 1))
            end))
    ";
    println!("{}", parse);
    let ast = parser::parse("stdin", parse).unwrap();
    let root = ir::ast_to_ir(ast).unwrap();
    println!("{}", root);
}
