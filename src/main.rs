use lil_owo_bootstrap::parser;
use lil_owo_bootstrap::ir;

fn main() {
    let parse = "
    (func test (x)
        (func testy (y)
            (func uwu (z)
                (+ x y z))))
    ";
    println!("{}", parse);
    let ast = parser::parse("stdin", parse).unwrap();
    let root = ir::ast_to_ir(ast).unwrap();
    println!("{}", root);
}
