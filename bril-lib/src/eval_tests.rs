use super::eval::*;
use super::lexer::*;
use super::parser::*;
use utils::DiagnosticEmitter;

fn eval_string(source: &str, args: &[Value]) -> Result<(Value, String), String> {
    let mut diag = DiagnosticEmitter::new(Box::new(Vec::new()), Box::new(Vec::new()));
    let lexer = Lexer::new(source, &mut diag);
    let tokens = lexer.lex_all();
    if tokens.tokens.is_empty() {
        return Err(diag.out_buffer().to_string() + diag.err_buffer());
    }
    let parser = Parser::new(tokens, &mut diag);
    let Some(unit) = parser.parse()
    else {
        return Err(diag.out_buffer().to_string() + diag.err_buffer());
    };
    let mut interp = Interpreter::new(&unit, &mut diag);
    let Some(result) = interp.eval_main(args)
    else {
        return Err(diag.out_buffer().to_string() + diag.err_buffer());
    };
    Ok((result, diag.out_buffer().to_string()))
}

#[test]
fn eval_basic_operations() -> Result<(), String> {
    let source = r"@main(): int { 
  x: int = const 5;
  nop;
  print x;
  ret x;
}";
    let (result, out) = eval_string(source, &[])?;
    assert_eq!(result, Value::I(5));
    assert_eq!(out, "5\n");

    let source = r"@main(): int { 
  x: int = const 5;
  y: int = id x;
  x: int = add x x;
  print x;
  x: int = mul x y;
  print x;
  x: int = sub x y;
  print x;
  x: int = div x y;
  print x;
  b: bool = eq x y;
  print b;
  b: bool = gt x y;
  print b;
  b: bool = not b;
  print b;
  t: bool = const true;
  b: bool = or b t;
  print b;
  ret x;
}";
    let (_, out) = eval_string(source, &[])?;
    assert_eq!(out, "10\n50\n45\n9\nfalse\ntrue\nfalse\ntrue\n");

    Ok(())
}

#[test]
fn eval_test_control_flow() -> Result<(), String> {
    let source = r"@main(x: int): int { 
  res: int = const 1;
  i: int = const 0;
  jmp .test;

.test:
  cond: bool = lt i x;
  br cond .loop .done;

.loop:
  one: int = const 1;
  i: int = add i one;
  res: int = mul res i;
  jmp .test;

.done:
  ret res;
}";
    let (result, _) = eval_string(source, &[Value::I(5)])?;
    assert_eq!(result, Value::I(120));

    Ok(())
}