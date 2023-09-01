use super::eval::*;
use super::lexer::*;
use super::parser::*;
use utils::DiagnosticEmitter;

fn eval_string(source: &str, args: &[Value]) -> Result<(Value, String), String> {
    let mut diag = DiagnosticEmitter::log_to_buffer();
    let lexer = Lexer::new(source, &mut diag);
    let tokens = lexer.lex_all();
    if tokens.tokens.is_empty() {
        return Err(diag.out_buffer().unwrap() + &diag.err_buffer().unwrap());
    }
    let parser = Parser::new(tokens, &mut diag);
    let Some(unit) = parser.parse() else {
        return Err(diag.out_buffer().unwrap() + &diag.err_buffer().unwrap());
    };
    let mut interp = Interpreter::new(&unit, &mut diag);
    let Some(result) = interp.eval_main(args) else {
        return Err(diag.out_buffer().unwrap() + &diag.err_buffer().unwrap());
    };
    Ok((result, diag.out_buffer().unwrap()))
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
  x: int = const 5;
  y: int = const 3;
  x: int = mod x y;
  print x;
  ret x;
}";
    let (_, out) = eval_string(source, &[])?;
    assert_eq!(out, "10\n50\n45\n9\nfalse\ntrue\nfalse\ntrue\n2\n");

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

#[test]
fn eval_function_call() -> Result<(), String> {
    let source = r"@fact(y: int): int {
  base: int = const 1;
  cond: bool = le y base;
  br cond .base .rec;

.base:
  res: int = const 1;
  ret res;

.rec:
  one: int = const 1;
  dec: int = sub y one;
  res: int = call @fact dec;
  res: int = mul y res;
  ret res;
}
    
@main(x: int): int { 
  res: int = call @fact x;
  ret res;
}";
    let (result, _) = eval_string(source, &[Value::I(5)])?;
    assert_eq!(result, Value::I(120));

    Ok(())
}

#[test]
fn eval_test_div_zero() {
    let source = r"@main { 
  x: int = const 0;
  x: int = div x x;
  ret;
}";
    assert_eq!(
        eval_string(source, &[]).expect_err("Div zero."),
        "[line 3] Error at 'div': Division by zero.\n"
    );
}
