use super::cfg::*;

use super::parser_tests::parse_string;

#[test]
fn basic_cfg_printed() -> Result<(), String> {
    let source = r"init(50, 50, 50, 50);
translation(10, 0);
iter {
  {
    translation(10, 0)
  } or {
    rotation(0, 0, 90)
  }
}";
    let ctx = parse_string(source)?;
    let cfg = Cfg::new(&ctx);
    let pretty_printed = print(&cfg, &ctx);
    let expected = r#"digraph CFG {
  Node_0[label="init(50, 50, 50, 50)\ntranslation(10, 0)"]
  Node_1[label=""]
  Node_2[label="translation(10, 0)"]
  Node_3[label="rotation(0, 0, 90)"]
  Node_4[label=""]
  Node_5[label=""]

  Node_0 -> Node_1
  Node_1 -> Node_2
  Node_1 -> Node_3
  Node_2 -> Node_4
  Node_3 -> Node_4
  Node_4 -> Node_1
  Node_4 -> Node_5
}
"#;
    assert_eq!(expected, pretty_printed);

    Ok(())
}

#[test]
fn reverse_cfg_printed() -> Result<(), String> {
    let source = r"init(50, 50, 50, 50);
translation(10, 0);
iter {
  {
    translation(10, 0)
  } or {
    rotation(0, 0, 90)
  }
}";
    let ctx = parse_string(source)?;
    let cfg = Cfg::new(&ctx);
    let reverse_cfg = reverse(&cfg);
    let pretty_printed = print(&reverse_cfg, &ctx);
    let expected = r#"digraph CFG {
  Node_0[label=""]
  Node_1[label=""]
  Node_2[label="rotation(0, 0, 90)"]
  Node_3[label="translation(10, 0)"]
  Node_4[label=""]
  Node_5[label="translation(10, 0)\ninit(50, 50, 50, 50)"]

  Node_0 -> Node_1
  Node_1 -> Node_3
  Node_1 -> Node_2
  Node_2 -> Node_4
  Node_3 -> Node_4
  Node_4 -> Node_5
  Node_4 -> Node_1
}
"#;
    assert_eq!(expected, pretty_printed);

    Ok(())
}

#[test]
fn more_nested_cfg_printed() -> Result<(), String> {
    let source = r"init(50, 50, 50, 50);
translation(10, 0);
iter {
  iter {
    translation(10, 0)
  };
  {
    translation(10, 0)
  } or {
    {
      translation(10, 0)
    } or {
      iter {
        rotation(0, 0, 90)
      }
    }
  }
}";
    let ctx = parse_string(source)?;
    let cfg = Cfg::new(&ctx);
    let pretty_printed = print(&cfg, &ctx);
    let expected = r#"digraph CFG {
  Node_0[label="init(50, 50, 50, 50)\ntranslation(10, 0)"]
  Node_1[label=""]
  Node_2[label="translation(10, 0)"]
  Node_3[label=""]
  Node_4[label="translation(10, 0)"]
  Node_5[label=""]
  Node_6[label="translation(10, 0)"]
  Node_7[label=""]
  Node_8[label="rotation(0, 0, 90)"]
  Node_9[label=""]
  Node_10[label=""]
  Node_11[label=""]
  Node_12[label=""]

  Node_0 -> Node_1
  Node_1 -> Node_2
  Node_2 -> Node_2
  Node_2 -> Node_3
  Node_3 -> Node_4
  Node_3 -> Node_5
  Node_4 -> Node_11
  Node_5 -> Node_6
  Node_5 -> Node_7
  Node_6 -> Node_10
  Node_7 -> Node_8
  Node_8 -> Node_8
  Node_8 -> Node_9
  Node_9 -> Node_10
  Node_10 -> Node_11
  Node_11 -> Node_1
  Node_11 -> Node_12
}
"#;
    assert_eq!(expected, pretty_printed);

    Ok(())
}
