use std::{collections::HashMap, hash::Hash};

use crate::lexer::Token;

pub struct NumPair {
    pub x: Token,
    pub y: Token,
}

pub struct Init {
    pub kw: Token,
    pub bottom_left: NumPair,
    pub size: NumPair,
}

pub struct Translation {
    pub kw: Token,
    pub vector: NumPair,
}

pub struct Rotation {
    pub kw: Token,
    pub origin: NumPair,
    pub deg: Token,
}

pub struct Sequence {
    pub nodes: Vec<Node>,
}

pub struct Branch {
    pub kw: Token,
    pub lhs: Node, // Must be a Sequence(u32)
    pub rhs: Node, // Must be a Sequence(u32)
}

pub struct Loop {
    pub kw: Token,
    pub body: Node, // Must be a Sequence(u32)
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Node {
    Init(u32),
    Translation(u32),
    Rotation(u32),
    Sequence(u32),
    Branch(u32),
    Loop(u32),
}

pub enum NodeRef<'a> {
    Init(&'a Init),
    Translation(&'a Translation),
    Rotation(&'a Rotation),
    Sequence(&'a Sequence),
    Branch(&'a Branch),
    Loop(&'a Loop),
}

pub struct ASTContext {
    inits: Vec<Init>,
    translations: Vec<Translation>,
    rotations: Vec<Rotation>,
    sequences: Vec<Sequence>,
    branches: Vec<Branch>,
    loops: Vec<Loop>,
}

impl<'a> ASTContext {
    pub fn new() -> Self {
        Self {
            inits: Vec::new(),
            translations: Vec::new(),
            rotations: Vec::new(),
            sequences: Vec::new(),
            branches: Vec::new(),
            loops: Vec::new(),
        }
    }

    pub fn get_root(&self) -> Node {
        Node::Sequence((self.sequences.len() - 1) as u32)
    }

    pub fn make_init(&mut self, n: Init) -> Node {
        self.inits.push(n);
        Node::Init((self.inits.len() - 1) as u32)
    }

    pub fn make_translation(&mut self, n: Translation) -> Node {
        self.translations.push(n);
        Node::Translation((self.translations.len() - 1) as u32)
    }

    pub fn make_rotation(&mut self, n: Rotation) -> Node {
        self.rotations.push(n);
        Node::Rotation((self.rotations.len() - 1) as u32)
    }

    pub fn make_sequence(&mut self, n: Sequence) -> Node {
        self.sequences.push(n);
        Node::Sequence((self.sequences.len() - 1) as u32)
    }

    pub fn make_branch(&mut self, n: Branch) -> Node {
        assert!(matches!(n.lhs, Node::Sequence(_)));
        assert!(matches!(n.rhs, Node::Sequence(_)));
        self.branches.push(n);
        Node::Branch((self.branches.len() - 1) as u32)
    }

    pub fn make_loop(&mut self, n: Loop) -> Node {
        assert!(matches!(n.body, Node::Sequence(_)));
        self.loops.push(n);
        Node::Loop((self.loops.len() - 1) as u32)
    }

    pub fn node_to_ref(&self, n: Node) -> NodeRef {
        match n {
            Node::Init(id) => NodeRef::Init(&self.inits[id as usize]),
            Node::Translation(id) => NodeRef::Translation(&self.translations[id as usize]),
            Node::Rotation(id) => NodeRef::Rotation(&self.rotations[id as usize]),
            Node::Sequence(id) => NodeRef::Sequence(&self.sequences[id as usize]),
            Node::Branch(id) => NodeRef::Branch(&self.branches[id as usize]),
            Node::Loop(id) => NodeRef::Loop(&self.loops[id as usize]),
        }
    }
}

impl<'a> Default for ASTContext {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Annotations {
    pub pre_annotations: HashMap<Node, Vec<String>>,
    pub post_annotations: HashMap<Node, Vec<String>>,
}

impl Annotations {
    pub fn new() -> Self {
        Self {
            pre_annotations: HashMap::new(),
            post_annotations: HashMap::new(),
        }
    }
}

impl<'a> Default for Annotations {
    fn default() -> Self {
        Self::new()
    }
}

pub fn print(n: Node, ctx: &ASTContext, ann: &Annotations) -> String {
    print_impl(0, n, ctx, ann)
}

fn indent(indent: u32) -> String {
    str::repeat(" ", indent as usize)
}

fn print_impl(ind: u32, n: Node, ctx: &ASTContext, ann: &Annotations) -> String {
    let node_ref = ctx.node_to_ref(n);
    let mut result = "".to_owned();
    if !matches!(n, Node::Sequence(_)) {
        result.push_str(&indent(ind));
    }
    result.push_str(&render_pre_annotations(n, ann));
    match node_ref {
        NodeRef::Init(init) => {
            result.push_str(&format!(
                "init({}, {}, {}, {})",
                init.bottom_left.x, init.bottom_left.y, init.size.x, init.size.y
            ));
        }
        NodeRef::Translation(trans) => {
            result.push_str(&format!(
                "translation({}, {})",
                trans.vector.x, trans.vector.y
            ));
        }
        NodeRef::Rotation(rot) => {
            result.push_str(&format!(
                "rotation({}, {}, {})",
                rot.origin.x, rot.origin.y, rot.deg
            ));
        }
        NodeRef::Sequence(seq) => {
            let v: Vec<String> = seq
                .nodes
                .clone()
                .into_iter()
                .map(|n: Node| print_impl(ind, n, ctx, ann))
                .collect();
            result.push_str(&v.join(";\n"));
        }
        NodeRef::Branch(branch) => {
            result.push_str("{\n");
            result.push_str(&print_impl(ind + 2, branch.lhs, ctx, ann));
            result.push_str(&format!("\n{}}} or {{\n", &indent(ind)));
            result.push_str(&print_impl(ind + 2, branch.rhs, ctx, ann));
            result.push_str(&format!("\n{}}}", &indent(ind)));
        }
        NodeRef::Loop(l) => {
            result.push_str("iter {\n");
            result.push_str(&print_impl(ind + 2, l.body, ctx, ann));
            result.push_str(&format!("\n{}}}", &indent(ind)));
        }
    };
    result.push_str(&render_post_annotations(n, ann));
    result
}

// TODO: Unfortunately, it looks like I cannot specialize
// functions in Rust yet. Remove this workaround once that
// language feature is added.
struct IsPre<const B: bool>;

trait RenderAnnotations {
    fn render_annotations(n: Node, ann: &HashMap<Node, Vec<String>>) -> String;
}

impl<const PRE: bool> RenderAnnotations for IsPre<PRE> {
    fn render_annotations(n: Node, ann: &HashMap<Node, Vec<String>>) -> String {
        if let Some(annotations) = ann.get(&n) {
            if !annotations.is_empty() {
                let mut result = "".to_owned();
                result.push_str(if PRE { "" } else { " " });
                result.push_str("/* ");
                result.push_str(&annotations.join(" "));
                result.push_str(" */");
                result.push_str(if PRE { " " } else { "" });
                return result;
            }
        }
        "".to_owned()
    }
}

fn render_pre_annotations(n: Node, ann: &Annotations) -> String {
    IsPre::<true>::render_annotations(n, &ann.pre_annotations)
}

fn render_post_annotations(n: Node, ann: &Annotations) -> String {
    IsPre::<false>::render_annotations(n, &ann.post_annotations)
}
