use analysis::cfg::indent;
use core::fmt::Write;
use std::collections::HashMap;
use utils::Vec2;

use crate::lexer::Token;

/// Represents a pair of tokens where each
/// token is a Number.
#[derive(Clone, Debug)]
pub struct NumPair {
    pub x: Token,
    pub y: Token,
}

impl From<&NumPair> for Vec2 {
    fn from(value: &NumPair) -> Self {
        Self {
            x: value.x.value.to_num().into(),
            y: value.y.value.to_num().into(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Init {
    pub kw: Token,
    pub bottom_left: NumPair,
    pub size: NumPair,
}

#[derive(Clone, Debug)]
pub struct Translation {
    pub kw: Token,
    pub vector: NumPair,
}

#[derive(Clone, Debug)]
pub struct Rotation {
    pub kw: Token,
    pub origin: NumPair,
    pub deg: Token,
}

#[derive(Clone, Debug)]
pub struct Sequence {
    pub nodes: Vec<Node>,
}

#[derive(Clone, Debug)]
pub struct Branch {
    pub kw: Token,
    /// Must be a Sequence(u32)
    pub lhs: Node,
    /// Must be a Sequence(u32)
    pub rhs: Node,
}

#[derive(Clone, Debug)]
pub struct Loop {
    pub kw: Token,
    /// Must be a Sequence(u32)
    pub body: Node,
}

/// Nodes are represented as indices into some storage in
/// `ASTContext`. The reason they are not references because
/// references would be invalidated after we pushed some
/// more nodes into the `ASTContext`. If you want to access the
/// fields use `ASTContext::node_to_ref` to get back the reference.
/// `ASTContext` cannot be mutated while any of the references are
/// live.
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Node {
    Operation(Operation),
    Sequence(u32),
    Branch(u32),
    Loop(u32),
}

/// Elementary operation that does not involve any control
/// flow, can be the element of a basic block.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Operation {
    Init(u32),
    Translation(u32),
    Rotation(u32),
}

#[derive(Clone, Debug)]
pub enum NodeRef<'ctx> {
    Init(&'ctx Init),
    Translation(&'ctx Translation),
    Rotation(&'ctx Rotation),
    Sequence(&'ctx Sequence),
    Branch(&'ctx Branch),
    Loop(&'ctx Loop),
}

#[derive(Clone, Debug, Default)]
pub struct ASTContext {
    inits: Vec<Init>,
    translations: Vec<Translation>,
    rotations: Vec<Rotation>,
    sequences: Vec<Sequence>,
    branches: Vec<Branch>,
    loops: Vec<Loop>,
}

impl ASTContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_root(&self) -> Node {
        Node::Sequence((self.sequences.len() - 1) as u32)
    }

    pub fn make_init(&mut self, n: Init) -> Node {
        self.inits.push(n);
        Node::Operation(Operation::Init((self.inits.len() - 1) as u32))
    }

    pub fn make_translation(&mut self, n: Translation) -> Node {
        self.translations.push(n);
        Node::Operation(Operation::Translation((self.translations.len() - 1) as u32))
    }

    pub fn make_rotation(&mut self, n: Rotation) -> Node {
        self.rotations.push(n);
        Node::Operation(Operation::Rotation((self.rotations.len() - 1) as u32))
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
            Node::Operation(op) => self.op_to_ref(op),
            Node::Sequence(id) => NodeRef::Sequence(&self.sequences[id as usize]),
            Node::Branch(id) => NodeRef::Branch(&self.branches[id as usize]),
            Node::Loop(id) => NodeRef::Loop(&self.loops[id as usize]),
        }
    }

    pub fn op_to_ref(&self, o: Operation) -> NodeRef {
        match o {
            Operation::Init(id) => NodeRef::Init(&self.inits[id as usize]),
            Operation::Translation(id) => NodeRef::Translation(&self.translations[id as usize]),
            Operation::Rotation(id) => NodeRef::Rotation(&self.rotations[id as usize]),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Annotations {
    pub pre_annotations: HashMap<Node, Vec<String>>,
    pub post_annotations: HashMap<Node, Vec<String>>,
}

impl Annotations {
    pub fn new() -> Self {
        Self::default()
    }
}

pub fn print(root: Node, ctx: &ASTContext, ann: &Annotations) -> String {
    print_impl(0, root, ctx, ann)
}

fn print_impl(ind: u32, n: Node, ctx: &ASTContext, ann: &Annotations) -> String {
    let mut result = String::new();
    if !matches!(n, Node::Sequence(_)) {
        result.push_str(&indent(ind));
    }
    result.push_str(&render_pre_annotations(n, ann));
    match ctx.node_to_ref(n) {
        NodeRef::Init(init) => {
            write!(
                result,
                "init({}, {}, {}, {})",
                init.bottom_left.x, init.bottom_left.y, init.size.x, init.size.y
            )
            .unwrap();
        }
        NodeRef::Translation(trans) => {
            write!(
                result,
                "translation({}, {})",
                trans.vector.x, trans.vector.y
            )
            .unwrap();
        }
        NodeRef::Rotation(rot) => {
            write!(
                result,
                "rotation({}, {}, {})",
                rot.origin.x, rot.origin.y, rot.deg
            )
            .unwrap();
        }
        NodeRef::Sequence(seq) => {
            let v: Vec<_> = seq
                .nodes
                .iter()
                .map(|n: &Node| print_impl(ind, *n, ctx, ann))
                .collect();
            result.push_str(&v.join(";\n"));
        }
        NodeRef::Branch(branch) => {
            result.push_str("{\n");
            result.push_str(&print_impl(ind + 2, branch.lhs, ctx, ann));
            write!(result, "\n{}}} or {{\n", &indent(ind)).unwrap();
            result.push_str(&print_impl(ind + 2, branch.rhs, ctx, ann));
            write!(result, "\n{}}}", &indent(ind)).unwrap();
        }
        NodeRef::Loop(l) => {
            result.push_str("iter {\n");
            result.push_str(&print_impl(ind + 2, l.body, ctx, ann));
            write!(result, "\n{}}}", &indent(ind)).unwrap();
        }
    };
    result.push_str(&render_post_annotations(n, ann));
    result
}

// TODO: Unfortunately, it looks like I cannot specialize
// functions in Rust yet. Remove this workaround once that
// language feature is added.
struct IsPre<const PRE: bool>;

trait RenderAnnotations {
    fn render_annotations(n: Node, ann: &HashMap<Node, Vec<String>>) -> String;
}

impl<const PRE: bool> RenderAnnotations for IsPre<PRE> {
    fn render_annotations(n: Node, ann: &HashMap<Node, Vec<String>>) -> String {
        if let Some(annotations) = ann.get(&n) {
            if !annotations.is_empty() {
                let mut result = (if PRE { "" } else { " " }).to_owned();
                result.push_str("/* ");
                result.push_str(&annotations.join(" "));
                result.push_str(" */");
                result.push_str(if PRE { " " } else { "" });
                return result;
            }
        }
        String::new()
    }
}

fn render_pre_annotations(n: Node, ann: &Annotations) -> String {
    IsPre::<true>::render_annotations(n, &ann.pre_annotations)
}

fn render_post_annotations(n: Node, ann: &Annotations) -> String {
    IsPre::<false>::render_annotations(n, &ann.post_annotations)
}
