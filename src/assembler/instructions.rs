
// MARK: - Value Types

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'a> {
    String(&'a str),
    Numeric(usize)
}

// MARK: - Syntax List Nodes - Directive

#[derive(Debug, PartialEq)]
pub struct DirectiveNode<'a> {
    /// The directive identifier.
    identifier: &'a str,
    /// List of literals provided as arguments to the directive.
    arguments: Vec<Literal<'a>>
}

impl<'a> DirectiveNode<'a> {
    pub fn new(identifier: &'a str, arguments: Vec<Literal<'a>>) -> DirectiveNode<'a> {
        DirectiveNode {
            identifier: identifier,
            arguments: arguments
        }
    }
}

// MARK: - Syntax List Nodes - Label

#[derive(Debug, PartialEq)]
pub struct LabelNode<'a> {
    /// The label identifier.
    identifier: &'a str,
}

impl<'a> LabelNode<'a> {
    pub fn new(identifier: &'a str) -> LabelNode<'a> {
        LabelNode {
            identifier: identifier
        }
    }
}
