
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

// MARK: - Syntax List Nodes - Instruction

#[derive(Debug, PartialEq)]
pub enum InstructionField<'a> {
    /// A numeric literal value.
    NumericLiteral(usize),
    /// A general-purpose register beginning with "v".
    GeneralPurposeRegister(u8),
    /// The delay timer register ("dt").
    DelayTimer,
    /// The sound timer register ("st").
    SoundTimer,
    /// The keypad register ("k").
    KeypadRegister,
    /// The index register ("i").
    IndexRegister,
    /// Register-indirect access of memory ("[i]").
    IndexRegisterIndirect,
    /// Any other literal identifier, usually a label.
    Identifier(&'a str)
}

#[derive(Debug, PartialEq)]
pub struct InstructionNode<'a> {
    /// The instruction mnemonic as parsed.
    mnemonic: &'a str,
    /// The fields associated with the instruction.
    fields: Vec<InstructionField<'a>>
}

impl<'a> InstructionNode<'a> {
    pub fn new(mnemonic: &'a str, fields: Vec<InstructionField<'a>>) -> InstructionNode<'a> {
        InstructionNode {
            mnemonic: mnemonic,
            fields: fields
        }
    }
}
