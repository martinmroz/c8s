
// MARK: - Value Types

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'a> {
    String(&'a str),
    Numeric(usize)
}

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
pub enum Node<'a> {
    Directive   { identifier: &'a str, arguments: Vec<Literal<'a>> },
    Label       { identifier: &'a str },
    Instruction {   mnemonic: &'a str,    fields: Vec<InstructionField<'a>> }
}
