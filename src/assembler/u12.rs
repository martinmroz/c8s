
use std::ops::{Mul};
use std::fmt;
use std::convert::From;

#[derive(PartialEq, Eq, Copy, Clone)]
pub struct U12(u16);

impl U12 {

    pub fn zero() -> U12 {
        U12(0)
    }

    pub fn one() -> U12 {
        U12(1)
    }

    pub fn as_u16(self) -> u16 {
        self.into()
    }

    pub fn as_usize(self) -> usize {
        self.into()
    }

    pub fn as_u8(self) -> Option<u8> {
        match self.hi_4() {
            0 => Some(self.lo_8()),
            _ => None
        }
    }

    pub fn wrapping_add(self, other: U12) -> U12 {
        U12((self.0 + other.0) & 0xFFF)
    }

    pub fn hi_4(self) -> u8 {
        ((self.0 & 0x0F00) >> 8) as u8
    }

    pub fn lo_8(self) -> u8 {
        (self.0 & 0x00FF) as u8
    }

}

pub const MAX: U12 = U12(0xFFF);
pub const MIN: U12 = U12(0x000);

// Basic Operations

impl Mul for U12 {
    type Output = U12;
    fn mul(self, other: U12) -> U12 {
        // The implementation of sub() on U16 will panic at the same times as on U12.
        let product = self.0 * other.0;
        if (product & 0xF000) != 0 {
            panic!("Arithmetic operation overflowed");
        }
        U12(product)
    }
}

// Formatting

impl fmt::Debug for U12 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#3X}", self.0) 
    }
}

// Non-Failable Conversions

impl From<u8> for U12 {
    fn from(other: u8) -> U12 {
        U12(other as u16)
    }    
}

impl From<U12> for u16 {
    fn from(other: U12) -> u16 {
        other.0
    }
}

impl From<U12> for usize {
    fn from(other: U12) -> usize {
        other.0 as usize
    }
}

// Failable Conversions

pub trait AsU12 {
    fn as_u12(self) -> Option<U12>;
}

impl AsU12 for i16 {
    fn as_u12(self) -> Option<U12> {
        (self as u16).as_u12()
    }
}

impl AsU12 for u16 {
    fn as_u12(self) -> Option<U12> {
        if (self & 0xF000) != 0 {
            None
        } else {
            Some(U12(self))
        }
    }
}

impl AsU12 for i32 {
    fn as_u12(self) -> Option<U12> {
        (self as u32).as_u12()
    }
}

impl AsU12 for u32 {
    fn as_u12(self) -> Option<U12> {
        if (self & 0xFFFFF000) != 0 {
            None
        } else {
            Some(U12(self as u16))
        }
    }
}

impl AsU12 for isize {
    fn as_u12(self) -> Option<U12> {
        (self as usize).as_u12()
    }
}

impl AsU12 for usize {
    fn as_u12(self) -> Option<U12> {
        if (self & !0xFFF) != 0 {
            None
        } else {
            Some(U12(self as u16))
        }
    }
}
