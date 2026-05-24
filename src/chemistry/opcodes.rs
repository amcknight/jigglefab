#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum OpKind {
    Nop = 0,
    Sig = 1,
    Apply = 2,
    Done = 3,
    Wait = 4,
    Take = 5,
    Drop = 6,
    Die = 7,
    Spawn = 8,
    Hold = 9,
    Send = 10,
}

impl OpKind {
    pub fn from_u8(raw: u8) -> Option<Self> {
        Some(match raw {
            0 => OpKind::Nop,
            1 => OpKind::Sig,
            2 => OpKind::Apply,
            3 => OpKind::Done,
            4 => OpKind::Wait,
            5 => OpKind::Take,
            6 => OpKind::Drop,
            7 => OpKind::Die,
            8 => OpKind::Spawn,
            9 => OpKind::Hold,
            10 => OpKind::Send,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Sig {
    Red,
    Blue,
}

// Packed opcode: top 4 bits = kind, bottom 28 bits = payload. Designed so the
// GPU port can store programs as u32 buffers without further marshalling.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Op(u32);

impl Op {
    pub fn new(kind: OpKind, payload: u32) -> Self {
        debug_assert!(payload < (1 << 28));
        Op(((kind as u32) << 28) | (payload & 0x0FFF_FFFF))
    }
    pub fn nop() -> Self {
        Op::new(OpKind::Nop, 0)
    }
    pub fn sig(s: Sig) -> Self {
        Op::new(
            OpKind::Sig,
            match s {
                Sig::Red => 0,
                Sig::Blue => 1,
            },
        )
    }
    pub fn simple(kind: OpKind) -> Self {
        Op::new(kind, 0)
    }
    pub fn send(program_index: u32) -> Self {
        Op::new(OpKind::Send, program_index)
    }

    // Encode a "legacy" state index from the existing dense `Chemistry` table.
    // Each existing state index is stuffed into the Sig payload while we
    // migrate prototype chems into the compiled form. Stop-gap until full
    // Sem-shaped chemistries replace the legacy state-index path.
    pub fn sig_legacy(state_index: u32) -> Self {
        Op::new(OpKind::Sig, state_index)
    }

    pub fn kind(self) -> OpKind {
        OpKind::from_u8((self.0 >> 28) as u8).expect("invalid opcode kind")
    }
    pub fn payload(self) -> u32 {
        self.0 & 0x0FFF_FFFF
    }
}

impl From<u32> for Op {
    fn from(v: u32) -> Self {
        Op(v)
    }
}
impl From<Op> for u32 {
    fn from(o: Op) -> Self {
        o.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nop_round_trips() {
        let op = Op::nop();
        assert_eq!(op.kind(), OpKind::Nop);
        assert_eq!(op.payload(), 0);
    }

    #[test]
    fn sig_red_blue_distinct() {
        let red = Op::sig(Sig::Red);
        let blue = Op::sig(Sig::Blue);
        assert_eq!(red.kind(), OpKind::Sig);
        assert_eq!(blue.kind(), OpKind::Sig);
        assert_ne!(red.payload(), blue.payload());
    }

    #[test]
    fn send_carries_program_index() {
        let op = Op::send(7);
        assert_eq!(op.kind(), OpKind::Send);
        assert_eq!(op.payload(), 7);
    }

    #[test]
    fn opcodes_pack_to_u32() {
        let op = Op::send(0x123);
        let raw: u32 = op.into();
        let back: Op = raw.into();
        assert_eq!(op, back);
    }
}
