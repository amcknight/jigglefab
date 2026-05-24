use glam::Vec2;

use crate::chemistry::{Op, Tag};

pub const STACK_CAP: usize = 16;

#[derive(Debug, Clone, Copy)]
pub struct Bead {
    pub pos: Vec2,
    pub vel: Vec2,
    pub tag: Tag,
    pub payload: u32,
    pub alive: bool,
    // Beads born this substep skip the rest of the substep's contact pass —
    // they appear at the midpoint of their parents, which can clip into a
    // third neighbour. Letting one substep elapse gives them a clean position
    // before they participate in CCD.
    pub born_this_substep: bool,
    pub stack_len: u32,
    pub stack: [Op; STACK_CAP],
}

impl Bead {
    pub fn top_op(&self) -> Op {
        if self.stack_len == 0 {
            Op::nop()
        } else {
            self.stack[(self.stack_len - 1) as usize]
        }
    }

    pub fn pop_top(&mut self) {
        if self.stack_len > 0 {
            self.stack_len -= 1;
        }
    }

    pub fn load_program(&mut self, prog: &[Op]) {
        debug_assert!(prog.len() <= STACK_CAP);
        self.stack_len = prog.len() as u32;
        for (i, &op) in prog.iter().enumerate() {
            self.stack[i] = op;
        }
    }
}
