use crate::parallel::state::Bead;

pub struct BeadPool {
    beads: Vec<Bead>,
    free_list: Vec<u32>,
    high_water: u32,
    capacity: u32,
}

#[derive(Debug)]
pub struct PoolOverflow;

impl BeadPool {
    pub fn with_capacity(cap: u32) -> Self {
        BeadPool {
            beads: Vec::with_capacity(cap as usize),
            free_list: Vec::new(),
            high_water: 0,
            capacity: cap,
        }
    }

    pub fn alloc(&mut self, bead: Bead) -> u32 {
        self.try_alloc(bead).expect("pool overflow")
    }

    pub fn try_alloc(&mut self, mut bead: Bead) -> Result<u32, PoolOverflow> {
        bead.alive = true;
        if let Some(slot) = self.free_list.pop() {
            self.beads[slot as usize] = bead;
            return Ok(slot);
        }
        if self.high_water >= self.capacity {
            return Err(PoolOverflow);
        }
        let slot = self.high_water;
        self.beads.push(bead);
        self.high_water += 1;
        Ok(slot)
    }

    pub fn free(&mut self, slot: u32) {
        if (slot as usize) < self.beads.len() {
            self.beads[slot as usize].alive = false;
            self.free_list.push(slot);
        }
    }

    pub fn get(&self, slot: u32) -> &Bead {
        &self.beads[slot as usize]
    }
    pub fn get_mut(&mut self, slot: u32) -> &mut Bead {
        &mut self.beads[slot as usize]
    }
    pub fn high_water(&self) -> u32 {
        self.high_water
    }
    pub fn capacity(&self) -> u32 {
        self.capacity
    }
    pub fn beads(&self) -> &[Bead] {
        &self.beads
    }
    pub fn beads_mut(&mut self) -> &mut [Bead] {
        &mut self.beads
    }

    pub fn alive_slots(&self) -> impl Iterator<Item = u32> + '_ {
        self.beads.iter().enumerate().filter_map(|(i, b)| {
            if b.alive {
                Some(i as u32)
            } else {
                None
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chemistry::{Op, Tag};
    use crate::parallel::STACK_CAP;
    use glam::Vec2;

    fn make_wire() -> Bead {
        Bead {
            pos: Vec2::ZERO,
            vel: Vec2::ZERO,
            tag: Tag::Wire,
            payload: 0,
            alive: true,
            born_this_substep: false,
            stack_len: 0,
            stack: [Op::nop(); STACK_CAP],
        }
    }

    #[test]
    fn pool_allocates_and_recycles() {
        let mut pool = BeadPool::with_capacity(8);
        let i0 = pool.alloc(make_wire());
        let i1 = pool.alloc(make_wire());
        assert_eq!(i0, 0);
        assert_eq!(i1, 1);
        pool.free(i0);
        let i2 = pool.alloc(make_wire());
        assert_eq!(i2, 0, "freed slot is reused");
    }

    #[test]
    fn pool_overflow_returns_err() {
        let mut pool = BeadPool::with_capacity(2);
        pool.alloc(make_wire());
        pool.alloc(make_wire());
        assert!(pool.try_alloc(make_wire()).is_err());
    }

    #[test]
    fn alive_slots_iterates_in_index_order() {
        let mut pool = BeadPool::with_capacity(4);
        for _ in 0..4 {
            pool.alloc(make_wire());
        }
        pool.free(1);
        pool.free(3);
        let alive: Vec<u32> = pool.alive_slots().collect();
        assert_eq!(alive, vec![0, 2]);
    }
}
