use glam::Vec2;

// Cell size is 2*RADIUS so that any pair with center-distance < RADIUS (a bond)
// AND any pair that could cross the bond boundary within one frame is guaranteed
// to live in same-or-adjacent cells. Two cells (k apart, axis-aligned) have
// minimum center-to-center distance (k-1)*CELL_SIZE; with CELL_SIZE = 2, cells
// 2 apart have min distance 2 — well beyond the bond threshold of 1.
pub const CELL_SIZE: f32 = 2.0;

pub struct Grid {
    world_size: f32,
    cells_per_axis: usize,
    // cells[cy * n + cx] = Vec<bead_id>
    cells: Vec<Vec<u32>>,
    // bead_id → cell index, or -1 if the bead is not currently in the grid.
    // Lets `update_position` short-circuit when a bead hasn't crossed a cell
    // boundary (the overwhelmingly common case at dt_sub=1/240).
    cell_of_bead: Vec<i32>,
    // List of cell indices that currently contain ≥1 bead. Lets
    // `candidate_pairs` skip the 16k empty cells in a 10k-bead sim instead
    // of iterating every cell.
    non_empty_cells: Vec<u32>,
    // For each cell, the index into non_empty_cells (or -1 if empty).
    // Enables O(1) removal from non_empty_cells via swap_remove.
    cell_in_set_at: Vec<i32>,
}

impl Grid {
    pub fn new(world_size: f32) -> Self {
        let cells_per_axis = (world_size / CELL_SIZE).ceil() as usize;
        let n_cells = cells_per_axis * cells_per_axis;
        let cells = (0..n_cells).map(|_| Vec::new()).collect();
        let cell_in_set_at = vec![-1; n_cells];
        Self {
            world_size,
            cells_per_axis,
            cells,
            cell_of_bead: Vec::new(),
            non_empty_cells: Vec::new(),
            cell_in_set_at,
        }
    }

    pub fn clear(&mut self) {
        for &cell_idx in &self.non_empty_cells {
            self.cells[cell_idx as usize].clear();
            self.cell_in_set_at[cell_idx as usize] = -1;
        }
        self.non_empty_cells.clear();
        for v in self.cell_of_bead.iter_mut() {
            *v = -1;
        }
    }

    fn ensure_bead_capacity(&mut self, bead_id: u32) {
        let needed = (bead_id as usize) + 1;
        if self.cell_of_bead.len() < needed {
            self.cell_of_bead.resize(needed, -1);
        }
    }

    fn mark_cell_non_empty(&mut self, cell_idx: usize) {
        if self.cell_in_set_at[cell_idx] < 0 {
            self.cell_in_set_at[cell_idx] = self.non_empty_cells.len() as i32;
            self.non_empty_cells.push(cell_idx as u32);
        }
    }

    fn mark_cell_empty(&mut self, cell_idx: usize) {
        let pos = self.cell_in_set_at[cell_idx];
        if pos < 0 {
            return;
        }
        let pos = pos as usize;
        let last_idx = self.non_empty_cells.len() - 1;
        if pos != last_idx {
            let moved = self.non_empty_cells[last_idx];
            self.non_empty_cells[pos] = moved;
            self.cell_in_set_at[moved as usize] = pos as i32;
        }
        self.non_empty_cells.pop();
        self.cell_in_set_at[cell_idx] = -1;
    }

    /// Remove `bead_id` from `cells[cell_idx]` and mark the cell empty if
    /// it ends up so. Precondition: `bead_id` MUST be in `cells[cell_idx]`
    /// — callers must check `cell_of_bead` before calling.
    fn remove_bead_from_cell(&mut self, bead_id: u32, cell_idx: usize) {
        let pos = self.cells[cell_idx]
            .iter()
            .position(|&id| id == bead_id)
            .expect("bead_id missing from cell it claims to be in");
        self.cells[cell_idx].swap_remove(pos);
        if self.cells[cell_idx].is_empty() {
            self.mark_cell_empty(cell_idx);
        }
    }

    /// Insert a bead into the grid. Must only be called after `clear()` or
    /// for a `bead_id` that's never been inserted — use `update_position` to
    /// move an existing bead.
    pub fn insert(&mut self, bead_id: u32, pos: Vec2) {
        let cell_idx = self.cell_idx_of(pos);
        self.cells[cell_idx].push(bead_id);
        self.ensure_bead_capacity(bead_id);
        self.cell_of_bead[bead_id as usize] = cell_idx as i32;
        if self.cells[cell_idx].len() == 1 {
            self.mark_cell_non_empty(cell_idx);
        }
    }

    /// Move a bead to the cell its new position falls in, or insert it if
    /// it wasn't in the grid yet. Short-circuits when the cell hasn't
    /// changed — at dt_sub = 1/240 with unit-speed beads moving 0.004 units
    /// per substep, ~99.9% of calls short-circuit.
    pub fn update_position(&mut self, bead_id: u32, pos: Vec2) {
        self.ensure_bead_capacity(bead_id);
        let new_cell = self.cell_idx_of(pos) as i32;
        let old_cell = self.cell_of_bead[bead_id as usize];
        if old_cell == new_cell {
            return;
        }
        if old_cell >= 0 {
            self.remove_bead_from_cell(bead_id, old_cell as usize);
        }
        let new = new_cell as usize;
        self.cells[new].push(bead_id);
        self.cell_of_bead[bead_id as usize] = new_cell;
        if self.cells[new].len() == 1 {
            self.mark_cell_non_empty(new);
        }
    }

    /// Remove a bead from the grid (e.g. on death). Idempotent.
    pub fn remove_bead(&mut self, bead_id: u32) {
        if (bead_id as usize) >= self.cell_of_bead.len() {
            return;
        }
        let cell = self.cell_of_bead[bead_id as usize];
        if cell < 0 {
            return;
        }
        self.remove_bead_from_cell(bead_id, cell as usize);
        self.cell_of_bead[bead_id as usize] = -1;
    }

    fn cell_idx_of(&self, pos: Vec2) -> usize {
        let (cx, cy) = self.cell_of(pos);
        cy * self.cells_per_axis + cx
    }

    fn cell_of(&self, pos: Vec2) -> (usize, usize) {
        let wrapped = self.wrap_pos(pos);
        let cx = (wrapped.x / CELL_SIZE) as usize % self.cells_per_axis;
        let cy = (wrapped.y / CELL_SIZE) as usize % self.cells_per_axis;
        (cx, cy)
    }

    /// Wraps a position into [0, world_size) in both axes.
    pub fn wrap_pos(&self, pos: Vec2) -> Vec2 {
        let mut x = pos.x.rem_euclid(self.world_size);
        let mut y = pos.y.rem_euclid(self.world_size);
        if x == self.world_size { x = 0.0; }
        if y == self.world_size { y = 0.0; }
        Vec2::new(x, y)
    }

    pub fn world_size(&self) -> f32 { self.world_size }

    /// Yields each unordered candidate pair (a, b) with a < b such that the
    /// two beads sit in the same or adjacent cells (with torus wrap).
    ///
    /// Iterates only the cells currently holding ≥1 bead. The (a.min, a.max)
    /// pair normalisation means iteration order doesn't affect downstream
    /// determinism (the active-contacts Vec is sorted by (t, a, b) before
    /// resolve, and (a, b) is unique per pair).
    pub fn candidate_pairs(&self) -> Vec<(u32, u32)> {
        let n = self.cells_per_axis as isize;
        let stride = self.cells_per_axis;
        let mut pairs = Vec::new();
        for &cell_idx in &self.non_empty_cells {
            let cell_idx = cell_idx as usize;
            let here = &self.cells[cell_idx];
            let cx = (cell_idx % stride) as isize;
            let cy = (cell_idx / stride) as isize;
            // Pairs within this cell.
            for i in 0..here.len() {
                for j in (i + 1)..here.len() {
                    let (a, b) = (here[i], here[j]);
                    pairs.push((a.min(b), a.max(b)));
                }
            }
            // Pairs with the 4 neighbours we haven't visited (avoid double-counting):
            // (+1, 0), (-1, +1), (0, +1), (+1, +1)
            let neighbours = [(1, 0), (-1, 1), (0, 1), (1, 1)];
            for (dx, dy) in neighbours {
                let nx = ((cx + dx).rem_euclid(n)) as usize;
                let ny = ((cy + dy).rem_euclid(n)) as usize;
                let there = &self.cells[ny * stride + nx];
                if there.is_empty() {
                    continue;
                }
                for &a in here {
                    for &b in there {
                        if a != b {
                            pairs.push((a.min(b), a.max(b)));
                        }
                    }
                }
            }
        }
        pairs
    }

    /// Shortest displacement from `from` to `to` under torus topology.
    /// Returns the vector with components in [-world_size/2, world_size/2].
    pub fn min_image(&self, from: Vec2, to: Vec2) -> Vec2 {
        let half = self.world_size * 0.5;
        let mut d = to - from;
        if d.x >  half { d.x -= self.world_size; }
        if d.x < -half { d.x += self.world_size; }
        if d.y >  half { d.y -= self.world_size; }
        if d.y < -half { d.y += self.world_size; }
        d
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_and_find_pair_same_cell() {
        let mut g = Grid::new(10.0);
        g.insert(0, Vec2::new(2.1, 2.1));
        g.insert(1, Vec2::new(2.5, 2.5));
        let pairs = g.candidate_pairs();
        assert_eq!(pairs, vec![(0, 1)]);
    }

    #[test]
    fn insert_and_find_pair_adjacent_cell() {
        let mut g = Grid::new(10.0);
        g.insert(0, Vec2::new(2.1, 2.1)); // cell (2,2)
        g.insert(1, Vec2::new(3.5, 2.5)); // cell (3,2)
        let pairs = g.candidate_pairs();
        assert_eq!(pairs, vec![(0, 1)]);
    }

    #[test]
    fn far_beads_not_paired() {
        let mut g = Grid::new(10.0);
        g.insert(0, Vec2::new(1.0, 1.0));
        g.insert(1, Vec2::new(5.0, 5.0));
        let pairs = g.candidate_pairs();
        assert!(pairs.is_empty());
    }

    #[test]
    fn wraps_across_torus() {
        let mut g = Grid::new(10.0);
        g.insert(0, Vec2::new(0.1, 5.0)); // cell (0,5)
        g.insert(1, Vec2::new(9.9, 5.0)); // cell (9,5) — adjacent under wrap
        let pairs = g.candidate_pairs();
        assert_eq!(pairs, vec![(0, 1)]);
    }

    #[test]
    fn min_image_picks_short_side() {
        let g = Grid::new(10.0);
        // from (9.5, 5) to (0.5, 5): naive diff is (-9, 0); short way is (+1, 0).
        let d = g.min_image(Vec2::new(9.5, 5.0), Vec2::new(0.5, 5.0));
        assert!((d - Vec2::new(1.0, 0.0)).length() < 1e-5);
    }

    #[test]
    fn wrap_pos_into_unit_interval() {
        let g = Grid::new(10.0);
        assert!((g.wrap_pos(Vec2::new(10.5, -0.5)) - Vec2::new(0.5, 9.5)).length() < 1e-5);
    }

    #[test]
    fn update_position_moves_bead_across_cells() {
        let mut g = Grid::new(10.0);
        g.insert(0, Vec2::new(2.1, 2.1)); // cell (1,1)
        g.insert(1, Vec2::new(4.1, 4.1)); // cell (2,2)
        // No pair: cells (1,1) and (2,2) are diagonal but min cell distance is √2 in cell units → same as adjacent (chebyshev distance 1).
        // (1,1) and (2,2) are chebyshev-adjacent, so they DO pair.
        let pairs_before = g.candidate_pairs();
        assert_eq!(pairs_before, vec![(0, 1)]);

        // Move bead 0 to cell (4,4): chebyshev distance 2 from bead 1 → no pair.
        g.update_position(0, Vec2::new(8.1, 8.1));
        let pairs_after = g.candidate_pairs();
        assert!(pairs_after.is_empty(), "bead 0 moved out of bead 1's neighbour ring");
    }

    #[test]
    fn update_position_short_circuits_when_cell_unchanged() {
        let mut g = Grid::new(10.0);
        g.insert(0, Vec2::new(2.1, 2.1)); // cell (1,1)
        // Same cell, different position. Pair set should be unchanged.
        g.update_position(0, Vec2::new(2.9, 2.9));
        g.insert(1, Vec2::new(3.1, 3.1)); // cell (1,1)
        let pairs = g.candidate_pairs();
        assert_eq!(pairs, vec![(0, 1)]);
    }

    #[test]
    fn remove_bead_drops_it_from_grid() {
        let mut g = Grid::new(10.0);
        g.insert(0, Vec2::new(2.1, 2.1));
        g.insert(1, Vec2::new(2.5, 2.5));
        g.remove_bead(0);
        let pairs = g.candidate_pairs();
        assert!(pairs.is_empty(), "bead 0 was removed; bead 1 alone has no pairs");
    }

    #[test]
    fn clear_resets_all_state() {
        let mut g = Grid::new(10.0);
        g.insert(0, Vec2::new(2.1, 2.1));
        g.insert(1, Vec2::new(2.5, 2.5));
        g.clear();
        // After clear, re-inserting different beads should produce only their pairs.
        g.insert(2, Vec2::new(7.1, 7.1));
        g.insert(3, Vec2::new(7.5, 7.5));
        let pairs = g.candidate_pairs();
        assert_eq!(pairs, vec![(2, 3)]);
    }
}
