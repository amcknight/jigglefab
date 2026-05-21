use glam::Vec2;

pub const CELL_SIZE: f32 = 1.0;

pub struct Grid {
    world_size: f32,
    cells_per_axis: usize,
    // cells[cy * n + cx] = Vec<bead_id>
    cells: Vec<Vec<u32>>,
}

impl Grid {
    pub fn new(world_size: f32) -> Self {
        let cells_per_axis = (world_size / CELL_SIZE).ceil() as usize;
        let cells = (0..cells_per_axis * cells_per_axis).map(|_| Vec::new()).collect();
        Self { world_size, cells_per_axis, cells }
    }

    pub fn clear(&mut self) {
        for c in &mut self.cells {
            c.clear();
        }
    }

    pub fn insert(&mut self, bead_id: u32, pos: Vec2) {
        let (cx, cy) = self.cell_of(pos);
        let idx = cy * self.cells_per_axis + cx;
        self.cells[idx].push(bead_id);
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
    pub fn candidate_pairs(&self) -> Vec<(u32, u32)> {
        let n = self.cells_per_axis;
        let mut pairs = Vec::new();
        for cy in 0..n {
            for cx in 0..n {
                let here = &self.cells[cy * n + cx];
                // Pairs within this cell
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
                    let nx = ((cx as isize + dx).rem_euclid(n as isize)) as usize;
                    let ny = ((cy as isize + dy).rem_euclid(n as isize)) as usize;
                    let there = &self.cells[ny * n + nx];
                    for &a in here {
                        for &b in there {
                            if a != b {
                                pairs.push((a.min(b), a.max(b)));
                            }
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
}
