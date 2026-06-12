use crate::bond::BondPair;

/// Assign each bead a connected-component id over the bond graph.
/// Unbonded beads each get a unique id. Result length equals `bead_count`.
///
/// Uses union-find with path compression. O(N + bonds·α(N)).
pub fn compute_component_ids(bead_count: usize, bonds: &[BondPair]) -> Vec<u32> {
    let mut parent: Vec<u32> = (0..bead_count as u32).collect();

    fn find(parent: &mut [u32], mut x: u32) -> u32 {
        while parent[x as usize] != x {
            let p = parent[x as usize];
            parent[x as usize] = parent[p as usize];
            x = parent[x as usize];
        }
        x
    }

    for b in bonds {
        let a = find(&mut parent, b.lo());
        let c = find(&mut parent, b.hi());
        if a != c {
            parent[a as usize] = c;
        }
    }

    // Flatten so every bead points at its root.
    for i in 0..bead_count {
        parent[i] = find(&mut parent, i as u32);
    }
    parent
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unbonded_beads_each_get_unique_id() {
        let ids = compute_component_ids(3, &[]);
        let set: std::collections::HashSet<_> = ids.iter().collect();
        assert_eq!(set.len(), 3);
    }

    #[test]
    fn two_bonded_beads_share_id() {
        let bonds = vec![BondPair::new(0, 1)];
        let ids = compute_component_ids(3, &bonds);
        assert_eq!(ids[0], ids[1]);
        assert_ne!(ids[0], ids[2]);
    }

    #[test]
    fn three_bead_chain_shares_id() {
        let bonds = vec![BondPair::new(0, 1), BondPair::new(1, 2)];
        let ids = compute_component_ids(4, &bonds);
        assert_eq!(ids[0], ids[1]);
        assert_eq!(ids[1], ids[2]);
        assert_ne!(ids[0], ids[3]);
    }

    #[test]
    fn triangle_shares_id() {
        let bonds = vec![
            BondPair::new(0, 1),
            BondPair::new(1, 2),
            BondPair::new(2, 0),
        ];
        let ids = compute_component_ids(3, &bonds);
        assert_eq!(ids[0], ids[1]);
        assert_eq!(ids[1], ids[2]);
    }

    #[test]
    fn empty_bead_list_returns_empty() {
        let ids = compute_component_ids(0, &[]);
        assert!(ids.is_empty());
    }
}
