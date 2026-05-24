use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Pair {
    pub a: u32,
    pub b: u32,
    pub t: f32,
}

// Deterministic greedy coloring. Pairs are processed in (t, a, b) order; each
// pair gets the smallest color not used by any already-colored neighbour.
// Returns one color per input pair (same order as input).
pub fn color_pairs(pairs: &[Pair]) -> Vec<u32> {
    let n = pairs.len();
    let mut order: Vec<usize> = (0..n).collect();
    order.sort_by(|&i, &j| {
        let pi = &pairs[i];
        let pj = &pairs[j];
        (pi.t, pi.a, pi.b)
            .partial_cmp(&(pj.t, pj.a, pj.b))
            .unwrap()
    });

    let mut bead_to_pairs: HashMap<u32, Vec<usize>> = HashMap::new();
    for (i, p) in pairs.iter().enumerate() {
        bead_to_pairs.entry(p.a).or_default().push(i);
        bead_to_pairs.entry(p.b).or_default().push(i);
    }

    let mut colors = vec![u32::MAX; n];
    for &i in &order {
        let p = &pairs[i];
        let mut used: Vec<u32> = Vec::new();
        if let Some(ns) = bead_to_pairs.get(&p.a) {
            for &neighbor in ns {
                if neighbor != i && colors[neighbor] != u32::MAX {
                    used.push(colors[neighbor]);
                }
            }
        }
        if let Some(ns) = bead_to_pairs.get(&p.b) {
            for &neighbor in ns {
                if neighbor != i && colors[neighbor] != u32::MAX {
                    used.push(colors[neighbor]);
                }
            }
        }
        used.sort_unstable();
        used.dedup();
        let mut c = 0u32;
        for &u in &used {
            if u == c {
                c += 1;
            } else if u > c {
                break;
            }
        }
        colors[i] = c;
    }
    colors
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn isolated_pairs_get_same_color() {
        let pairs = vec![
            Pair { a: 0, b: 1, t: 0.1 },
            Pair { a: 2, b: 3, t: 0.2 },
        ];
        let colors = color_pairs(&pairs);
        assert_eq!(colors, vec![0, 0]);
    }

    #[test]
    fn sharing_pairs_get_different_colors() {
        let pairs = vec![
            Pair { a: 0, b: 1, t: 0.1 },
            Pair { a: 1, b: 2, t: 0.2 },
        ];
        let colors = color_pairs(&pairs);
        assert_eq!(colors, vec![0, 1]);
    }

    #[test]
    fn coloring_is_deterministic() {
        let pairs = vec![
            Pair { a: 0, b: 1, t: 0.3 },
            Pair { a: 1, b: 2, t: 0.1 },
            Pair { a: 2, b: 3, t: 0.2 },
            Pair { a: 0, b: 3, t: 0.4 },
        ];
        let c1 = color_pairs(&pairs);
        let c2 = color_pairs(&pairs);
        assert_eq!(c1, c2);
    }

    #[test]
    fn lower_toi_gets_lower_color_among_neighbors() {
        let pairs = vec![
            Pair { a: 0, b: 1, t: 0.5 },
            Pair { a: 1, b: 2, t: 0.1 },
        ];
        let colors = color_pairs(&pairs);
        assert_eq!(colors[1], 0, "earlier TOI gets color 0");
        assert_eq!(colors[0], 1);
    }
}
