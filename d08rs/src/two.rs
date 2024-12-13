use std::collections::HashSet;

use crate::get_input;

pub fn run() {
    let inpt = get_input().unwrap();
    let ants = inpt.antennas;
    let used_pos = inpt.antenna_positions;
    let (max, may) = inpt.max;
    let mut antinodes = HashSet::new();

    for (antenna, positions) in ants.iter() {
        println!("Checking: {}", antenna);
        for pos in positions.iter() {
            for other_pos in positions.iter() {
                if pos == other_pos {
                    continue;
                }

                let xs = get_line(pos, other_pos, &max, &may)
                    .into_iter()
                    .filter(|a| is_valid_pos(*a, &max, &may))
                    .collect::<Vec<_>>();

                for a in xs {
                    antinodes.insert((a.0 as usize, a.1 as usize));
                }
            }
        }
    }

    let mut antinodes_str = String::new();
    for y in 0..may {
        let mut row = String::new();
        for x in 0..max {
            let pos = (x, y);
            match used_pos.get(&pos) {
                Some(_) if antinodes.contains(&pos) => row.push('#'),
                None if antinodes.contains(&pos) => row.push('#'),
                Some(a) => row.push(*a),
                None => row.push('.'),
            }
        }
        antinodes_str.push_str(&row);
        println!("{}", row);
    }

    println!("{}", antinodes_str.chars().filter(|c| c == &'#').count());
}

fn is_valid_pos((x, y): (f64, f64), max: &usize, may: &usize) -> bool {
    x >= 0_f64 && y >= 0_f64 && x <= *max as f64 && y <= *may as f64
}

pub fn get_line(
    a: &(usize, usize),
    b: &(usize, usize),
    max: &usize,
    may: &usize,
) -> Vec<(f64, f64)> {
    let dx = a.0 as f64 - b.0 as f64;
    let dy = a.1 as f64 - b.1 as f64;

    let mut vec = Vec::new();
    vec.push((a.0 as f64, a.1 as f64));
    vec.push((b.0 as f64, b.1 as f64));

    let mut cur_pos = (a.0 as f64 + dx, a.1 as f64 + dy);

    let mut i: f64 = 0f64;
    while is_valid_pos(cur_pos, max, may) {
        i += 1f64;
        vec.push(cur_pos);
        cur_pos = (a.0 as f64 + dx * i, a.1 as f64 + dy * i);
    }

    vec
}