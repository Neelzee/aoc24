use std::{collections::HashMap, fs::File, io::Read};

fn main() {
    let stones = get_input();

    let mut stone_count = 0;
    let mut map = HashMap::new();
    for stone in stones {
        let mut cur_stone_count = 0;
        for i in 0..6 {
            let i_stone = iterate(stone, i, &mut map, 6);
            println!("{:?}", i_stone.iter().map(|i| i.val).collect::<Vec<_>>());
            cur_stone_count = i_stone.len();
        }
        stone_count += cur_stone_count;
    }

    println!("Stones: {}", stone_count);
}

#[derive(Hash, Clone, PartialEq, Eq, Copy)]
pub struct Stone {
    pub val: usize,
}

fn sub_iterate(s: Stone) -> Vec<Stone> {
    match s.val {
        0 => vec![Stone::new(1)],
        // Even number of digits
        v if v.to_string().chars().count() % 2 == 0 => {
            // I'm doing this twice! :(
            let digits: Vec<char> = v.to_string().chars().collect();
            let (f, l) = digits.split_at(digits.len() / 2);

            vec![
                Stone::new(f.iter().collect::<String>().parse::<usize>().unwrap()),
                Stone::new(l.iter().collect::<String>().parse::<usize>().unwrap()),
            ]
        }
        v => vec![Stone::new(v * 2024)],
    }
}

pub fn iterate(
    stone: Stone,
    current_iteration: usize,
    map: &mut HashMap<Stone, HashMap<usize, Vec<Stone>>>,
    max_iter: usize,
) -> Vec<Stone> {
    if max_iter == current_iteration {
        return vec![stone];
    }
    match map.get(&stone) {
        Some(map) if map.contains_key(&current_iteration) => {
            map.get(&current_iteration).unwrap().clone()
        }
        Some(smap) => {
            let mut sub_map = smap.clone();
            let closest_iteration = sub_map.keys().max().unwrap();
            let (start, end);
            if *closest_iteration > current_iteration {
                (start, end) = (*closest_iteration - current_iteration, *closest_iteration);
            } else {
                (start, end) = (*closest_iteration, current_iteration - *closest_iteration);
            }
            let mut stones = sub_iterate(stone);
            for i in start..end {
                sub_map.insert(i, stones.clone());
                stones = stones
                    .into_iter()
                    .flat_map(|s| iterate(s, i, map, max_iter))
                    .collect();
            }
            stones
        }
        None => {
            let val = sub_iterate(stone);
            let mut sub_map = HashMap::new();
            sub_map.insert(current_iteration + 1, val.clone());
            map.insert(stone, sub_map);
            val
        }
    }
}

impl Stone {
    pub fn new(val: usize) -> Self {
        Self { val }
    }
}

pub fn get_input() -> Vec<Stone> {
    let mut file = File::open("input.txt").unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();
    buf.split_whitespace()
        .map(|s| Stone::new(s.parse().unwrap()))
        .collect()
}
