use std::fs::read_to_string;
use std::ops::RangeInclusive;

use anyhow::{Context, Result};

fn main() -> Result<()> {
    let input = read_to_string("../../input/day4.txt")?;
    let number_overlap = input
        .lines()
        .map(|line| {
            line.split_once(',')
                .and_then(|(first, second)| {
                    let (first_start, first_end) = first.split_once('-')?;
                    let (second_start, second_end) = second.split_once('-')?;
                    let parse = |num: &str| num.parse::<usize>().ok();
                    let first_start = parse(first_start)?;
                    let first_end = parse(first_end)?;
                    let second_start = parse(second_start)?;
                    let second_end = parse(second_end)?;
                    Some((first_start..=first_end, second_start..=second_end))
                })
                .context("parse failed")
        })
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .filter(range_overlap)
        .count();
    println!("{number_overlap}");
    Ok(())
}

fn range_overlap<T: PartialOrd>(
    (range_a, range_b): &(RangeInclusive<T>, RangeInclusive<T>),
) -> bool {
    let overlap =
        |a: &RangeInclusive<T>, b: &RangeInclusive<T>| a.contains(b.start()) || a.contains(b.end());
    overlap(range_a, range_b) || overlap(range_b, range_a)
}
