use anyhow::{Context, Result};
use std::path::Path;

fn main() -> Result<()> {
    std::fs::read_to_string(Path::new("../../input/day1.txt"))
        .context("Couldn't read input!")
        .map(|input| {
            [1, 3]
                .into_iter()
                .map(|top_n| {
                    parsed_summed_up_calories(&input)
                        .map(sort_calories)
                        .map(get_top_n_calories(top_n))
                        .map(|calories| (calories, top_n))
                })
                .for_each(print_result)
        })
}

#[derive(Default, Debug)]
struct Calories(pub usize);

// utils

fn str_not_empty(str: &&str) -> bool {
    !str.is_empty()
}

// printing

fn print_result(result: Result<(Calories, usize)>) {
    match result {
        Ok((calories, n)) => println!("Top {n} elves carry {calories:?}"),
        Err(err) => println!("Algo failed with error: {err:?}"),
    }
}

// parsing & preparation

fn parsed_summed_up_calories(input: &str) -> Result<Vec<Calories>> {
    input
        .split("\n\n")
        .filter(str_not_empty)
        .map(sum_calories)
        .collect()
}

fn sum_calories(list_per_elf: &str) -> Result<Calories> {
    list_per_elf
        .split('\n')
        .filter(str_not_empty)
        .map(parse_calories)
        .sum::<Result<usize>>()
        .map(Calories)
}

fn parse_calories(calories_str: &str) -> Result<usize> {
    calories_str
        .parse::<usize>()
        .context("Didn't find parseable calories!")
}

// sorting

fn sort_calories(mut elves_calories: Vec<Calories>) -> Vec<Calories> {
    elves_calories.sort_by(|Calories(ref a), Calories(ref b)| a.cmp(b).reverse());
    elves_calories
}

// calculating top sum

fn get_top_n_calories(top_n: usize) -> impl FnOnce(Vec<Calories>) -> Calories {
    move |elves_calories| {
        elves_calories
            .iter()
            .take(top_n)
            .fold(Calories::default(), |Calories(sum), Calories(cals)| {
                Calories(sum + cals)
            })
    }
}
