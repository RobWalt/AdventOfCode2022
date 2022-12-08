use std::fs::read_to_string;
use std::str::FromStr;

use anyhow::Result;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::space0;
use nom::combinator::map_res;
use nom::{Finish, IResult};
use strum::EnumString;

#[derive(EnumString, Debug, Clone, Copy)]
enum EnemyChoice {
    #[strum(serialize = "A")]
    Rock,
    #[strum(serialize = "B")]
    Paper,
    #[strum(serialize = "C")]
    Scissors,
}

#[derive(EnumString, Debug, Clone, Copy)]
enum MyChoice {
    #[strum(serialize = "X")]
    Rock = 1,
    #[strum(serialize = "Y")]
    Paper = 2,
    #[strum(serialize = "Z")]
    Scissors = 3,
}

#[derive(Debug, Clone, Copy, EnumString)]
enum Outcome {
    #[strum(serialize = "X")]
    Lose = 0,
    #[strum(serialize = "Y")]
    Draw = 3,
    #[strum(serialize = "Z")]
    Win = 6,
}

fn calc_outcome_t1(this_match @ (_, me): (EnemyChoice, MyChoice)) -> (MyChoice, Outcome) {
    use Outcome::*;
    (
        me,
        match this_match {
            (EnemyChoice::Rock, MyChoice::Rock) => Draw,
            (EnemyChoice::Rock, MyChoice::Paper) => Win,
            (EnemyChoice::Rock, MyChoice::Scissors) => Lose,
            (EnemyChoice::Paper, MyChoice::Rock) => Lose,
            (EnemyChoice::Paper, MyChoice::Paper) => Draw,
            (EnemyChoice::Paper, MyChoice::Scissors) => Win,
            (EnemyChoice::Scissors, MyChoice::Rock) => Win,
            (EnemyChoice::Scissors, MyChoice::Paper) => Lose,
            (EnemyChoice::Scissors, MyChoice::Scissors) => Draw,
        },
    )
}

fn calc_outcome_t2(this_match @ (_, outcome): (EnemyChoice, Outcome)) -> (MyChoice, Outcome) {
    use Outcome::*;
    (
        match this_match {
            (EnemyChoice::Rock, Lose) => MyChoice::Scissors,
            (EnemyChoice::Rock, Draw) => MyChoice::Rock,
            (EnemyChoice::Rock, Win) => MyChoice::Paper,
            (EnemyChoice::Paper, Lose) => MyChoice::Rock,
            (EnemyChoice::Paper, Draw) => MyChoice::Paper,
            (EnemyChoice::Paper, Win) => MyChoice::Scissors,
            (EnemyChoice::Scissors, Lose) => MyChoice::Paper,
            (EnemyChoice::Scissors, Draw) => MyChoice::Scissors,
            (EnemyChoice::Scissors, Win) => MyChoice::Rock,
        },
        outcome,
    )
}

fn parse_match<T: FromStr>(line: &str) -> IResult<&str, (EnemyChoice, T)> {
    let (non_space_start, _) = space0(line)?;
    let (rest, enemy) =
        map_res(alt((tag("A"), tag("B"), tag("C"))), EnemyChoice::from_str)(non_space_start)?;
    let (non_space_start, _) = space0(rest)?;
    let (rest, me) = map_res(alt((tag("X"), tag("Y"), tag("Z"))), T::from_str)(non_space_start)?;
    Ok((rest, (enemy, me)))
}

fn task<T>(input: &str, outcome_f: fn((EnemyChoice, T)) -> (MyChoice, Outcome)) -> Result<usize>
where
    T: FromStr,
{
    input
        .lines()
        .map(|line| {
            parse_match(line)
                .finish()
                .map(|(_, data)| data)
                .map(outcome_f)
                .map(|(me, outcome)| me as usize + outcome as usize)
                .map_err(|_| anyhow::anyhow!("parsing failed"))
        })
        .sum::<Result<_, _>>()
}

fn main() -> Result<()> {
    let input = read_to_string("../../input/day2.txt")?;
    let task_one = task(input.as_str(), calc_outcome_t1)?;
    let task_two = task(input.as_str(), calc_outcome_t2)?;
    println!("The end result of task one is: {task_one}");
    println!("The end result of task two is: {task_two}");
    Ok(())
}

#[test]
fn example() -> Result<()> {
    let input = String::from("A Y\nB X\nC Z");
    let task_one_test = task(input.as_str(), calc_outcome_t1)?;
    assert_eq!(task_one_test, 15);
    Ok(())
}
