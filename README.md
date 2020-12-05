# Advent of Code 2020

My solvers for the [Advent of Code 2020](https://adventofcode.com/2020).

My plan for this year is to quickly bash out initial solutions using whatever
feels most comfortable in the moment (and is most likely to get me on the
leaderboard), and then go back and produce a tidy Haskell solution for each
problem using only the built-in libraries.

## Instructions

To compile a solver, run
```
ghc --make -no-keep-hi-files -no-keep-o-files <day_number>
```
replacing `<day_number>` with `01`, `02`, etc. This will produce an executable
with the same name in the current directory.

All solvers take a command line argument indicating which part to solve, and
read their input from stdin. For example, to run part 2 of the Day 1 solver with
the input in `input.txt`, type:
```
cat input.txt | ./01 2
```
