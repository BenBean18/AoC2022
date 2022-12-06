# My Advent of Code 2022 solutions

I'm trying to learn Haskell this year since it seems like a cool challenge to use functional programming! It's been an adventure, the main things that have been weird is that variables are immutable, there are no for/while loops (you have to use recursive functions instead), and IO functions have to be explicitly declared (which makes it hard to do visualizations I think).

## Days 1-3
Code is in `day#/code` and must be executed with the `day#` folder as the working directory.

## Days 4+
I switched to using `cabal` so I can add external libraries (like regex parsers, for example). Code is in `src/`.

To run a program, do `cabal run AoC2022 <day#> <part#>` in the AoC2022 folder.
For example, `cabal run AoC2022 5 2` runs Day 5 part 2.

I hope this can be used as a resource for other people doing Advent of Code this year! I'll try to comment well :)
