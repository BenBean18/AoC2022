#!/bin/bash
export THIS_DAY=$(pwd)
alias part1="g++ $THIS_DAY/code/1.cpp -o $THIS_DAY/code/1.out && $THIS_DAY/code/1.out $THIS_DAY/input.txt"
alias part1r="$THIS_DAY/code/1.out $THIS_DAY/input.txt"
alias part2="g++ $THIS_DAY/code/2.cpp -o $THIS_DAY/code/2.out && $THIS_DAY/code/2.out $THIS_DAY/input.txt"
alias part2r="$THIS_DAY/code/2.out $THIS_DAY/input.txt"