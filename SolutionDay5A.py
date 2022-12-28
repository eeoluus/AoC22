# ### PROBLEM DESCRIPTION ###
# 
# Find out the top crates after rearranging them according to instructions, like so.
# Example from https://adventofcode.com/2022/day/5.
#
#
#     [D]    
# [N] [C]    
# [Z] [M] [P]
#  1   2   3 
#
# move 1 from 2 to 1
# move 3 from 1 to 3
# move 2 from 2 to 1
# move 1 from 1 to 2
#
#

import re
from collections import deque
from functools import partial, reduce
from itertools import filterfalse, zip_longest
from typing import Deque, Iterable, Tuple    # Running on Python 3.8, for 3.9+ use collections.abc.


def solution(file):
    """Solve the first problem of Advent of Code 2022 Day 5.
    
    Separate crates from instructions by splitting at the empty line. Convert
    the contents to double-ended queues for optimized pushing and popping. Then,
    go through the instructions one by one to rearrange the content deques.
    Finally, form a string from the heads of deques.    
    """
    
    lines = *map(str.rstrip, file),    # Cursed but works.
    
    crates, instructions = split_at("", lines)
    
    stacks = *map(deque, contents_of(crates)),

    rearranged_stacks = reduce(
        rearrange_by, 
        instructions, 
        stacks
    )
    top_contents = concat(map(deque.popleft, rearranged_stacks))
    
    return top_contents

def split_at(separator: str, strings: Tuple[str]) -> Tuple[Tuple[str]]:
    """Take left and right subsequences around a separator."""
    
    sep = strings.index(separator)
    
    return strings[:sep], strings[sep + 1:]

def contents_of(crate_stacks: Tuple[str]) -> Iterable[str]:
    """Remove brackets and whitespace."""
    
    def transpose(strings: Tuple[str]) -> Iterable[str]:
        return map(
            concat, 
            zip_longest(*strings, fillvalue=' ')
        )
    regexp = re.compile(r'[\[\]]')
    
    exposed_contents = filterfalse(
        regexp.search, 
        transpose(crate_stacks)
    )
    contents = (
        ec.lstrip() 
        for ec 
        in exposed_contents 
        if not ec.isspace()
    )
    return contents

def rearrange_by(contents: Tuple[Deque[str]], instr: str) -> Tuple[Deque[str]]:
    """Pop from and push to stacks a number of times, specified by instructions."""
    
    def parse(string: str) -> Iterable[str]:
        return filter(
            str.isdigit, 
            string.split(" ")
        )
    def index_by_substr(substr: str, strings: Tuple[Deque[str]]) -> int:
        for index, s in enumerate(strings):
            if substr in s:
                return index
            
    reps, position, new_pos = parse(instr)
    
    i, new_i = (
        index_by_substr(p, contents)    # A bit unncessary but works with arbitrary string labels.
        for p 
        in (position, new_pos)
    )
    for _ in range(int(reps)):
        contents[new_i].appendleft(contents[i].popleft())
        
    return contents

concat = partial(''.join)    # I just don't like how the standard way of string concatenation looks.

if __name__ == '__main__':
    
    with open("C:/Users/eerou/Desktop/input.txt") as file:
        
        top_crates = solution(file)
        print(top_crates)

