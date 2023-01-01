# ### PROBLEM DESCRIPTION
#
# In this puzzle, you are given a log file that lists commands to ($)
# and output from a computer. Commands either change directory (cd) or
# list its contents (ls). Numbers represent file sizes. Find directories
# under the size of 100000 and sum up their sizes.
#
# Excerpt from a log file. More info at https://adventofcode.com/2022/day/7.
#
# $ cd /
# $ ls
# dir a
# 14848514 b.txt
# 8504156 c.dat
# dir d
# $ cd a
# $ ls
# dir e
# 29116 f
#
#

import json
from collections import deque
from functools import reduce
from typing import Dict, List, Tuple, TypeVar

T = TypeVar('T')

def solution(file):
    """Solve the first problem of Advent of Code 2022 Day 7.
    
    First, reconstruct the filesystem. Then, traverse the filesystem 
    recursively, making note if at any point a directory smaller 
    than or equal to 100000 is found. Finally, sum up the sizes
    of those directories.
    """
    
    lines = [l.rstrip() for l in file]
    
    filesystem = reconstruct_from(lines)
    
    # Print out the filesystem for fun.
    print(json.dumps(filesystem, sort_keys=True, indent=4), "\n")
    
    _, dir_sizes = scan(filesystem, 100000)
    
    return sum(dir_sizes)

def reconstruct_from(lines: List[str]) -> Dict:
    """Reconstruct the filesystem from a logfile."""
    
    filesystem = {}
    path = []    # Helper variable to keep track of position in the filesystem.
    
    for line in lines:
        
        if line.startswith("$ cd"):
            change_directory(filesystem, path, line.split())

        elif "$" not in line:
            populate(filesystem, path, line.split())
        
    return filesystem

def change_directory(filesystem: Dict, path: List[str], words: List[str]) -> None:
    """Move in the filesystem by manipulating the path variable.
    
    If the cd command moved to directory that was not listed by "$ ls", 
    it is reconstructed here.
    """
    
    cwd = query(filesystem, path)
    
    _, _, directory = words
    
    if directory not in cwd.keys():
        
        if directory == "..":
            path.pop()
            return
        
        cwd[directory] = {}
    
    path.append(directory)
    
    pass

def populate(filesystem: Dict, path: List[str], words: List[str]) -> None:
    """Reconstruct contents listed by the ls command."""

    cwd = query(filesystem, path)
    
    content, name = words
    
    if content == "dir":
        cwd[name] = {}

    elif content.isdigit():
        cwd[name] = int(content)
        
    pass

def query(filesystem: Dict[str, T], path: List[str]) -> T:
    """Helper function to return the current working directory"""

    return reduce(
        dict.get, 
        path, 
        filesystem
    )

def scan(filesystem: Dict, max_size: int, dir_sizes: List[int] = None) -> Tuple[int, List[int]]:
    """Recursively list directory sizes under a maximum value."""
    
    if dir_sizes is None:
        dir_sizes = []
    
    total_memory = 0
    
    for k, v in filesystem.items():
    
        if isinstance(v, Dict):
            memory, dir_sizes = scan(v, max_size, dir_sizes)
            total_memory += memory
            
        else:    
            total_memory += v
    
    if total_memory <= max_size: 
        dir_sizes.append(total_memory)
    
    return total_memory, dir_sizes

if __name__ == '__main__':

    with open("Path/File.txt") as file:
        
        dir_sizes = solution(file)
        print(dir_sizes)

