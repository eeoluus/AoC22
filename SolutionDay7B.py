# ### PROBLEM DESCRIPTION
#
# This is pretty similar to the first problem. As a recap...
#
# "In this puzzle, you are given a log file that lists commands to ($)
# and output from a computer. Commands either change directory (cd) or
# list its contents (ls). Numbers represent file sizes."" 
# 
# The difference being that we must now find (the size of) a directory which,
# if deleted, would free up enough to space to run an update of size 30000000.
# The total capacity of the device is 70000000.
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
    """Solve the second problem of AoC 2022 Day 7.
    
    First, reconstruct the filesystem (recursively). Then, traverse the filesystem 
    recursively, listing all directory sizes. Find the directory whose deletion
    would free up enough space to run the update.
    
    (Only the main function had to be tweaked, otherwise similar to the tryhard solution.)
    """
    
    lines = deque([l.rstrip() for l in file])
    
    filesystem = reconstruct_from(lines)

    # Print out the filesystem for fun.
    print(json.dumps(filesystem, sort_keys=True, indent=4), "\n")

    capacity = 70000000
    required_space = 30000000

    memory_used, dir_sizes = scan(filesystem, capacity)
    
    memory_left = capacity - memory_used
    
    to_be_deleted = required_space - memory_left
    
    big_dirs = [
        d 
        for d in dir_sizes 
        if d > to_be_deleted
    ]
    
    return min(big_dirs)

def reconstruct_from(lines: List[str], filesystem: Dict = None, path: List[str] = None) -> Dict:
    """Recursively reconstruct the filesystem from a logfile."""
    
    if filesystem is None:    # Initialize filesystem and path as empty upon first call. 
        filesystem = {}
        
    if path is None:
        path = []

    while lines:
        if lines[0].startswith("$ cd"):
            branch = change_directory    # Each branch pops from lines before recursing back.
            
        elif "$" not in lines[0]:
            branch = populate
            
        else:    # Need to explicitly handle all cases unlike in the loop version.
            branch = ignore
        
        return branch(lines, filesystem, path)
    
    else:
        return filesystem
    
def ignore(lines: List[str], filesystem: Dict, path: List[str]) -> Dict:
    """No need to do anything on "$ ls"."""
    
    lines.popleft()
    
    return reconstruct_from(lines, filesystem, path)
        
def change_directory(lines: List[str], filesystem: Dict, path: List[str]) -> Dict:
    """Move in the filesystem by manipulating the path variable.
    
    If the cd command moved to directory that was not listed by "$ ls", 
    it is reconstructed here.
    """
    
    cwd = query(filesystem, path)
    
    _, _, directory = lines.popleft().split()
    
    if directory not in cwd.keys():
        
        if directory == "..":
            path.pop()
            
            return reconstruct_from(lines, filesystem, path)
                    
        cwd[directory] = {}
    
    path.append(directory)
    
    return reconstruct_from(lines, filesystem, path)

def populate(lines: List[str], filesystem: Dict, path: List[str]) -> Dict:
    """Reconstruct contents listed by the ls command."""

    cwd = query(filesystem, path)
    
    content, name = lines.popleft().split()
    
    if content == "dir":
        cwd[name] = {}

    elif content.isdigit():
        cwd[name] = int(content)
    
    return reconstruct_from(lines, filesystem, path)
    
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
    
    memory_used = 0
    
    for k, v in filesystem.items():
    
        if isinstance(v, Dict):
            memory, dir_sizes = scan(v, max_size, dir_sizes)
            memory_used += memory
            
        else:    
            memory_used += v
    
    if memory_used <= max_size: 
        dir_sizes.append(memory_used)
    
    return memory_used, dir_sizes

if __name__ == '__main__':

    with open("Path/File.txt") as file:
        
        del_size = solution(file)
        print(del_size)

