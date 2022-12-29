# ### PROBLEM DESCRIPTION
#
# A start-of-message marker is a substring where all characters are 
# different. Given a string, how many characters have 
# to be processed until a marker of length 14 is found?
#
# More info at https://adventofcode.com/2022/day/6.
#

def solution(file):
    """Solve the second problem of Advent of Code 2022 Day 6.
    
    Scan the signal with a moving window of length 14. Converting to set 
    removes duplicates, so compare lengths of window and set of elements
    in the window. If lengths match, all elements are unique and the
    solution is found.
    """
    
    signal = file.read()
    window_size = 14    # Just change the window size.
    
    i = 0
    while i < len(signal) - window_size + 1:

        last_sample_processed = i + window_size

        window = signal[i : last_sample_processed]

        if len(window) == len(set(window)):
            return last_sample_processed

        i += 1
    
    return "Could not find a start-of-packet marker."

if __name__ == '__main__':
    
    with open("Path/File.txt") as file:
        
        marker_end = solution(file)
        print(marker_end)

