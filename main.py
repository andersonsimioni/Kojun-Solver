import random

#8x8
width = 8
height = 8
#numbers = [[0 for x in range(width)] for y in range(height)]
#groups = [['' for x in range(width)] for y in range(height)]
numbers = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 1, 3, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 3, 0, 0],
    [0, 0, 3, 0, 0, 0, 0, 0],
    [0, 5, 0, 3, 0, 0, 0, 0],
    [0, 2, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 3, 0],
    [0, 0, 5, 3, 0, 0, 0, 0]
]
groups = [
    [ 0, 0, 1, 1, 2, 3, 4, 4],
    [ 0, 0, 5, 1, 6, 3, 3, 4],
    [ 5, 5, 5, 7, 6, 8, 9, 9],
    [10,10,10, 7, 6, 8, 8, 9],
    [11, 7, 7, 7, 7, 8, 8, 9],
    [11,12,13,13,13,14,15, 9],
    [12,12,12,12,16,14,14,14],
    [17,16,16,16,16,14,18,18]
   ]

def print_table():
    for y in range(len(numbers)):
                
        for x in range(len(numbers[y])):
            print(f'+-----' if y==0 or groups[y][x] != groups[max(0, y-1)][x] else '+     ', end="")
            if(x == len(numbers[y])-1): print("+", end="")
        
        print("\n|", end="")
        
        for x in range(len(numbers[y])):
            print(f' {numbers[y][x]:2} ', '|' if x == len(numbers[y])-1 or groups[y][x] != groups[y][min(width-1, x+1)] else ' ', end="")

        print()
            
    print('+-----' * width + '+')


def already_in_group(group_name, n):
    for y in range(len(groups)):
        for x in range(len(groups)):
            if groups[y][x] == group_name and numbers[y][x] == n: return True
    
    return False

def solve_puzzle_rec():
    for y in range(len(numbers)-1, -1, -1):
        for x in range(len(numbers)):
            if(numbers[y][x] == 0):
                n = 1
                while already_in_group(groups[y][x], n): n+=1
                numbers[y][x] = n

print("Kojun")
print_table()
solve_puzzle_rec()
print("puzzle solved!")
print_table()