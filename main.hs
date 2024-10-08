width = 8
height = 8

_numbers = [
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


replace_n :: [Int] -> Int -> Int -> [Int]
replace_n (el:lst) n val    
                            | (n < 0) = (el:lst)
                            | (n == 0) = (val:lst)
                            | otherwise = (el:(replace_n lst (n-1) val))

replace_mtx :: [[Int]] -> Int -> Int -> Int -> [[Int]]
replace_mtx (arr:mtx) x y val   
                                | (y == 0) = ((replace_n arr x val):mtx)
                                | otherwise = (arr:(replace_mtx mtx x (y-1) val))




print_line :: [[Int]] -> Int -> Int -> String
print_line numbers x y  
                | (x >= width-1) = (" " ++ (show (numbers !! y !! x)) ++ " ")
                | ((groups !! y !! x) /= (groups !! y  !! (min (width-1) (x+1)))) = ((" " ++ (show (numbers !! y !! x)) ++ " |") ++ (print_line numbers (x+1) y))
                | otherwise = (" " ++ (show (numbers !! y !! x)) ++ "  ") ++ (print_line numbers (x+1) y)

print_line_up_wall :: Int -> Int -> String
print_line_up_wall x y  
                        | (x >= width) = "+" 
                        | (y == 0 || y == height || (groups !! y !! x) /= (groups !! (max 0 (y-1)) !! x)) = ("+---" ++ (print_line_up_wall (x+1) y))
                        | otherwise = ("+   " ++ (print_line_up_wall (x+1) y))

print_table_rec :: [[Int]] -> Int -> String
print_table_rec numbers y   
                    | (y >= height-1) = (print_line_up_wall 0 y) ++ "\n" ++  "|" ++ (print_line numbers 0 y) ++ "|" ++ "\n" ++ (print_line_up_wall 0 (y+1))
                    | otherwise = (print_line_up_wall 0 y) ++ "\n" ++ "|" ++ (print_line numbers 0 y) ++ "|" ++ "\n" ++ (print_table_rec numbers (y+1))

print_table :: [[Int]] -> String
print_table numbers = (print_table_rec numbers 0)






already_in_group_rec :: [[Int]] -> Int -> Int -> Int -> Int -> Bool
already_in_group_rec numbers group_name n x y   
                                        | (y >= height) = False
                                        | (x >= width) = (already_in_group_rec numbers group_name n 0 (y+1))
                                        | ((groups !! y !! x) == group_name && (numbers !! y !! x) == n) = True
                                        | otherwise = (already_in_group_rec numbers group_name n (x+1) y)

already_in_group :: [[Int]] -> Int -> Int -> Bool
already_in_group numbers group_name n = (already_in_group_rec numbers group_name n 0 0)




solve_puzzle_rec :: [[Int]] -> Int -> Int -> Int -> [[Int]]
solve_puzzle_rec numbers x y n    
                                | (y < 0) = numbers
                                | (x >= width) = (solve_puzzle_rec numbers 0 (y-1) 0)
                                | (n > 0  && (already_in_group numbers (groups !! y !! x) n)) = (solve_puzzle_rec numbers x y (n+1))
                                | (n > 0  && not (already_in_group numbers (groups !! y !! x) n)) = (solve_puzzle_rec (replace_mtx numbers x y n) (x+1) y 0)
                                | ((numbers !! y !! x) == 0) = (solve_puzzle_rec numbers x y 1)
                                | otherwise = (solve_puzzle_rec numbers (x+1) y 0)

solve_puzzle :: [[Int]] -> [[Int]]
solve_puzzle numbers = (solve_puzzle_rec numbers 0 (height-1) 0)







main = (putStr ("KOJUN PUZZLE\n" ++ (print_table _numbers) ++ "\n\nSOLVING..\n\n" ++ (print_table (solve_puzzle _numbers)) ))