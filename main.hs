width = 8
height = 8

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



print_line :: Int -> Int -> String
print_line x y  | (x >= width-1) = (" " ++ (show (numbers !! y !! x)) ++ " ")
                | ((groups !! y !! x) /= (groups !! y  !! (min (width-1) (x+1)))) = ((" " ++ (show (numbers !! y !! x)) ++ " |") ++ (print_line (x+1) y))
                | otherwise = (" " ++ (show (numbers !! y !! x)) ++ "  ") ++ (print_line (x+1) y)

print_line_up_wall :: Int -> Int -> String
print_line_up_wall x y  | (x >= width) = "+" 
                        | (y == 0 || y == height || (groups !! y !! x) /= (groups !! (max 0 (y-1)) !! x)) = ("+---" ++ (print_line_up_wall (x+1) y))
                        | otherwise = ("+   " ++ (print_line_up_wall (x+1) y))

print_table_rec :: Int -> String
print_table_rec y   | (y >= height-1) = (print_line_up_wall 0 y) ++ "\n" ++  "|" ++ (print_line 0 y) ++ "|" ++ "\n" ++ (print_line_up_wall 0 (y+1))
                    | otherwise = (print_line_up_wall 0 y) ++ "\n" ++ "|" ++ (print_line 0 y) ++ "|" ++ "\n" ++ (print_table_rec (y+1))

print_table ::  String
print_table = (print_table_rec 0)



main = (putStr print_table)