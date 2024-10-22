import System.Random

randomInt :: Int -> Int -> Int -> Int
randomInt seed min max = 
    let g = mkStdGen (seed)
        (value, _) = randomR (min, max) g
    in value
    

width = 8
height = 8

--Matrix de numeros iniciais
--configuração inicial do tabuleiro
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

--Grupos de numeros no tabuleiro,
--cada grupo possui seu numero
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

get_arr_len :: [Int] -> Int
get_arr_len [] = 0
get_arr_len (h:lst) = 1 + (get_arr_len lst)


get_random_from_arr :: [Int] -> Int -> Int
get_random_from_arr arr seed = (arr!!((randomInt seed 1 (length arr))-1))


get_max_from_arr :: [Int] -> Int -> Int
get_max_from_arr (el:lst) n
                        | (lst == [] && n>el) = n
                        | (lst == [] && n<el) = el
                        | (el > n) = (get_max_from_arr lst el)
                        | (otherwise) = (get_max_from_arr lst n)


--Modifica uma elemento na posicao n de uma lista
replace_n :: [Int] -> Int -> Int -> [Int]
replace_n (el:lst) n val    
                            | (n < 0) = (el:lst)
                            | (n == 0) = (val:lst)
                            | otherwise = (el:(replace_n lst (n-1) val))

--Modifica um elemento na posicao xy de uma matriz,
--modifica a lista na posicao y na matriz usando a funcao replace_n
replace_mtx :: [[Int]] -> Int -> Int -> Int -> [[Int]]
replace_mtx (arr:mtx) x y val   
                                | (y == 0) = ((replace_n arr x val):mtx)
                                | otherwise = (arr:(replace_mtx mtx x (y-1) val))



--Printa os numeros da linha y
print_line :: [[Int]] -> Int -> Int -> String
print_line numbers x y  
                --Printa ultimo numero da linha pois nao tem parede
                | (x >= width-1) = (" " ++ (show (numbers !! y !! x)) ++ " ")
                --Caso haja parede printa | para separar os grupos
                | ((groups !! y !! x) /= (groups !! y  !! (min (width-1) (x+1)))) = ((" " ++ (show (numbers !! y !! x)) ++ " |") ++ (print_line numbers (x+1) y))
                | otherwise = (" " ++ (show (numbers !! y !! x)) ++ "  ") ++ (print_line numbers (x+1) y)

--Printa as paredes de cima, caso o elemento x y-1 seja do mesmo grupo nao printa parede, deixa vazio,
-- no inicio e final da tabela printa as paredes de borda
print_line_up_wall :: Int -> Int -> String
print_line_up_wall x y  
                        | (x >= width) = "+" 
                        | (y == 0 || y == height || (groups !! y !! x) /= (groups !! (max 0 (y-1)) !! x)) = ("+---" ++ (print_line_up_wall (x+1) y))
                        | otherwise = ("+   " ++ (print_line_up_wall (x+1) y))

--Printa a tabela inteira de forma recursiva chamando print_line e print_line_up_wall,
print_table_rec :: [[Int]] -> Int -> String
print_table_rec numbers y   
                    | (y >= height-1) = (print_line_up_wall 0 y) ++ "\n" ++  "|" ++ (print_line numbers 0 y) ++ "|" ++ "\n" ++ (print_line_up_wall 0 (y+1))
                    | otherwise = (print_line_up_wall 0 y) ++ "\n" ++ "|" ++ (print_line numbers 0 y) ++ "|" ++ "\n" ++ (print_table_rec numbers (y+1))

--Apenas chamada print_table_rec com os parametros corretos,
-- essa funcao é pra simplificar na hora de printar a tabela
print_table :: [[Int]] -> String
print_table numbers = (print_table_rec numbers 0)










--Verifica se um n já está em um grupo
already_in_group_rec :: [[Int]] -> Int -> Int -> Int -> Int -> Bool
already_in_group_rec numbers group_name n x y   
                                        | (y >= height) = False --Final da tabela, nao tem mais elementos
                                        | (x >= width) = (already_in_group_rec numbers group_name n 0 (y+1)) --passa para proxima linha
                                        | ((groups !! y !! x) == group_name && (numbers !! y !! x) == n) = True --encontrou um elemento = n
                                        | otherwise = (already_in_group_rec numbers group_name n (x+1) y) --elemento != n pode passar pro proximo ao lado

--Mesmo esquema da print_table, chama already_in_group_rec, serve para simplificar
already_in_group :: [[Int]] -> Int -> Int -> Bool
already_in_group numbers group_name n = (already_in_group_rec numbers group_name n 0 0)



get_group_heigth :: Int -> Int -> Int -> Int -> Int -> Int
get_group_heigth group x y miny maxy
                                    | (y >= height) = (maxy - miny + 1)
                                    | (x >= width) = (get_group_heigth group 0 (y+1) miny maxy)
                                    
                                    | ((groups!!y!!x)==group && y<miny && y>maxy) = (get_group_heigth group (x+1) y y y)
                                    | ((groups!!y!!x)==group && y>maxy) = (get_group_heigth group (x+1) y miny y)
                                    | ((groups!!y!!x)==group && y<miny) = (get_group_heigth group (x+1) y y maxy)
                                    
                                    | otherwise = (get_group_heigth group (x+1) y miny maxy)
                                    
                                    


get_group_len :: Int -> Int -> Int -> Int
get_group_len group x y
                            | (y >= height) = 0 --Final da tabela, nao tem mais elementos
                            | (x >= width) = (get_group_len group 0 (y+1))
                            | ((groups !! y !! x) == group) = (1 + (get_group_len group (x+1) y))
                            | (otherwise) = (get_group_len group (x+1) y)


adj_check :: [[Int]] -> Int -> Int -> Int -> Bool
adj_check numbers x y n
                        --Verifica el acima - OK - testado!
                        | ( y>0 &&  n>=(numbers!!(y-1)!!x) && ((groups!!y!!x)==(groups!!(y-1)!!x)) ) = False
                        
                        --Verifica el abaixo - OK - testado!
                        | (y<(height-1) &&   n<=(numbers!!(y+1)!!x) && ((groups!!y!!x)==(groups!!(y+1)!!x)) ) = False
                        
                        --tudo certo,
                        | otherwise = True
                        

ortg_check :: [[Int]] -> Int -> Int -> Int -> Bool
ortg_check numbers x y n
                        --Verifica el esquerdo
                        | (x>0 &&   n==(numbers!!y!!(x-1)) ) = False
                        --Verifica el direito
                        | (x<(width-1) && n==(numbers!!y!!(x+1)) ) = False
                        
                        
                        --Verifica el acima -
                        | (y>0 &&  n==(numbers!!(y-1)!!x) ) = False
                        --Verifica el abaixo -
                        | (y<(height-1) &&   n==(numbers!!(y+1)!!x) ) = False
                        
                        
                        | otherwise = True

is_n_ok :: [[Int]] -> Int -> Int -> Int -> Bool -> Bool
is_n_ok numbers x y n chk_in_group
                        | (n > (get_group_len (groups!!y!!x) 0 0)) = False
                        | (chk_in_group == True && (already_in_group numbers (groups!!y!!x) n)) = False
                        | (not (ortg_check numbers x y n)) = False
                        | (not (adj_check numbers x y n)) = False
                        | otherwise = True



find_n_list_to_pos :: [[Int]] -> Int -> Int -> Int -> [Int] -> [Int]
find_n_list_to_pos numbers x y n lst
                            | ((numbers!!y!!x) > 0) = []
                            | (n>(get_group_len (groups!!y!!x) 0 0) ) = lst
                            
                            | (is_n_ok numbers x y n True) = (find_n_list_to_pos numbers x y (n+1) (n:lst))
                            | (otherwise) = (find_n_list_to_pos numbers x y (n+1) lst)
                            
                            

puzzle_is_valid :: [[Int]] -> Int -> Int -> Bool
puzzle_is_valid numbers x y
                        | (y >= height) = True --Final da tabela, nao tem mais elementos
                        | (x >= width) = (puzzle_is_valid numbers 0 (y+1))
                        | ((numbers!!y!!x) == 0) = False
                        | (not (is_n_ok numbers x y (numbers !! y !! x) False)) = False
                        | otherwise = (puzzle_is_valid numbers (x+1) y)


is_vertical_group :: Int -> Bool
is_vertical_group group = ((get_group_heigth group 0 0 1000 0) - (get_group_len group 0 0) == 0)

fill_cell :: [[Int]] -> Int -> Int -> Int -> [[Int]]
fill_cell numbers x y seed = (replace_mtx numbers x y (get_random_from_arr (find_n_list_to_pos numbers x y 0 []) seed) )
                            

can_fill_cell :: [[Int]] -> Int -> Int -> Bool
can_fill_cell numbers x y = ((length (find_n_list_to_pos numbers x y 0 [])) > 0)


solve_vertical_groups :: [[Int]] -> Int -> Int -> [[Int]]
solve_vertical_groups numbers x y
                                | (y >= height) = numbers
                                | (x >= width) = (solve_vertical_groups numbers 0 (y+1))
                                | ((is_vertical_group (groups!!y!!x)) && (numbers!!y!!x)==0) = (solve_vertical_groups (replace_mtx numbers x y (get_max_from_arr (find_n_list_to_pos numbers x y 0 []) 0)) (x+1) y)
                                | (otherwise) = (solve_vertical_groups numbers (x+1) y)


solve_one_possibilities :: [[Int]] -> Int -> Int -> Int -> [[Int]]
solve_one_possibilities numbers x y n
                                | (y >= height && n>0) = (solve_one_possibilities numbers 0 0 (n-1))
                                | (y >= height) = numbers
                                | (x >= width) = (solve_one_possibilities numbers 0 (y+1) n)
                                | ((length (find_n_list_to_pos numbers x y 0 [])) == 1) = (solve_one_possibilities (replace_mtx numbers x y ((find_n_list_to_pos numbers x y 0 [])!!0)) (x+1) y n)
                                | (otherwise) = (solve_one_possibilities numbers (x+1) y n)
                                




--Resolve nosso PROBLEMAO
--caso n seja > 0 ela entra em uma subrotina pra tentar encontrar um n
--que encaixe dentro do grupo
solve_puzzle_rec :: [[Int]] -> Int -> Int -> Int -> [[Int]]
solve_puzzle_rec numbers x y n    
                                | (y < 0) = numbers --terminou, ultimo elemento da tabela, nao tem mais nada pra resolver
                                | (x >= width) = (solve_puzzle_rec numbers 0 (y-1) 0) --passa pra proxima linha
                                
                                | (n > (get_group_len (groups !! y !! x) 0 0)) = (solve_puzzle_rec numbers (x+1) y 0)
                                
                                --subrotina para encontrar um n novo para o grupo
                                --esse n já existe no grupo, incrementa 1 e tenta novamente.. até encontrar um novo
                                | (n > 0  && not (is_n_ok numbers x y n True)) = (solve_puzzle_rec numbers x y (n+1)) 
                                --encontrou um n que nao esta no grupo, pode setar ele e passar pro elemento ao lado
                                | (n > 0  && (is_n_ok numbers x y n True)) = (solve_puzzle_rec (replace_mtx numbers x y n) (x+1) y 0)
                                
                                --encontrou um elemento = 0, logo ele precisa entrar na 
                                --subrotina para encontrar um n novo no grupo
                                | ((numbers !! y !! x) == 0) = (solve_puzzle_rec numbers x y 1)
                                
                                --nenhum caso especial, passa pro elemento do lado pra continuar o preenchimento
                                | otherwise = (solve_puzzle_rec numbers (x+1) y 0)

--Chama a funcao solve_puzzle_rec de forma simplificada
solve_puzzle :: [[Int]] -> [[Int]]
solve_puzzle numbers = (solve_puzzle_rec numbers 0 (height-1) 0)




                                


solve_puzzle_2 :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
solve_puzzle_2 original numbers x y seed n
                            | (n > 1000000) = numbers
                            | (y >= height) = numbers
                            | (x >= width) = (solve_puzzle_2 original numbers 0 (y+1) (seed+1) n)
                            
                            | ((numbers!!y!!x) > 0) = (solve_puzzle_2 original numbers (x+1) y (seed+1) n)
                            | (can_fill_cell numbers x y) = (solve_puzzle_2 original (fill_cell numbers x y seed) x y (seed+1) n)
                            
                            | otherwise = (solve_puzzle_2 original original 0 0 (seed+1) (n+1))
                            
                            



-- [[numbers]] x y
data TableSnapshot = TableSnapshot
  { numbers :: [[Int]]
  , x :: Int
  , y :: Int
  } deriving (Show) 

--Cria X-possibilities TableSnapshot, cria uma nova TableSnapshot para cada
-- possibilidade, salvando coordenadas de cada simulacao e a tabela de numeros
append_pos :: [TableSnapshot] -> [Int] -> [[Int]] -> Int -> Int -> [TableSnapshot]
append_pos (item:lst) [] numbers x y = lst
append_pos (item:lst) (possibility:possibilities) numbers x y
                                                    | (y>=height) = (append_pos (item:lst) possibilities numbers x y)
                                                    | (x>=width) = (append_pos (item:replace_new_line:lst) possibilities numbers x y)
                                                    | (otherwise) = (append_pos (item:replace:lst) possibilities numbers x y)
                                                    where
                                                        replace_new_line = (TableSnapshot (replace_mtx numbers 0 (y+1) possibility) 0 (y+1))
                                                        replace = (TableSnapshot (replace_mtx numbers x y possibility) (x+1) y)


solve_puzzle_3 :: [TableSnapshot] -> Int -> Int -> [[Int]] --[TableSnapshot]
solve_puzzle_3 ((TableSnapshot numbers x y):lst) seed n
                            | (valid) = numbers-- [item]
                            -- | (n == 1000) = numbers--(item:lst) --DEBUG
                            
                            | (x >= width) = (solve_puzzle_3 ((TableSnapshot numbers 0 (y+1)):lst) (seed+1) (n+1))
                            -- | (y >= height && valid) = (item:lst)
                            -- | (y >= height && not valid) = (solve_puzzle_3 lst (seed+1) (n+1))
                            
                            | ((numbers!!y!!x)/=0) = (solve_puzzle_3 ((TableSnapshot numbers (x+1) y):lst) (seed+1) (n+1))
                            
                            | (not zero_possibilities) = (solve_puzzle_3 (append_pos (item:lst) possibilities numbers x y) (seed+1) (n+1))
                            
                            | (zero_possibilities) = (solve_puzzle_3 lst (seed+1) (n+1))
                            
                            -- | (otherwise) = (solve_puzzle_3 (item:lst) (seed+1) (n+1))
                            where
                                item = (TableSnapshot numbers x y)
                                item_next_line = (TableSnapshot numbers 0 (y+1))
                                valid = (puzzle_is_valid numbers 0 0)
                                next_n = ((length possibilities)-1)
                                zero_possibilities = (length possibilities == 0)
                                possibilities = (find_n_list_to_pos numbers x y 0 [])
                                
                                
                                

main = (putStr (print_table (solve_puzzle_3 [(TableSnapshot (solve_one_possibilities (solve_vertical_groups _numbers 0 0) 0 0 (width*height*10)) 0 0)] 123 0)))
