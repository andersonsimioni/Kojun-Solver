(setq width 8)
(setq height 8)

(setq _numbers 
  '((0 0 0 0 0 0 0 0)
    (0 1 3 0 0 0 0 0)
    (0 0 0 0 0 3 0 0)
    (0 0 3 0 0 0 0 0)
    (0 5 0 3 0 0 0 0)
    (0 2 0 0 0 0 0 0)
    (0 0 0 0 0 0 3 0)
    (0 0 5 3 0 0 0 0))
)

;Grupos de numeros no tabuleiro, cada grupo possui seu numero
(setq groups 
  '((00 00 01 01 02 03 04 04)
    (00 00 05 01 06 03 03 04)
    (05 05 05 07 06 08 09 09)
    (10 10 10 07 06 08 08 09)
    (11 07 07 07 07 08 08 09)
    (11 12 13 13 13 14 15 09)
    (12 12 12 12 16 14 14 14)
    (17 16 16 16 16 14 18 18))
)

; Cria uma estrutura pra guardar o estado do tabuleiro,
; usado para fazer pilha pro backtracking
(defstruct table-snapshot numbers x y)

; Pega o máximo valorzinho de uma lista
(defun get-max-from-arr (lst n)
  (cond
    ((null lst) n)
    ((> (car lst) n) (get-max-from-arr (cdr lst) (car lst)))
    (t (get-max-from-arr (cdr lst) n))))

; Modifica uma elemento na posicao n de uma lista
(defun replace-n (lst n val)
  (if (< n 0)
      lst
      (if (zerop n)
          (cons val (cdr lst))
          (cons (car lst) (replace-n (cdr lst) (1- n) val)))))

; Modifica um elemento na posicao xy de uma matriz
(defun replace-mtx (mtx x y val)
  (if (zerop y)
      (cons (replace-n (car mtx) x val) (cdr mtx))
      (cons (car mtx) (replace-mtx (cdr mtx) x (1- y) val))))

; Printa os numeros da linha y
(defun print-line (numbers x y)
  (if (>= x (1- width))
      (format nil " ~A " (nth x (nth y numbers)))
      (if (/= (nth x (nth y groups)) (nth (1+ x) (nth y groups)))
          (format nil " ~A |~A" (nth x (nth y numbers)) (print-line numbers (1+ x) y))
          (format nil " ~A  ~A" (nth x (nth y numbers)) (print-line numbers (1+ x) y)))))

; Printa as paredes de cima, casa elemento x y
; Printa a tabela interia de forma recursiva
(defun print-line-up-wall (x y)
  (cond
    ((>= x width) "+")
    ((or (= y 0) (= y height)
         (/= (nth x (nth y groups))
             (nth x (nth (max 0 (- y 1)) groups)))
      )
     (format nil "+---~A" (print-line-up-wall (1+ x) y))
    )
    (t (format nil "+   ~A" (print-line-up-wall (1+ x) y)))))

(defun print-table-rec (numbers y)
  (if (>= y (1- height))
      (format nil "~A~%|~A|~%~A" (print-line-up-wall 0 y) (print-line numbers 0 y) (print-line-up-wall 0 (1+ y)))
      (format nil "~A~%|~A|~%~A" (print-line-up-wall 0 y) (print-line numbers 0 y) (print-table-rec numbers (1+ y)))))

; Apenas chamada print_table_rec com os parametros corretos,
; essa funcao é pra simplificar na hora de printar a tabela
(defun print-table (numbers)
  (print-table-rec numbers 0))

; Verifica se um n já está em um grupo
(defun already-in-group-rec (numbers group-name n x y)
  (cond ((>= y height) nil)
        ((>= x width) (already-in-group-rec numbers group-name n 0 (+ y 1)))
        ((and (= (nth y (nth x groups)) group-name) (= (nth y (nth x numbers)) n)) t)
        (t (already-in-group-rec numbers group-name n (+ x 1) y))
  )
)

; Mesmo esquema da print_table, chama already_in_group_rec, serve para simplificar
(defun already-in-group (numbers group-name n)
  (already-in-group-rec numbers group-name n 0 0))

; Retorna a altura de um grupo
(defun get-group-height (group x y miny maxy)
  (cond ((>= y height) (+ 1 (- maxy miny)))
        ((>= x width) (get-group-height group 0 (+ y 1) miny maxy))
        ((and (= (nth x (nth y groups)) group) (< y miny) (> y maxy))
         (get-group-height group (+ x 1) y y y))
        ((and (= (nth x (nth y groups)) group) (> y maxy))
         (get-group-height group (+ x 1) y miny y))
        ((and (= (nth x (nth y groups)) group) (< y miny))
         (get-group-height group (+ x 1) y y maxy))
        (t (get-group-height group (+ x 1) y miny maxy))
  )
)

; Retorna quantidade celulas de um grupo
(defun get-group-len (group x y)
  (cond ((>= y height) 0)
        ((>= x width) (get-group-len group 0 (+ y 1)))
        ((= (nth x (nth y groups)) group) (+ 1 (get-group-len group (+ x 1) y)))
        (t (get-group-len group (+ x 1) y))
  )
)

; Verificação regra adjacente
(defun adj-check (numbers x y n)
  (cond ((and (> y 0)
              (>= n (nth x (nth (1- y) numbers)))
              (= (nth x (nth y groups)) 
                 (nth x (nth (1- y) groups)))) nil)
        ((and (< y (1- height))
              (<= n (nth x (nth (1+ y) numbers)))
              (= (nth x (nth y groups)) 
                 (nth x (nth (1+ y) groups)))) nil)
        (t t)
  )
)

; Verificação regra ortogonal
(defun ortg-check (numbers x y n)
  (cond ((and (> x 0) (= n (nth (1- x) (nth y numbers)))) nil)
        ((and (< x (1- width)) (= n (nth (1+ x) (nth y numbers)))) nil)
        ((and (> y 0) (= n (nth x (nth (1- y) numbers)))) nil)
        ((and (< y (1- height)) (= n (nth x (nth (1+ y) numbers)))) nil)
        (t t)
  )
)

; Verifica se pode colocar N em X Y
(defun is-n-ok (numbers x y n chk-in-group)
  (cond ((> n (get-group-len (nth x (nth y groups)) 0 0)) nil)
        ((and chk-in-group 
              (already-in-group numbers (nth x (nth y groups)) n)) nil)
        ((not (ortg-check numbers x y n)) nil)
        ((not (adj-check numbers x y n)) nil)
        (t t)
  )
)

; Procura quais n podem ser colocados na posição XY
; no caso retorna as possibilidades pra XY
(defun find-n-list-to-pos (numbers x y n lst)
  (cond 
    ((or (>= y height) (>= x width)) ())
    ((> (nth x (nth y numbers)) 0) '())
    ((> n (get-group-len (nth x (nth y groups)) 0 0)) lst)
    ((is-n-ok numbers x y n t)
    (find-n-list-to-pos numbers x y (1+ n) (cons n lst)))
    (t (find-n-list-to-pos numbers x y (1+ n) lst))
  )
)

; Verifica se tem mais de 1 possibilidade pra XY
(defun can-fill-cell (numbers x y)
  (> (length (find-n-list-to-pos numbers x y 0 '())) 0)
)

; Verifica se o tabuleiro é válido
(defun puzzle-is-valid (numbers x y)
  (cond ((>= y height) t)
        ((>= x width) (puzzle-is-valid numbers 0 (1+ y)))
        ((= (nth x (nth y numbers)) 0) nil)
        ((not (is-n-ok numbers x y (nth x (nth y numbers)) nil)) nil)
        (t (puzzle-is-valid numbers (1+ x) y))
  )
)

; Verifica se o grupo é vertical, se altura = numero de elementos
; pois aí podemos preencher no inicio
(defun is-vertical-group (group)
  (= (- (get-group-height group 0 0 1000 0) (get-group-len group 0 0)) 0)
)

; Resolve os grupos verticais que tem apenas 1 possibilidade
(defun solve-vertical-groups (numbers x y)
  (cond 
    ((>= y height) numbers)
    ((>= x width) (solve-vertical-groups numbers 0 (1+ y)))
    
    ((and (is-vertical-group (nth x (nth y groups))) (= (nth x (nth y numbers)) 0))
     (solve-vertical-groups (replace-mtx numbers x y (get-max-from-arr (find-n-list-to-pos numbers x y 0 '()) 0)) (1+ x) y)
    )
      
    (t (solve-vertical-groups numbers (1+ x) y))
  )
)

; Resolve células que tem apenas 1 possibilidade de N
(defun solve-one-possibilities (numbers x y n)
  (cond ((and (>= y height) (> n 0)) (solve-one-possibilities numbers 0 0 (1- n)))
        ((>= y height) numbers)
        ((>= x width) (solve-one-possibilities numbers 0 (1+ y) n))
        ((= (length (find-n-list-to-pos numbers x y 0 '())) 1)
         (solve-one-possibilities
          (replace-mtx numbers x y (nth 0 (find-n-list-to-pos numbers x y 0 '())))
          (1+ x) y n))
        (t (solve-one-possibilities numbers (1+ x) y n))
  )
)

; Cria X-possibilities TableSnapshot, cria uma nova TableSnapshot para cada
; possibilidade, salvando coordenadas de cada simulação e a tabela de numbers
(defun append-pos (lst possibilities numbers x y)
  (cond
    ((null possibilities) lst)  ; Se não há mais possibilidades, retornamos a lista atual
    (t
     (let* (
              (possibility (car possibilities))
              ; Atualize as coordenadas x e y para a nova posição
              (new-x (if (>= x (length (car numbers))) 0 (+ x 1)))
              (new-y (if (>= x (length (car numbers))) (+ y 1) y))
              ; Crie um novo instantâneo da mesa com as novas coordenadas
              (replace (make-table-snapshot :numbers (replace-mtx numbers x y possibility) :x new-x :y new-y))
            )
            
       (append-pos (cons replace lst) (cdr possibilities) numbers x y))
    )
  )
)

; resolve usando backtracking e pilha, vai jogando as novas possibilidades na pilha
(defun solve-puzzle (snapshots n)
  (let* (
            (snapshot (car snapshots))
            (numbers (table-snapshot-numbers snapshot))
            (x (table-snapshot-x snapshot))
            (y (table-snapshot-y snapshot))
            (valid (puzzle-is-valid numbers 0 0))
            (possibilities (find-n-list-to-pos numbers x y 0 '()))
            (zero-possibilities (zerop (length possibilities)))
            (width (length (car numbers)))
        )

       (cond
        (valid numbers)  ; Se a configuração atual é válida, retorne os números
        
        ((>= x width)
          (solve-puzzle (cons (make-table-snapshot :numbers numbers :x 0 :y (+ y 1))
                            (cdr snapshots)) (1+ n)))
        
        ; Verifique se o elemento já está preenchido e pode ser pulado
        ((not (zerop (nth x (nth y numbers))))
          (solve-puzzle 
            (cons (make-table-snapshot :numbers numbers :x (1+ x) :y y) (cdr snapshots))
            (1+ n)
          )
        )
        
        ; Se houver possibilidades, add novos snapshots à pilha
        ((not zero-possibilities) (solve-puzzle (append-pos (cdr snapshots) possibilities numbers x y) (1+ n)))
        
        ; Se não há mais jogadas possíveis, continue com o próximo snapshot
        (zero-possibilities (solve-puzzle (cdr snapshots) (1+ n)))

      )
  )
)





(defun main ()
  (format t "KOJUN PUZZLE~%~A~%~%SOLVING..~%~A~%"
          (print-table _numbers)
          (print-table (solve-puzzle (list (make-table-snapshot :_numbers (solve-one-possibilities (solve-vertical-groups _numbers 0 0) 0 0 (* width * height * 10))) :x 0 :y 0) 0))
  )
)

;(write (main))

;(write (print-table _numbers)); - OK

;(write (get-group-len 0 0 0)) - OK

;(write (get-group-height 0 0 0 0 0)) - OK

;(write (already-in-group _numbers 0 3)) - OK

;(write (adj-check _numbers 1 0 0)) - OK

;(write (ortg-check _numbers 2 1 1)) - OK

;(write (is-n-ok _numbers 2 2 2 t)) - OK

;(write (find-n-list-to-pos _numbers 2 2 0 ())) - OK

;(write (can-fill-cell _numbers 0 0)) - OK

;(write (puzzle-is-valid _numbers 0 0)) - OK

;(write (is-vertical-group 06)) - OK

;(write (print-table (solve-vertical-groups _numbers 0 0))) ; - OK

;(write (print-table (solve-one-possibilities (solve-vertical-groups _numbers 0 0) 0 0 20))) ; - OK

(setq semi-solved 
  (solve-one-possibilities (solve-vertical-groups _numbers 0 0) 0 0 20)
)

(setq fstack 
  (list (make-table-snapshot
    :numbers semi-solved
    :x 0
    :y 0
  ))
)

(setq solved (solve-puzzle fstack 0) )

(write (print-table solved))

(setq s1 (append-pos fstack (list 4 3) _numbers 0 0))
(setq s2 (append-pos s1 (list 5 6) _numbers 1 0))

;(write s2)

;(write (print-table solved))

;(write (append-pos fstack (list 1 2 3) _numbers 0 0))







