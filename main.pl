width(8).
height(8).
numbers([
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 1, 3, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 3, 0, 0],
    [0, 0, 3, 0, 0, 0, 0, 0],
    [0, 5, 0, 3, 0, 0, 0, 0],
    [0, 2, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 3, 0],
    [0, 0, 5, 3, 0, 0, 0, 0]
    ]).
groups([
    [ 0, 0, 1, 1, 2, 3, 4, 4],
    [ 0, 0, 5, 1, 6, 3, 3, 4],
    [ 5, 5, 5, 7, 6, 8, 9, 9],
    [10,10,10, 7, 6, 8, 8, 9],
    [11, 7, 7, 7, 7, 8, 8, 9],
    [11,12,13,13,13,14,15, 9],
    [12,12,12,12,16,14,14,14],
    [17,16,16,16,16,14,18,18]
    ]).
    
% pega o min de dois numeros 
get_min(X,Y,X) :- X =< Y.
get_min(X,Y,Y) :- X > Y.

% pega o max de dois numeros 
get_max(X,Y,X) :- X >= Y.
get_max(X,Y,Y) :- X < Y.
    
% pega o elemento da lista
get_element_arr(0,[H|_],H).
get_element_arr(Index,[_|T],El) :-
    Index > 0, 
    Index1 is Index-1,
    get_element_arr(Index1, T, El).

% pega o x,y da matriz
get_element_mtx(Mtx,X,Y,El) :- 
    get_element_arr(Y,Mtx,Row),
    get_element_arr(X,Row,El).


get_max_from_arr([H|T],N,Max) :- 
    (T == [], N > H, !, Max = N);
    (T == [] , N < H ,!, Max = H);
    (H > N, !, get_max_from_arr(T,H,Max));
    get_max_from_arr(T,N,Max).

table_snapshot(_Numbers, _X, _Y).
%--------------------------------------------
% --Modifica uma elemento na posicao n de uma lista
replace_n([_|T],0,Val,[Val|T]).

replace_n(List,N,_,List) :- N < 0.

replace_n([H|T],N,Val,[H|Q]) :-
    N > 0, 
    N1 is N-1,
    replace_n(T,N1,Val,Q). 
    
% --Modifica um elemento na posicao xy de uma matriz,
% --modifica a lista na posicao y na matriz usando a funcao replace_n
replace_mtx([Row|Mtx],X,0,Val,[NewRow|Mtx]) :- 
    replace_n(Row,X,Val,NewRow).
    
replace_mtx([Row|Mtx],X,Y,Val,[Row|NewMtx]) :-
    Y > 0,
    Y1 is Y-1,
    replace_mtx(Mtx,X,Y1,Val,NewMtx).
%--------------------------------------------    
%--Verifica se um n já está em um grupo
already_in_group_rec(Numbers,Group_name,N,X,Y,Res) :-
    % --Final da tabela, nao tem mais elementos
    (height(Height), 
        Y >= Height, 
        !, Res = false
    ) ;
    % --passa para proxima linha
    (width(Width), X >= Width, 
        Y1 is Y + 1, 
        !, already_in_group_rec(Numbers,Group_name,N,0,Y1,Res)
    );
    % --encontrou um elemento = n
    (
        groups(Groups),
        get_element_mtx(Groups, X, Y, ElAux),
        ElAux == Group_name, 
        get_element_mtx(Numbers, X, Y, El2),
        El2 == N, 
        !, Res = true
    );
    % --elemento != n pode passar pro proximo ao lado
    (X1 is X+1, 
        already_in_group_rec(Numbers, Group_name, N, X1, Y, Res)
    ).

% --Mesmo esquema da print_table, chama already_in_group_rec, serve para simplificar  
already_in_group(Numbers,Group_name,N,Res) :-
    already_in_group_rec(Numbers,Group_name,N,0,0,ResAux),
    Res = ResAux.
    
get_group_heigth(Group,X,Y,MinY,MaxY,H) :- 
    (
        height(Height),
        Y >= Height,
        !, H is MaxY - MinY + 1
    );
    (
        width(Width),
        X >= Width,
        Y1 is Y+1, 
        !, get_group_heigth(Group, 0, Y1, MinY, MaxY,H)
    );
    (
        groups(Groups),
        get_element_mtx(Groups,X,Y,El),
        El == Group,
        (
            (
                Y < MinY, 
                Y > MaxY,
                X1 is X+1,
                !, get_group_heigth(Group, X1, Y, Y, Y, H)
            );
            (
                Y > MaxY, 
                X1 is X+1,
                !, get_group_heigth(Group, X1, Y, MinY, Y, H)
            );
            (
                Y < MinY, 
                X1 is X+1,
                !, get_group_heigth(Group, X1, Y, Y, MaxY, H)
            )
        )
    );
    (   X1 is X+1,
        get_group_heigth(Group, X1, Y, MinY, MaxY, H)
    ).
    
%--retorna quantidade celulas de um grupo
get_group_len(Group, X, Y, Len) :- 
    % --Final da tabela, nao tem mais elementos
    (
        height(Height),
        Y >= Height, 
        !, Len = 0
    );
    (
        width(Width),
        X >= Width, 
        Y1 is Y + 1,
        !, get_group_len(Group,0,Y1,Len)
    );
    (
        groups(Groups),
        get_element_mtx(Groups, X, Y, El),
        El == Group,
        X1 is X+1,
        get_group_len(Group, X1, Y, Len1),
        !, Len is Len1 + 1
    );
    (
        X1 is X+1, 
        !, get_group_len(Group, X1, Y, Len)
    ).
    
%--Verificação regra adjacente
adj_check(Numbers, X, Y, N, Check) :- 
    % --Verifica el acima - OK - testado!
    (
        groups(Groups),
        Y > 0, 
        Y1 is Y-1,
        get_element_mtx(Numbers, X, Y1, El), 
        N >= El, 
        get_element_mtx(Groups, X, Y, El1),  
        get_element_mtx(Groups, X, Y1, El2),
        El1 == El2,
        !, Check = false
    );
    % --Verifica el abaixo - OK - testado!
    (
        groups(Groups),
        height(Height),
        HeightAux is Height-1,
        Y < HeightAux,
        Y1 is Y+1,
        get_element_mtx(Numbers, X, Y1, ElAux),
        N =< ElAux, 
        get_element_mtx(Groups, X, Y, El1),
        get_element_mtx(Groups, X, Y1, El2),
        El1 == El2,
        !, Check = false
    );
    Check = true.

%--Verificação regra ortogonal
ortg_check(Numbers, X, Y, N, Check) :- 
    (
        (
            % --Verifica el esquerdo
            (
                X > 0, 
                X1 is X-1,
                get_element_mtx(Numbers,X1, Y, El),
                N == El
            ); 
            % --Verifica el direito
            (
                width(Width),
                Width1 is Width-1,
                X < Width1,
                X1 is X+1,
                get_element_mtx(Numbers, X1, Y, El1),
                N == El1
            );
            % --Verifica el acima
            (
                Y>0, 
                Y1 is Y-1,
                get_element_mtx(Numbers,X, Y1, El2),
                N == El2
            );
            % --Verifica el abaixo
            (
                height(Height),
                HeightAux is Height-1,
                Y < HeightAux,
                Y1 is Y+1,
                get_element_mtx(Numbers, X, Y1, El3),
                N == El3
            )
        ), !, Check = false
    );
    Check = true.
    
%-- Verifica se pode colocar N em X Y
is_n_ok(_Numbers, _X, _Y, 0, _Is_In_Group, false).
is_n_ok(Numbers, X, Y, N, Is_In_Group,ResNOk) :-
    (
        groups(Groups),
        get_element_mtx(Groups, X, Y, El),
        get_group_len(El, 0, 0, Len),
        N > Len,
        !, ResNOk = false
    );

    (
        groups(Groups),
        Is_In_Group == true,
        get_element_mtx(Groups, X, Y, El2),
        already_in_group(Numbers, El2, N, Already),
        Already == true,
        !, ResNOk = false
    );
    (
        ortg_check(Numbers, X, Y, N, Check),
        Check == false,
        !, ResNOk = false
    );
    (
        adj_check(Numbers, X, Y, N, Check1),
        Check1 == false, 
        !, ResNOk = false
    );
    ResNOk = true.

% --Procura quais n podem ser colocados na posição XY,
% --no caso retorna as possibilidades pra XY
find_n_list_to_pos(Numbers, X, Y, N, List, Res) :-
    (
        width(Width), 
        X >= Width, 
        !, Res = []
    );
    (
        height(Height),
        Y >= Height,
        !, Res = []
    );
    (
        get_element_mtx(Numbers,X,Y,El),
        El > 0,
        !, Res = []
    );
    (
        groups(Groups),
        get_element_mtx(Groups,X,Y,El2), 
        get_group_len(El2,0,0,Len),
        N > Len,
        !, Res = List
    );
    (
        is_n_ok(Numbers,X,Y,N,true,IsOk),
        IsOk == true,
        N1 is N+1,
        !,find_n_list_to_pos(Numbers,X,Y,N1,[N|List],Res)
    );
    (
        N1 is N+1,
        !, find_n_list_to_pos(Numbers,X,Y,N1,List,Res)
    ).
    
% --Verifica se o tabuleira é válido
puzzle_is_valid(Numbers,X,Y,IsValid) :-
    (
        height(Height),
        Y >= Height,
        !, IsValid = true
    );
    (
        width(Width),
        X >= Width,
        Y1 is Y+1,
        !,puzzle_is_valid(Numbers,0,Y1,IsValid)
    );
    (
        get_element_mtx(Numbers,X,Y,El),
        El == 0,
        !, IsValid = false
    );
    (
        get_element_mtx(Numbers,X,Y,El),
        is_n_ok(Numbers,X,Y,El,false,Res),
        Res == false,
        IsValid = false
    );
    X1 is X+1,
    puzzle_is_valid(Numbers,X1,Y,IsValid).
    
% --Verifica se o grupo é vertical,
% --se altura = numero de elementos,
% --pois ai podemos preencher no inicio
is_vertical_group(Group, IsVertical) :-
    get_group_heigth(Group,0,0,1000,0,H),
    get_group_len(Group,0,0,Len),
    Aux is H - Len,
    (
        Aux == 0,
        !,IsVertical = true
    );
    !,IsVertical = false.

% --Preenche os grupos verticais que tem apenas 1 possibiliade
solve_vertical_groups(Numbers,X,Y,Solve) :-
    (
        height(Height),
        Y >= Height,
        !,Solve = Numbers
    );
    (
        width(Width),
        X >= Width,
        Y1 is Y+1,
        !,solve_vertical_groups(Numbers,0,Y1,Solve)
    );
    (
        groups(Groups),
        get_element_mtx(Groups,X,Y,El),
        is_vertical_group(El,IsVertical),
        IsVertical == true,
        get_element_mtx(Numbers,X,Y,El2),
        find_n_list_to_pos(Numbers, X, Y, 0, [], List),
        El2 == 0, 
        get_max_from_arr(List,0,Max),
        replace_mtx(Numbers,X,Y,Max,NewMtx),
        X1 is X+1,
        !,solve_vertical_groups(NewMtx,X1,Y,Solve)
    );
    X1 is X+1,
    solve_vertical_groups(Numbers,X1,Y,Solve).
    
% --Resolve células que tem apenas 1 possibilidade de N
solve_one_possibilities(Numbers,X,Y,N,SolvedMtx) :-
    (
        height(Height),
        Y >= Height,
        N > 0,
        N1 is N-1,
        !, solve_one_possibilities(Numbers,0,0,N1,SolvedMtx)
    );
    (
        height(Height),
        Y >= Height,
        !,SolvedMtx = Numbers
    );
    (
        width(Width),
        X >= Width,
        Y1 is Y+1,
        !, solve_one_possibilities(Numbers,0,Y1,N,SolvedMtx)
    );
    (
        find_n_list_to_pos(Numbers,X,Y,0,[],List),
        length(List,Length),
        Length == 1,
        get_element_arr(0,List,El),
        replace_mtx(Numbers,X,Y,El,NewMtx),
        X1 is X+1,
        !,solve_one_possibilities(NewMtx,X1,Y,N,SolvedMtx)
    );
    (
        X1 is X+1,
        !, solve_one_possibilities(Numbers, X1, Y, N, SolvedMtx)
    ).


% --Cria X-possibilities TableSnapshot, cria uma nova TableSnapshot para cada
% -- possibilidade, salvando coordenadas de cada simulacao e a tabela de numeros
append_pos([_|ListApp], [], _Numbers, _X, _Y, ListApp).
append_pos([ItemApp|ListApp], [Poss|PossibList], Numbers, X, Y, NewListApp) :-
    % --corrige a trajetoria, x passou do limite entao pula uma linha, 
    % -- inclui a possibilidade com a trajetoria corrigida na pilha,
    (
        width(Width),
        X >= Width,
        Y1 is Y+1,
        replace_mtx(Numbers, 0, Y1, Poss, NewMtx),
        ReplaceNewLine = table_snapshot(NewMtx, 0, Y1), %inves de data virou um predicado
        !, append_pos([ItemApp, ReplaceNewLine|ListApp], PossibList, Numbers, X, Y, NewListApp)
    );
    % --nao pulou linha, segue normal, inclui a possibilidade na pilha
    (
        X1 is X+1,
        replace_mtx(Numbers, X, Y, Poss, NewMtx2),
        Replace = table_snapshot(NewMtx2, X1, Y),
        !, append_pos([ItemApp,Replace|ListApp], PossibList, Numbers, X, Y, NewListApp)
    ).
    
% --resolve usando backtracking e pilha, vai jogando as novas possibilidades na pilha
solve_puzzle([table_snapshot(Numbers,X,Y)|List], N, Solved) :-
    Item = table_snapshot(Numbers,X,Y),
    puzzle_is_valid(Numbers, 0, 0, IsValid),
    Valid = IsValid,
    find_n_list_to_pos(Numbers, X, Y, 0, [], Result),
    Possibilities = Result,
    (
        length(Possibilities, Length),
        (
            Length == 0, 
            ZeroPossibilities = true 
        ); 
        ZeroPossibilities = false
    ),
    ( 
        % Resolvido!!
        (
            Valid == true,
            !, Solved = Numbers
        );
        % -- corrige x caso esteja fora da dimensao horizontal
        (
            width(Width),
            X >= Width, 
            Y1 is Y+1, 
            N1 is N+1,
            !, solve_puzzle([table_snapshot(Numbers, 0, Y1)|List], N1, Solved)
        );
        % --numero já preenchido pode pular
        (
            get_element_mtx(Numbers, X, Y, El), 
            El \== 0, 
            X1 is X+1,
            N2 is N+1,
            !, solve_puzzle([table_snapshot(Numbers, X1, Y)|List], N2, Solved)
        );
        % --adiciona a pilha novos table snapshots com os possivies numeros
        (
            ZeroPossibilities == false,
            N3 is N+1,
            append_pos([Item|List], Possibilities, Numbers, X, Y, RetAppend),
            !, solve_puzzle(RetAppend, N3, Solved)
        );
        % --não tem mais jogadas pra fazer.. e sabemos que o numero é != de 0, ou seja, falhou... 
        % descartamos o snapshot da pilha
        (
            ZeroPossibilities == true,
            N4 is N+1, 
            !, solve_puzzle(List,N4,Solved)
        )
    ).

% PRINT FUNCTIONS
% --Printa os numeros da linha y
print_line(Numbers,X,Y) :- 
    % --Printa ultimo numero da linha pois nao tem parede
    get_element_mtx(Numbers,X,Y,NumberEl),
    (
        (
            width(Width),
            WidthAux is Width-1,
            X >= WidthAux,
            write(' '), write(NumberEl), write(' ')
        );
        % --Caso haja parede printa | para separar os grupos
        (
            groups(Groups),
            width(Width),
            Width1 is Width-1,
            X1 is X+1,
            get_element_mtx(Groups,X,Y,El),
            get_min(Width1, X1, Min),
            get_element_mtx(Groups,Min,Y,El2),
            El \== El2,
            write(' '), write(NumberEl), write(' |'),
            !, print_line(Numbers, X1, Y)
        );
        (
            X1 is X+1,
            write(' '), write(NumberEl), write('  '),
            !, print_line(Numbers, X1, Y)
        )
    ).
    
% --Printa as paredes de cima, caso o elemento x y-1 seja do mesmo grupo nao printa parede, deixa vazio,
% -- no inicio e final da tabela printa as paredes de borda
print_line_up_wall(X,Y) :- 
    (
        width(Width),
        X >= Width, 
        write('+')
    );
    (
        height(Height),
        groups(Groups),
        Y1 is Y-1,
        get_max(0, Y1, Max),
        (
            Y == 0;
            Y == Height;
            (
                get_element_mtx(Groups, X, Y, El),
                get_element_mtx(Groups, X, Max, El2),
                El \== El2
            )
        ),
        X1 is X+1,
        write('+---'), 
        !,print_line_up_wall(X1,Y)
    );
    (
        write('+   '),
        X1 is X+1,
        !,print_line_up_wall(X1, Y)
    ).

print_table_rec(Numbers, Y) :- 
    (
        height(Height),
        HeightAux is Height-1,
        Y >= HeightAux,
        Y1 is Y+1,
        print_line_up_wall(0,Y),
        nl, write('|'),
        print_line(Numbers, 0, Y),
        write('|'), nl,
        !, print_line_up_wall(0, Y1)
    );
    (
        Y1 is Y+1,
        print_line_up_wall(0,Y),
        nl, write('|'),
        print_line(Numbers, 0, Y),
        write('|'), nl,
        !, print_table_rec(Numbers, Y1)
    ).

print_table(Numbers) :- 
    print_table_rec(Numbers, 0).

kojun_solve :- 
    % serve para ter um parametro mais dinamico de loops para a solução de 1 possib.
    % width(Width),
    % height(Height),
    % WidHeiAux is Width * Height * 10,
    numbers(Numbers),
    write('KOJUN PUZZLE'), nl,
    print_table(Numbers),
    nl, nl, write('SOLVING...'), nl, nl,
    solve_vertical_groups(Numbers, 0, 0, SolvedVertical),
    solve_one_possibilities(SolvedVertical, 0, 0, 10, SolvedOne),
    solve_puzzle([table_snapshot(SolvedOne, 0, 0)], 0, SolvedPuzzle),
    write('Solved'), nl,
    print_table(SolvedPuzzle).
    

main :-
    kojun_solve, 
    halt. 

:- main.