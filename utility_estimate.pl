
next_player(1, 2).      %%% determines the next player after the given player
next_player(2, 1).

inverse_mark('x', 'o'). %%% determines the opposite of the given mark
inverse_mark('o', 'x').

player_mark(1, 'x').    %%% the mark for the given player
player_mark(2, 'o').    

opponent_mark(1, 'o').  %%% shorthand for the inverse mark of the given player
opponent_mark(2, 'x').

blank_mark('.').        %%% the mark used in an empty square

maximizing('x').        %%% the player playing x is always trying to maximize the utility of the board position
minimizing('o').        %%% the player playing o is always trying to minimize the utility of the board position


% Transpose une liste de listes (matrice)
transpose([], []).
transpose([[]|_], []) :- !.
transpose(Matrix, [Row|Rows]) :-
    extract_column(Matrix, Row, RestMatrix),
    transpose(RestMatrix, Rows).

% Extraire la première colonne d une matrice et retourner le reste
extract_column([], [], []).
extract_column([[H|T]|Rest], [H|Column], [T|RestMatrix]) :-
    extract_column(Rest, Column, RestMatrix).

% Lignes du plateau
rows(Board, Rows) :-
    transpose(Board, Columns),        % Transposer pour avoir les colonnes comme lignes
    length(Columns, _),
    findall(Row, (between(1, 6, R), extract_row(Board, R, Row)), Rows).

% Extraire une ligne spécifique (index 1-6)
extract_row(Board, RowIndex, Row) :-
    maplist(nth1(RowIndex), Board, Row).

% Colonnes du plateau (équivalent au plateau lui-même)
columns(Board, Board).

% Diagonales du plateau
diagonals(Board, Diagonals) :-
    findall(Diagonal, diagonal(Board, Diagonal), Diagonals).

% Obtenir une diagonale (haut-gauche à bas-droit ou bas-gauche à haut-droit)
diagonal(Board, Diagonal) :-
    % Haut-gauche à bas-droit
    between(-2, 4, Offset),
    findall(Item, (
        between(1, 6, Row),
        Col is Row + Offset,
        Col >= 1, Col =< 7,
        nth1(Col, Board, Column),
        nth1(Row, Column, Item)
    ), Diagonal),
    length(Diagonal, L),
    L >= 4.

diagonal(Board, Diagonal) :-
    % Bas-gauche à haut-droit
    between(-2, 4, Offset),
    findall(Item, (
        between(1, 6, Row),
        Col is 7 - Row + Offset,
        Col >= 1, Col =< 7,
        nth1(Col, Board, Column),
        nth1(Row, Column, Item)
    ), Diagonal),
    length(Diagonal, L),
    L >= 4.

% Vérifier si un joueur a gagné
win(B, M) :-
    rows(B, R),
    member(Row, R),
    four_in_a_row(Row, M).

win(B, M) :-
    columns(B, Columns),
    member(Column, Columns),
    four_in_a_row(Column, M).

win(B, M) :-
    diagonals(B, Diagonals),
    member(Diagonal, Diagonals),
    four_in_a_row(Diagonal, M).

% Vérifier 4 jetons consécutifs dans une liste
four_in_a_row([M, M, M, M|_], M) :-
    blank_mark(E),
    M \= E.
four_in_a_row([_|Tail], M) :- four_in_a_row(Tail, M).


replace_blank([],[],_).
replace_blank(["."|R],[M|T],M):- replace_blank(R,T,M).
replace_blank([L|R],[L|T],M):- L \= ".", replace_blank(R,T,M).

replace_blank_list([],[],_).
replace_blank_list([L1|R],[L2|T],M):- replace_blank(L1,L2,M), replace_blank_list(R,T,M),!.

possible_combination(B, M, Count):- replace_blank_list(B,L,M), findall(1, win(L,M), W), length(W, Count).


%?- Board = [["r",".",".",".",".","."], ["r",".",".",".",".","."], ["r",".",".",".",".","."], ["r",".",".",".",".","."], [".",".",".",".",".","."], [".",".",".",".",".","."], [".",".",".",".",".","."]], possible_combination(Board, "o", Count).