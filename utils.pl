:- module(utils, [
    next_player/2, player_mark/2, blank_mark/1, maximizing/1,
    transpose/2, rows/2, columns/2, diagonals/2, extract_row/3,
    moves/2, win/2, move/4
]).


next_player(1, 2).      %%% determines the next player after the given player
next_player(2, 1).


player_mark(1, 'x').    %%% the mark for the given player
player_mark(2, 'o').    

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

moves(B,L) :-
    blank_mark(E),
    extract_row(B, 6, R),
    findall(N, nth1(N,R,E), L).
    
% Appliquer un mouvement sur le plateau
move(Board, ColumnIndex, PlayerMark, NewBoard) :-
    nth1(ColumnIndex, Board, Column),                     % Obtenir la colonne spécifiée
    add_token(Column, PlayerMark, NewColumn),             % Ajouter le jeton dans cette colonne
    replace_column(Board, ColumnIndex, NewColumn, NewBoard). % Mettre à jour le plateau

% Ajouter un jeton dans une colonne spécifique
add_token(Column, PlayerMark, NewColumn) :-
    replace_first_empty(Column, PlayerMark, NewColumn).

% Remplacer la première case vide par le jeton du joueur
replace_first_empty([H|Tail], PlayerMark, [PlayerMark|Tail]) :-
    blank_mark(E),
    H == E, !
    .
replace_first_empty([Head|Tail], PlayerMark, [Head|NewTail]) :-
    replace_first_empty(Tail, PlayerMark, NewTail).


% Remplacer une colonne spécifique dans le plateau
replace_column([_|Tail], 1, NewColumn, [NewColumn|Tail]) :- !. % Remplace la première colonne
replace_column([Head|Tail], Index, NewColumn, [Head|NewTail]) :-
    Index > 1,
    NewIndex is Index - 1,
    replace_column(Tail, NewIndex, NewColumn, NewTail).

% Vérifier si un joueur a gagné
win(B, M) :-
    rows(B, R), member(Row, R), four_in_a_row(Row, M);
    columns(B, Columns), member(Column, Columns), four_in_a_row(Column, M);
    diagonals(B, Diagonals), member(Diagonal, Diagonals), four_in_a_row(Diagonal, M).

% Vérifier 4 jetons consécutifs dans une liste
four_in_a_row([M, M, M, M|_], M).
four_in_a_row([_|Tail], M) :- four_in_a_row(Tail, M).
