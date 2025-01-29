:- module(utils, [
    empty_board/1, next_player/2, player_mark/2, inverse_mark/2, blank_mark/1, maximizing/1, minimizing/1,
    transpose/2, rows/2, columns/2, diagonals/2, extract_row/3,
    moves/2, win/2, move/4
]).


empty_board([
    [E, E, E, E, E, E],
    [E, E, E, E, E, E],
    [E, E, E, E, E, E],
    [E, E, E, E, E, E],
    [E, E, E, E, E, E],
    [E, E, E, E, E, E],
    [E, E, E, E, E, E]
]) :- blank_mark(E).


% Définit le joueur suivant pour un joueur donné
next_player(1, 2).     
next_player(2, 1).

% Définit l'opposé d'un symbole donné
inverse_mark('x', 'o').
inverse_mark('o', 'x').

% On associe à chaque numéro de joueur un symbole
player_mark(1, 'x').    
player_mark(2, 'o').    

% Définit le symbole associé à une case vide
blank_mark('.').     

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

% rows(+B,-R)
% Obtient les lignes (stockées dans R) du plateau B
rows(B, Rows) :-
    transpose(B, Columns),        % Transposer pour avoir les colonnes comme lignes
    length(Columns, _),
    findall(Row, (between(1, 6, R), extract_row(B, R, Row)), Rows).

% Extraire une ligne spécifique (index 1-6)
extract_row(B, RowIndex, Row) :-
    maplist(nth1(RowIndex), B, Row).

% Colonnes du plateau (équivalent au plateau lui-même)
columns(B, B).

% Diagonales du plateau
diagonals(B, Diagonals) :-
    findall(Diagonal, diagonal(B, Diagonal), Diagonals).

% Obtenir une diagonale (haut-gauche à bas-droit ou bas-gauche à haut-droit)
diagonal(B, Diagonal) :-
    % Haut-gauche à bas-droit
    between(-2, 4, Offset),
    findall(Item, (
        between(1, 6, Row),
        Col is Row + Offset,
        Col >= 1, Col =< 7,
        nth1(Col, B, Column),
        nth1(Row, Column, Item)
    ), Diagonal),
    length(Diagonal, L),
    L >= 4.

diagonal(B, Diagonal) :-
    % Bas-gauche à haut-droit
    between(-2, 4, Offset),
    findall(Item, (
        between(1, 6, Row),
        Col is 7 - Row + Offset,
        Col >= 1, Col =< 7,
        nth1(Col, B, Column),
        nth1(Row, Column, Item)
    ), Diagonal),
    length(Diagonal, L),
    L >= 4.

% moves(+B,-ValidMoves)
% Obtient la liste des coups valides 'ValidMoves' (indices des colonnes où on peut jouer) pour le plateau 'B'
moves(B, ValidMoves) :-
    blank_mark(E),
    % On calcule les coups valides en regardant les colonnes qui ont au moins une case vide 
    findall(Col, (
        nth1(Col, B, Column), % Obtient chaque colonne
        column_has_blank(Column, E) % Vérifie si la colonne a au moins une case vide
    ), ValidMoves).

% Vérifie si une colonne a au moins une case vide
column_has_blank([E|_], E) :- !. % Si la première case est vide, on s'arrête
column_has_blank([_|Rest], E) :- column_has_blank(Rest, E). % Sinon, on continue


% move(+B,+ColumnIndex,+PlayerMark,-NewBoard)    
% Ajoute un jeton de symbole 'PlayerMark' dans la colonne 'ColumnIndex' du plateau 'B' et calcule le nouveau plateau 'NewBoard'
move(B, ColumnIndex, PlayerMark, NewBoard) :-
    nth1(ColumnIndex, B, Column),                        % Extraie la colonne spécifiée de B
    add_token(Column, PlayerMark, NewColumn),            % Ajoute le jeton à cette colonne
    replace_column(B, ColumnIndex, NewColumn, NewBoard). % Calcule le nouveau plateau 'NewBoard'

% add_token(+Column, +PlayerMark, -NewColumn)
% Ajoute un jeton de symbole 'PlayerMark' dans la colonne 'Column' du plateau 'B' et calcule la nouvelle colonne 'NewColumn'
add_token(Column, PlayerMark, NewColumn) :-
    replace_first_empty(Column, PlayerMark, NewColumn).

% Remplace la première case vide de la colonne par le jeton du joueur
replace_first_empty([H|Tail], PlayerMark, [PlayerMark|Tail]) :-
    blank_mark(E),
    H == E, !
    .
replace_first_empty([Head|Tail], PlayerMark, [Head|NewTail]) :-
    replace_first_empty(Tail, PlayerMark, NewTail).

% Remplace une colonne spécifique (numéro 'Index') dans un plateau et calcule le nouveau plateau
replace_column([_|Tail], 1, NewColumn, [NewColumn|Tail]) :- !. % Remplace la première colonne
replace_column([Head|Tail], Index, NewColumn, [Head|NewTail]) :-
    Index > 1,
    NewIndex is Index - 1,
    replace_column(Tail, NewIndex, NewColumn, NewTail).

% win(+B,+M)
% Vérifie si le joueur associé au symbole 'M' a gagné pour la grille 'B' donnée 
% càd s'il a 4 jetons consécutifs dans une des lignes, une des colonnes ou une des diagonales
win(B, M) :-
    blank_mark(E), M \= E,
    rows(B, R), member(Row, R), four_in_a_row(Row, M);  % Vérifie si le joueur a 4 jetons consécutifs dans une des lignes
    columns(B, Columns), member(Column, Columns), four_in_a_row(Column, M);  % Idem mais dans une des colonnes
    diagonals(B, Diagonals), member(Diagonal, Diagonals), four_in_a_row(Diagonal, M). % Idem mais dans une des diagonales

% four_in_a_row(+L,+M)
% Vérifie s'il y a 4 jetons 'M' consécutifs dans la liste 'L'
four_in_a_row([M, M, M, M|_], M).
four_in_a_row([_|Tail], M) :- four_in_a_row(Tail, M).
