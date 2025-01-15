%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Représentation du plateau
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Plateau vide : une liste de 7 colonnes, chaque colonne contenant 6 cases vides.
empty_board([
    [e, e, e, e, e, e],
    [e, e, e, e, e, e],
    [e, e, e, e, e, e],
    [e, e, e, e, e, e],
    [e, e, e, e, e, e],
    [e, e, e, e, e, e],
    [e, e, e, e, e, e]
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ajout d un jeton dans une colonne
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ajouter un jeton dans une colonne spécifique
add_token(Column, PlayerMark, NewColumn) :-
    replace_first_empty(Column, PlayerMark, NewColumn).

% Remplacer la première case vide par le jeton du joueur
replace_first_empty([e|Tail], PlayerMark, [PlayerMark|Tail]) :- !. % Remplace le premier 'e' par PlayerMark
replace_first_empty([Head|Tail], PlayerMark, [Head|NewTail]) :-
    replace_first_empty(Tail, PlayerMark, NewTail).

% Appliquer un mouvement sur le plateau
make_move(Board, ColumnIndex, PlayerMark, NewBoard) :-
    nth1(ColumnIndex, Board, Column),                     % Obtenir la colonne spécifiée
    add_token(Column, PlayerMark, NewColumn),             % Ajouter le jeton dans cette colonne
    replace_column(Board, ColumnIndex, NewColumn, NewBoard). % Mettre à jour le plateau

% Remplacer une colonne spécifique dans le plateau
replace_column([_|Tail], 1, NewColumn, [NewColumn|Tail]) :- !. % Remplace la première colonne
replace_column([Head|Tail], Index, NewColumn, [Head|NewTail]) :-
    Index > 1,
    NewIndex is Index - 1,
    replace_column(Tail, NewIndex, NewColumn, NewTail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Vérification de victoire
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Vérifier si un joueur a gagné
has_won(Board, PlayerMark) :-
    rows(Board, Rows),
    member(Row, Rows),
    four_in_a_row(Row, PlayerMark).

has_won(Board, PlayerMark) :-
    columns(Board, Columns),
    member(Column, Columns),
    four_in_a_row(Column, PlayerMark).

has_won(Board, PlayerMark) :-
    diagonals(Board, Diagonals),
    member(Diagonal, Diagonals),
    four_in_a_row(Diagonal, PlayerMark).

% Vérifier 4 jetons consécutifs dans une liste
four_in_a_row([M, M, M, M|_], M) :- M \= e.
four_in_a_row([_|Tail], M) :-
    four_in_a_row(Tail, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Transposer une matrice
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obtenir lignes, colonnes et diagonales
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Lignes du plateau
rows(Board, Rows) :-
    transpose(Board, Columns),        % Transposer pour avoir les colonnes comme lignes
    length(Columns, N),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Affichage du plateau
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Affiche chaque ligne du bas vers le haut
print_board(Board) :-
    reverse_rows(Board, Reversed),
    maplist(print_row, Reversed).

% Transforme une colonne (verticale) en ligne pour affichage
reverse_rows(Board, Rows) :-
    transpose(Board, Columns),  % Transpose pour obtenir les lignes
    reverse(Columns, Rows).

% Affiche une ligne
print_row(Row) :-
    maplist(print_square, Row),
    nl.

% Affiche une case
print_square(e) :- write('. ').
print_square(Mark) :- write(Mark), write(' ').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Jeu interactif
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Lancer le jeu interactif
start_game :-
    empty_board(Board),
    play_game(Board, 1). % Commence avec le joueur 1

% Boucle principale du jeu
play_game(Board, Player) :-
    print_board(Board), % Affiche le plateau actuel
    (game_over(Board, Player) -> !; % Vérifie si la partie est terminée
     prompt_move(Player, Column), % Demande le coup au joueur
     player_mark(Player, Mark), % Récupère le jeton correspondant au joueur
     (make_move(Board, Column, Mark, NewBoard) -> % Applique le coup si valide
         next_player(Player, NextPlayer), % Passe au joueur suivant
         play_game(NewBoard, NextPlayer) % Continue le jeu
     ; % Sinon, redemande le coup
         write('Invalid move. Try again.'), nl,
         play_game(Board, Player)
     )).

% Vérification de la fin du jeu
game_over(Board, Player) :-
    (has_won(Board, x) -> write('Player X wins!'), nl, true;
     has_won(Board, o) -> write('Player O wins!'), nl, true;
     is_draw(Board) -> write('It\'s a draw!'), nl, true).

% Vérification de l égalité (plateau plein)
is_draw(Board) :-
    \+ (member(Column, Board), member(e, Column)).

% Demander un coup au joueur
prompt_move(Player, Column) :-
    write('Player '), write(Player), write(', enter column (1-7): '),
    read(Column),
    valid_column(Column).

% Vérifie que la colonne est valide (entre 1 et 7)
valid_column(Column) :-
    between(1, 7, Column), !.
valid_column(_) :-
    write('Invalid column. Must be between 1 and 7.'), nl, fail.

% Détermine le jeton du joueur (X ou O)
player_mark(1, x).
player_mark(2, o).

% Passe au joueur suivant
next_player(1, 2).
next_player(2, 1).