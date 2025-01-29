:- module(ouput, [output_players/4, output_winner/1, output_board/1]).
:- use_module(utils).

:- set_prolog_flag(singleton, off).
:- style_check(-singleton).


% Affiche les types des joueurs 1 et 2.
output_players(M1, C1, M2, C2) :- 
    write('Player '), write(M1), write(' is '), write(C1), nl,
    write('Player '), write(M2), write(' is '), write(C2), nl,
    !.

output_winner(E) :-
    blank_mark(E),
    write('No winner.'), nl
    .

output_winner(P) :-
    write(P),
    write(' wins.'),
    nl,
    !
    .

% output_board(+B)
% Affiche la grille de jeu actuelle : chaque ligne du bas vers le haut
output_board(B) :-
    reverse_rows(B, R),
    maplist(output_row, R).

% Transforme une colonne (verticale) en ligne pour affichage
reverse_rows(B, R) :-
    transpose(B, C),  % Transpose pour obtenir les lignes
    reverse(C, R).

% Affiche une ligne
output_row(R) :-
    maplist(output_square, R),
    nl.

% Affiche une case
output_square(M) :- write(M), write(' ').