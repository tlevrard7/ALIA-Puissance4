:- module(ouput, [output_players/0, output_winner/1, output_board/1]).
:- use_module(utils).

% Affiche les types des joueurs 1 et 2.
output_players :- 
    nl,
    player(1, V1),
    write('Player 1 is '),   
    write(V1),

    nl,
    player(2, V2),
    write('Player 2 is '),   
    write(V2), 
    nl,
    !
    .

output_winner(P) :-
    player_mark(P,M),
    write(M),
    write(' wins.'),
    nl,
    !
    .

output_winner(_) :-
    write('No winner.'),
    nl
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