:- module(ai, [
    random_move/3, minmax_winnings_move/3, minmax_naif_move/3, minmax_strategique_move/3
]).
:- use_module(utils).
:- use_module(reminmax).

:- set_prolog_flag(singleton, off).
:- style_check(-singleton).

% Calcule des coups pour les IA selon le board actuel et un depth défini

random_move(B,_, I) :-
    moves(B, C),
    random_member(I, C).

depth(4). % Profondeur maximale de calcul

% Afin de déterminer les bornes de l algorithme minmax, 
% On utilisera 100000 pour +infini et -100000 pour -infini


minmax_winnings_move(B, Player, COL):-
    depth(D),
    minmax(B, D, Player, Player, utility_4aligned, (-100000), 1000000, U, COL).

minmax_naif_move(B, Player, COL):-
    depth(D),
    minmax(B, D, Player, Player, utility_naif, (-100000), 1000000, _, COL).

minmax_strategique_move(B, Player, COL):-
    depth(D),
    minmax(B, D, Player, Player, utility_strategique, (-100000), 1000000, _, COL).

