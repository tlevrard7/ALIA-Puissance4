:- module(ai, [
    random_move/3, minmax_winnings_move/3, minmax_naif_move/3, minmax_strategique_move/3, minimax_gpt_move/3, minmax_noworky_move/3
]).
:- use_module(utils).
:- use_module(reminmax).
:- use_module(minpt).
:- use_module(minmax2).

:- set_prolog_flag(singleton, off).
:- style_check(-singleton).

depth(4).
random_move(B,_, I) :-
    moves(B, C),
    random_member(I, C).

minmax_noworky_move(B, Player, COL):-
    minimax2(0, B, Player,Player, COL, _).

minmax_winnings_move(B, Player, COL):-
    depth(D),
    minmax(B, D, Player, Player, utility_4aligned, -inf, +inf, U, COL).

minmax_naif_move(B, Player, COL):-
    depth(D),
    minmax(B, D, Player, Player, utility_naif, -inf, +inf, _, COL).

minmax_strategique_move(B, Player, COL):-
    depth(D),
    minmax(B, D, Player, Player, utility_strategique, -inf, +inf, _, COL).

minimax_gpt_move(B, Player, COL):-
    minimax_gpt(0, B, Player, COL, _, -inf, +inf).
