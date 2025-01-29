:- module(ai, [
    random_ai_move/3, minmax_AI_move/4
]).
:- use_module(utils).
:- use_module(minmax).

random_ai_move(B,_, I) :-
    moves(B, C),
    random_member(I, C).

minmax_AI_move(B, Player, COL, Utility_func) :-
    minimax(0, B, Player, COL, U, -inf, +inf, Player, Utility_func).
