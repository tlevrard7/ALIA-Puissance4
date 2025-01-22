:- module(ai, [
    random_ai_move/3, minmax_AI_move/3
]).
:- use_module(utils).
:- use_module(minmax).

random_ai_move(B, _, I) :-
    moves(B, C),
    random_member(I, C),
    write(C), nl.

minmax_AI_move(B, _, COL) :-
    minimax(0,B,'x',COL,U, -inf, +inf),
    COL is COLopti.