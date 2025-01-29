:- module(ai, [
    random_ai_move/3, minmax_AI_move/3
]).
:- use_module(utils).
:- use_module(minmax).

random_ai_move(B,_, I) :-
    moves(B, C),
    random_member(I, C),
    write(C), nl.

<<<<<<< HEAD
minmax_AI_move(B,M, COL) :-
    minimax(0,B,M,COL,U, -inf, +inf,M).
=======
minmax_AI_move(B, Player, COL) :-
    minimax(0, B, Player, COL, U, -inf, +inf).
>>>>>>> 61cf1b64bb183720be2f54c735ab3bfd4b724d46
