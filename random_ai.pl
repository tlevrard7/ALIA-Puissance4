:- module(random_ai, [random_ai_move/3]).
:- use_module(utils).

random_ai_move(B, _, I) :-
    moves(B, C),
    random_member(I, C),
    write(C), nl.