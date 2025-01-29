:- module(ai, [
    random_ai_move/3, minmax_winnings_move/3, minmax_naif_move/3, minmax_strategique_move/3, minimax_gpt_move/3
]).
:- use_module(utils).
:- use_module(minmax2).
:- use_module(minpt).

random_ai_move(B,_, I) :-
    moves(B, C),
    random_member(I, C).

minmax_winnings_move(B, Player, COL):-
    minimax(0, B, Player,Player, COL, U).
    % minimax(0, B, Player, COL, U, -inf, +inf, Player, utilityestimate_4aligned).

minmax_naif_move(B, Player, COL):-
    minimax(0, B, Player,Player, COL, U).
    % minimax(0, B, Player, COL, U, -inf, +inf, Player, utilityestimate_naif).

minmax_strategique_move(B, Player, COL):-
    minimax(0, B, Player,Player, COL, U).
    % minimax(0, B, Player, COL, U, -inf, +inf, Player, utilityestimate_strategique).

minimax_gpt_move(B, Player, COL):-
    minimax(0, B, Player,Player, COL, U).
    % minimax_gpt(0, B, Player, COL, U, -inf, +inf).
