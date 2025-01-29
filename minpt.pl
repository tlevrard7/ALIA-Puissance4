:- module(minpt, [minimax_gpt/7]).
:- use_module(utils, [win/2, moves/2, move/4]).

:- set_prolog_flag(singleton, off).
:- style_check(-singleton).


%.......................................
% sublist
%.......................................
% Vérifie si une liste est une sous-liste d une autre (utilisé pour détecter les groupes de 2 jetons)
sublist(Sub, List) :-
    append(_, Suffix, List),
    append(Sub, _, Suffix).



%.......................................
% utility
%.......................................
% Determines the value of a given board position

utility(B, U) :-
    win(B, 'x'),
    U = 1000, %  MODIFICATION : Augmentation de la valeur pour une victoire immédiate
    !.

utility(B, U) :-
    win(B, 'o'),
    U = -1000, %  MODIFICATION : Priorité plus forte pour bloquer une défaite
    !.

utility(_, U) :-
    U = 0.

%.......................................
% Estimation heuristique améliorée
%.......................................

replace_blank([], [], _).
replace_blank(["."|R], [M|T], M) :- replace_blank(R, T, M).
replace_blank([L|R], [L|T], M) :- L \= ".", replace_blank(R, T, M).

replace_blank_list([], [], _).
replace_blank_list([L1|R], [L2|T], M) :- replace_blank(L1, L2, M), replace_blank_list(R, T, M), !.

possible_combination(B, M, COUNT) :- 
    replace_blank_list(B, L, M), 
    findall(1, win(L, M), W), 
    length(W, COUNT).

% Détection des groupes de 2 jetons alignés
two_in_a_row(B, M, COUNT) :-
    replace_blank_list(B, L, M),
    findall(1, (member(Row, L), sublist([M, M, "."], Row)), W),
    length(W, COUNT).

% Ajout d'une meilleure heuristique
utilityestimate(B, U) :-
    (win(B, 'x') -> U = 1000, !) ;  % Victoire immédiate pour l'IA
    (win(B, 'o') -> U = -1000, !) ; % Victoire immédiate pour l'adversaire
    possible_combination(B, 'x', COUNTAI3),
    possible_combination(B, 'o', COUNTP1_3),
    two_in_a_row(B, 'x', COUNTAI2), % Ajout des groupes de 2 jetons
    two_in_a_row(B, 'o', COUNTP1_2), % Ajout des groupes de 2 jetons pour l'adversaire
    U is (100 * COUNTAI3) - (100 * COUNTP1_3) + (10 * COUNTAI2) - (10 * COUNTP1_2).

% Vérification des coups gagnants immédiats et des blocages critiques
winning_move(B, M, COL) :-
    moves(B, PossibleMoves),
    member(COL, PossibleMoves),
    move(B, COL, M, B2),
    win(B2, M).

blocking_move(B, M, COL) :-
    inverse_mark(M, Opponent),
    moves(B, PossibleMoves),
    member(COL, PossibleMoves),
    move(B, COL, Opponent, B2),
    win(B2, Opponent).

%.......................................
% minimax_gpt avec élagage alpha-bêta
%.......................................

dmax(4).

minimax_gpt(D, B, M, COL, U, ALPHA, BETA) :-
    % For the first move, prefer column 4 if possible
    (D == 0, moves(B, Moves), member(4, Moves) -> COL = 4, U = 0, ! ;
    (winning_move(B, M, COL) -> U = 1000, ! ;  % Coup gagnant direct
    blocking_move(B, M, COL) -> U = -1000, ! ; % Blocage immédiat
    minimax_internal(D, B, M, COL, U, ALPHA, BETA))).

minimax_internal(D, B, _, _, U, _, _) :-
    D2 is D + 1,
    dmax(D2),
    utilityestimate(B, U), !.

minimax_internal(D, B, M, COL, U, ALPHA, BETA) :-
    D2 is D + 1,
    moves(B, L),
    best(D2, B, M, L, COL, U, ALPHA, BETA), !.

minimax_internal(_, B, _, _, U, _, _) :-
    utility(B, U).

%.......................................
% Pruning Alpha-Bêta
%.......................................

alpha_beta_pruning(_, _, M, _, U1, ALPHA, _, _, _) :-
    minimizing(M), U1 =< ALPHA, !.
alpha_beta_pruning(D, B, M, MOVES, U1, ALPHA, BETA, U2, COL2) :-
    NEWBETA is min(BETA, U1),
    best(D, B, M, MOVES, COL2, U2, ALPHA, NEWBETA).

alpha_beta_pruning(_, _, M, _, U1, _, BETA, _, _) :-
    maximizing(M), U1 >= BETA, !.
alpha_beta_pruning(D, B, M, MOVES, U1, ALPHA, BETA, U2, COL2) :-
    NEWALPHA is max(ALPHA, U1),
    best(D, B, M, MOVES, COL2, U2, NEWALPHA, BETA).

%.......................................
% Sélection du meilleur coup
%.......................................

best(D, B, M, [COL1], COL, U, ALPHA, BETA) :-
    move(B, COL1, M, B2),
    inverse_mark(M, M2),
    minimax_gpt(D, B2, M2, _COL, U, ALPHA, BETA),
    COL = COL1, !.

best(D, B, M, [COL1|T], COL, U, ALPHA, BETA) :-
    move(B, COL1, M, B2),
    inverse_mark(M, M2),
    minimax_gpt(D, B2, M2, _COL, U1, ALPHA, BETA),
    alpha_beta_pruning(D, B, M, T, U1, ALPHA, BETA, U2, COL2),
    better(D, M, COL1, U1, COL2, U2, COL, U).

%.......................................
% Comparaison entre deux coups possibles
%.......................................

better(_, _, COL1, U1, COL2, U2, COL, U) :-
    (var(U1) -> U1 = -100000 ; true),  % Initialisation à une valeur très faible
    (var(U2) -> U2 = -100000 ; true),  
    (U1 > U2 -> COL = COL1, U = U1) ;
    (U2 > U1 -> COL = COL2, U = U2) ;
    random_member(COL, [COL1, COL2]), U is U1.