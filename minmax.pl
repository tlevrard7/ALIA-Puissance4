:- module(minmax, [minimax/7]).
:- use_module(utils,[win/2, moves/2, move/4]).


%.......................................
% utility
%.......................................
% determines the value of a given board position
%

utility(B, U, Player) :-
    win(B, Player),    % Si le joueur gagne
    U = 70,
    !
    .

utility(B, U, Player) :-
    inverse_mark(Player, Opponent),  % Si l adversaire gagne
    win(B, Opponent),
    U = -70,
    !
    .

utility(B,U) :-
    U = 0
    . % Sinon, égalité ou pas de victoire pour le moment

%.......................................
% Estimation
%.......................................
% In case we arrive at max depth.
% We calculate the difference of possible winning moves for each player.

replace_blank([],[],_).
replace_blank(["."|R],[M|T],M):- replace_blank(R,T,M).
replace_blank([L|R],[L|T],M):- L \= ".", replace_blank(R,T,M).

replace_blank_list([],[],_).
replace_blank_list([L1|R],[L2|T],M):- replace_blank(L1,L2,M), replace_blank_list(R,T,M),!.

possible_combination(B, M, COUNT) :-
    replace_blank_list(B, L, M),      % Remplace les cases vides par le jeton du joueur
    findall(1, win(L, M), W),         % Trouve toutes les combinaisons gagnantes pour le joueur
    length(W, COUNT)                  % Compte ces combinaisons
    .

utilityestimate(B, U, Player) :-
    inverse_mark(Player, Opponent),
    possible_combination(B, Player, COUNTAI),    % Combinaisons gagnantes pour le joueur
    possible_combination(B, Opponent, COUNTP1), % Combinaisons gagnantes pour l adversaire
    U is COUNTAI - COUNTP1
    .

%.......................................
% minimax
%.......................................
% The minimax algorithm always assumes an optimal opponent.
% For tic-tac-toe, optimal play will always result in a tie, so the algorithm is effectively playing not-to-lose.

% If max depth attained then return estimate.
dmax(3).

minimax(D,B,M,COL,U, ALPHA, BETA) :-
    D2 is D + 1,
    dmax(D2),
    utilityestimate(B, U, Player),
    !.

% For the opening move we choose the know best starting move column 4.
% Saves the user the trouble of waiting  for the computer to search the entire minimax tree.
minimax(_,[[E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E]],M,COL,U, _, _) :-   
    blank_mark(E),
    COL = 4,
    !.

minimax(D,B,M,COL,U, ALPHA, BETA) :-
    D2 is D + 1,
    moves(B,L),          %%% get the list of available moves
    !,
    best(D2,B,M,L,COL,U, ALPHA, BETA),  %%% recursively determine the best available move
    !.

% if there are no more available moves, 
% then the minimax value is the utility of the given board position

minimax(_, B, Player, _, U, _, _) :-
    utility(B, U, Player)
    .


%.......................................
% best
%.......................................
% determines the best move in a given list of moves by recursively calling minimax
%

% Pruning
alpha_beta_pruning(D,B,M,MOVES,U1, ALPHA, BETA, U2, COL2):-
    minimizing(M),    %%% If i'm minimizing, then previous player was maximizing with option ALPHA .
    U1 =< ALPHA,      %%% Thus if  (any other after =< COL =< ALPHA) then we know fully that this branch isn't getting picked
	  !.
alpha_beta_pruning(D,B,M,MOVES,U1, ALPHA, BETA, U2, COL2):-
	  NEWBETA is min(BETA,U1),
    best(D,B,M,MOVES,COL2,U2, ALPHA, NEWBETA)
    .

alpha_beta_pruning(D,B,M,MOVES,U1, ALPHA, BETA, U2, COL2):-
    maximizing(M),    %%% If i'm maximizing, then previous player was minimizing with option BETA.
    U1 >= BETA,       %%% Thus if (any other after >= COL >= BETA) then we know fully that this branch isn't getting picked
	  !.
alpha_beta_pruning(D,B,M,MOVES,U1, ALPHA, BETA, U2, COL2):-
	  NEWALPHA is max(ALPHA,U1),
    best(D,B,M,MOVES,COL2,U2, NEWALPHA, BETA)
    .


% if there is no move left
best(D,B,M,[],COL,U, ALPHA, BETA).

% if there is only one move left in the list...
best(D,B,M,[COL1],COL,U, ALPHA, BETA) :-
    move(B,COL1,M,B2),        %%% apply that move to the board, !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    inverse_mark(M,M2), 
    !,  
    minimax(D,B2,M2,_COL,U, ALPHA, BETA),  %%% then recursively search for the utility value of that move.
    COL = COL1, !
    .

% if there is more than one move in the list...

best(D,B,M,[COL1|T],COL,U, ALPHA, BETA) :-
    move(B,COL1,M,B2),             %%% apply the first move (in the list) to the board, !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    inverse_mark(M,M2), 
    !,
    minimax(D,B2,M2,_COL,U1, ALPHA, BETA),                         %%% recursively search for the utility value of that move,
    alpha_beta_pruning(D,B,M,T,U1, ALPHA, BETA, U2, COL2),         %%% stop searching if we already know it s not getting picked, else continue
    better(D,M,COL1,U1,COL2,U2,COL,U)                              %%% and choose the better of the two moves (based on their respective utility values).  
	  .

%.......................................
% better
%.......................................
% returns the better of two moves based on their respective utility values.
%
% if both moves have the same utility value, then one is chosen at random.

better(_, Player, COL1, U1, COL2, U2, COL, U) :-
    maximizing(Player),    % Si le joueur maximise
    U1 > U2,
    COL = COL1,
    U = U1,
    !.

better(_, Player, COL1, U1, COL2, U2, COL, U) :-
    minimizing(Player),    % Si le joueur minimise
    U1 < U2,
    COL = COL1,
    U = U1,
    !.

better(_, _, COL1, U1, COL2, U2, COL, U) :-
    U1 == U2,              % Si les utilités sont égales
    COL = COL1,
    U = U1,
    !.

better(_, _, _, _, COL2, U2, COL, U) :- % Par défaut, on choisit le deuxième coup
    COL = COL2,
    U = U2,
    !.
