%.......................................
% square !!!!!!!!!!!!!!!!!!
%.......................................
% The mark in a square(N) corresponds to an item in a list, as follows:

square([M,_,_,_,_,_,_,_,_],1,M).
square([_,M,_,_,_,_,_,_,_],2,M).
square([_,_,M,_,_,_,_,_,_],3,M).
square([_,_,_,M,_,_,_,_,_],4,M).
square([_,_,_,_,M,_,_,_,_],5,M).
square([_,_,_,_,_,M,_,_,_],6,M).
square([_,_,_,_,_,_,M,_,_],7,M).
square([_,_,_,_,_,_,_,M,_],8,M).
square([_,_,_,_,_,_,_,_,M],9,M).

%.......................................
% moves !!!!!!!!!!!!!!!!!!!!!!!
%.......................................
% retrieves a list of available moves (empty squares) on a board.
%

moves(B,L) :-
    not(win(B,x)),                %%% if either player already won, then there are no available moves
    not(win(B,o)),
    blank_mark(E),
    findall(N, square(B,N,E), L), 
    L \= []
    .

%.......................................
% utility
%.......................................
% determines the value of a given board position
%

utility(B,U) :-
    win(B,'x'),
    U = 70, % there is 69 winning combination in power 4
    !
    .

utility(B,U) :-
    win(B,'o'),
    U = (-70), % there is 69 winning combination in power 4
    !
    .

utility(B,U) :-
    U = 0
    .
% Estimation
replace_blank([],[],_).
replace_blank(["."|R],[M|T],M):- replace_blank(R,T,M).
replace_blank([L|R],[L|T],M):- L \= ".", replace_blank(R,T,M).

replace_blank_list([],[],_).
replace_blank_list([L1|R],[L2|T],M):- replace_blank(L1,L2,M), replace_blank_list(R,T,M),!.

possible_combination(B, M, COUNT):- replace_blank_list(B,L,M), findall(1, win(L,M), W), length(W, COUNT).

% toujours x - o
% utilityestimate(B,U,M):- inverse_mark(M, M2), possible_combination(B, M, COUNT1), possible_combination(B, M2, COUNT2), U is COUNT1 - COUNT2.
utilityestimate(B,U):- possible_combination(B, 'x', COUNTAI), possible_combination(B, 'o', COUNTP1), U is COUNTAI - COUNTP1.

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
    utilityestimate(B,U),      
    !.

% For the opening move we choose the know best starting move column 4.
% Saves the user the trouble of waiting  for the computer to search the entire minimax tree.
minimax(_,[E,E,E, E,E,E, E,E,E],M,COL,U, _, _) :-   
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

minimax(_,B,_,_,U, _, _) :-
    utility(B,U)      
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
    alpha_beta_pruning(D,B,M,T,U1, ALPHA, BETA, U2, COL2),         %%% stop searching if we already know it's not getting picked, else continue
    better(D,M,COL1,U1,COL2,U2,COL,U)                              %%% and choose the better of the two moves (based on their respective utility values).  
	  .

%.......................................
% better
%.......................................
% returns the better of two moves based on their respective utility values.
%
% if both moves have the same utility value, then one is chosen at random.

better(D,M,COL1,U1,COL2,U2,     COL,U) :-
    maximizing(M),                     %%% if the player is maximizing
    U1 > U2,                           %%% then greater is better.
    COL = COL1,
    U = U1,
    !
    .

better(D,M,COL1,U1,COL2,U2,     COL,U) :-
    minimizing(M),                     %%% if the player is minimizing,
    U1 < U2,                           %%% then lesser is better.
    COL = COL1,
    U = U1, 
    !
    .

better(D,M,COL1,U1,COL2,U2,     COL,U) :-
    U1 == U2,                          %%% if moves have equal utility,
    COL = COL1,                        %%% Pick first for now
    U = U1,
    !
    .

better(D,M,COL1,U1,COL2,U2,     COL,U) :-        %%% otherwise, second move is better
    COL = COL2,
    U = U2,
    !
    .
