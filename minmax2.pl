:- module(minmax2, [minimax2/6]).
:- use_module(utils,[win/2, moves/2, move/4]).

:- set_prolog_flag(singleton, off).
:- style_check(-singleton).


%.......................................
% utility
%.......................................
% determines the value of a given board position
%

utility(B,U, _, _) :-
    win(B,o),
    U = 70, 
    !.

utility(B,U, _, _) :-
    win(B,x),
    U = (-70), 
    !.


utility(_,U,_, _) :-
    U = 0.

%.......................................
% minimax
%.......................................
% The minimax algorithm always assumes an optimal opponent.
% For tic-tac-toe, optimal play will always result in a tie, so the algorithm is effectively playing not-to-lose.

% For the opening move against an optimal player, the best minimax can ever hope for is a tie.
% So, technically speaking, any opening move is acceptable.
% Save the user the trouble of waiting  for the computer to search the entire minimax tree 
% by simply selecting a random square.

dmax(4).
minimax2(_,[E,E,E, E,E,E, E,E,E],_, _,   COL, _) :-   
    blank_mark(E),
    COL=4, !.
minimax2(D,B,M,PLAYER,   COL,U) :-
    D2 is D + 1,
    not(dmax(D2)),
    not(win(B, x)),  %%% Auccun gagnant sinon utility
    not(win(B, o)),
    moves(B,L),  %%% obtient la liste de tous les coups possible
    L\=[],       %%% il y a des coups possibles
    !,
    best(D2,B,M,PLAYER,L,   COL,U),  %%% recursively determine the best available move
    !.

% if there are no more available moves, 
% then the minimax value is the utility of the given board position

minimax2(_,B,M,PLAYER,   COL,U) :-
    utility(B,U, M, PLAYER),
    % write(M),write(PLAYER),write(B), writeln(U),
    COL=7
    .


%.......................................
% best
%.......................................
% determines the best move in a given list of moves by recursively calling minimax
%

% if there is only one move left in the list...

best(D,B,M,PLAYER,[COL1],   COL,U) :-
    move(B,COL1,M,B2),        %%% apply that move to the board,
    inverse_mark(M,M2), 
    !,  
    minimax2(D,B2,M2,PLAYER,_COL,U),  %%% then recursively search for the utility value of that move.
    COL = COL1, 
    !.

% if there is more than one move in the list...

best(D,B,M,PLAYER,[COL1|T], COL,U) :-
    move(B,COL1,M,B2),             %%% apply the first move (in the list) to the board,
    inverse_mark(M,M2), 
    !,
    minimax2(D,B2,M2,PLAYER,_COL,U1),      %%% recursively search for the utility value of that move,
    best(D,B,M,PLAYER,T,COL2,U2),         %%% determine the best move of the remaining moves,     
    better(D,M,PLAYER,COL1,U1,COL2,U2,  COL,U)  %%% and choose the better of the two moves (based on their respective utility values)
    .


%.......................................
% better
%.......................................
% returns the better of two moves based on their respective utility values.
%
% if both moves have the same utility value, then one is chosen at random.

better(_,M,PLAYER,COL1,U1,_,U2,     COL,U) :-
    M == PLAYER,                     %%% if the player is maximizing
    U1 > U2,                           %%% then greater is better.
    COL = COL1,
    U = U1,
    !
    .

better(_,M,PLAYER,COL1,U1,_,U2,     COL,U) :-
    M \= PLAYER,                     %%% if the player is minimizing,
    U1 < U2,                           %%% then lesser is better.
    COL = COL1,
    U = U1, 
    !
    .

better(_, _,_,_,U1,_,U2,     _,U) :-
    U1 == U2,                          %%% if moves have equal utility,
    U = U1,
    !
    .

better(_,_,_,_,_,COL2,U2,     COL,U) :-        %%% otherwise, second move is better
    COL = COL2,
    U = U2,
    !
    .