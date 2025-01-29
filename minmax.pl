:- module(minmax, [minimax/8]).
:- use_module(utils,[win/2, moves/2, move/4]).


%.......................................
% utility
%.......................................
% détermine la valuer de la position donné du plateau
%

utility(B,U,PLAYER) :-
    win(B,M),
    U = 70, % there is 69 winning combination in power 4
    !
    .

utility(B,U,PLAYER) :-
    inverse_mark(PLAYER,ADV),
    win(B,ADV),
    U = (-70), % there is 69 winning combination in power 4
    !
    .

utility(B,U,_) :-
    U = 0
    . % Sinon, égalité ou pas de victoire pour le moment

%.......................................
% Estimation
%.......................................
% Dans le cas où on arrive à la profondeur max.
% On calcule la différence de combinaison de gagné de chaque joueur

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
% L'algorithme minimax considère toujours que l'adversaire fera le meilleur choix.

% Si la profondeur maximal est atteint, alors retourne une estimation
dmax(3).

minimax(D,B,M,COL,U, ALPHA, BETA,PLAYER) :-
    D2 is D + 1,
    dmax(D2),
    utilityestimate(B,U,PLAYER),      
    !.

% For the opening move we choose the know best starting move column 4.
% Saves the user the trouble of waiting  for the computer to search the entire minimax tree.
minimax(_,[[E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E]],M,COL,U, _, _, _) :-   
    blank_mark(E),
    COL = 4,
    !.

minimax(D,B,M,COL,U, ALPHA, BETA,PLAYER) :-
    D2 is D + 1,
    moves(B,L),          %%% obtient la liste de tous les coups possible
    !,
    best(D2,B,M,L,COL,U, ALPHA, BETA,PLAYER),  %%% recursively determine the best available move
    !.

% si il y a pas de coup possible,
% alors la valeur minimax est l'utilité de la position du plateau donnée

minimax(_,B,_,_,U, _, _,PLAYER) :-
    utility(B,U,PLAYER)      
    .


%.......................................
% best
%.......................................
% détermine le meilleur coup possible d'une liste de coup donnée produit récursivement par minimax
%

% Pruning
alpha_beta_pruning(D,B,M,MOVES,U1, ALPHA, BETA, U2, COL2,PLAYER):-
    minimizing(M),    %%% If i'm minimizing, then previous player was maximizing with option ALPHA .
    U1 =< ALPHA,      %%% Thus if  (any other after =< COL =< ALPHA) then we know fully that this branch isn't getting picked
	!.
alpha_beta_pruning(D,B,M,MOVES,U1, ALPHA, BETA, U2, COL2,PLAYER):-
	NEWBETA is min(BETA,U1),
    best(D,B,M,MOVES,COL2,U2, ALPHA, NEWBETA,PLAYER)
    .
alpha_beta_pruning(D,B,M,MOVES,U1, ALPHA, BETA, U2, COL2,PLAYER):-
    maximizing(M),    %%% If i'm maximizing, then previous player was minimizing with option BETA.
    U1 >= BETA,       %%% Thus if (any other after >= COL >= BETA) then we know fully that this branch isn't getting picked
	!.
alpha_beta_pruning(D,B,M,MOVES,U1, ALPHA, BETA, U2, COL2,PLAYER):-
	NEWALPHA is max(ALPHA,U1),
    best(D,B,M,MOVES,COL2,U2, NEWALPHA, BETA,PLAYER)
    .


% if there is no move left
best(_,_,_,[],_,_, _, _,_).

% si il y a seulement un seul coup restant dans la liste...


best(D,B,M,[COL1],COL,U, ALPHA, BETA,PLAYER) :-
    move(B,COL1,M,B2),        %%% applique ce coup au plateau, !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    inverse_mark(M,M2), 
    !,  
    minimax(D,B2,M2,_COL,U, ALPHA, BETA,PLAYER),  %%% et cherche récursivement la valeur d'utilité de ce coup.
    COL = COL1, !
    .

% si il y a plus d'un coup dans la liste...

best(D,B,M,[COL1|T],COL,U, ALPHA, BETA,PLAYER) :-
    move(B,COL1,M,B2),             %%% apply the first move (in the list) to the board, !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    inverse_mark(M,M2), 
    !,
    minimax(D,B2,M2,_COL,U1, ALPHA, BETA,PLAYER),                         %%% recursively search for the utility value of that move,
    alpha_beta_pruning(D,B,M,T,U1, ALPHA, BETA, U2, COL2,PLAYER),         %%% stop searching if we already know it's not getting picked, else continue
    better(D,M,COL1,U1,COL2,U2,COL,U)                              %%% and choose the better of the two moves (based on their respective utility values).  
	  .

%.......................................
% better
%.......................................
% retourne le meilleur des deux coups selon leur valeur d'utilité respective
%
% si les deux coups ont la même valeur d'utilité, alors un des deux est choisi aléatoirement.
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
