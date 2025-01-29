:- module(minmax, [minimax/9]).
:- use_module(utils,[win/2, moves/2, move/4]).


%.......................................
% utility
%.......................................
% détermine la valuer de la position donné du plateau
%

utility(B,U,PLAYER) :-
    win(B,PLAYER),
    U = 1000, % il y a 69 combinaisons gagnantes dans puissance 4
    !
    .

utility(B,U,PLAYER) :-
    inverse_mark(PLAYER,ADV),
    win(B,ADV),
    U = (-1000), % il y a 69 combinaisons gagnantes dans puissance 4
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
replace_blank([E|R],[M|T],M):- blank_mark(E), replace_blank(R,T,M).
replace_blank([L|R],[L|T],M):-  blank_mark(E), L \= E, replace_blank(R,T,M).

replace_blank_list([],[],_).
replace_blank_list([L1|R],[L2|T],M):- replace_blank(L1,L2,M), replace_blank_list(R,T,M),!.

possible_combination(B, M, COUNT) :-
    replace_blank_list(B, L, M),      % Remplace les cases vides par le jeton du joueur
    findall(1, win(L, M), W),         % Trouve toutes les combinaisons gagnantes pour le joueur
    length(W, COUNT)                  % Compte ces combinaisons
    .             
   
%.......................................
% utility de base avec seulement combinaisons gagnantes
%.......................................


utilityestimate_4aligned(B, U, _, Player, _) :-
    inverse_mark(Player, Opponent),
    (win(B, Player) -> U = 1000;
     win(B, Opponent) -> U = -1000;
     (three_in_a_row_detected(B, Opponent) -> U = -900;
      possible_combination(B, Player, COUNTAI),
      possible_combination(B, Opponent, COUNTP1),
      U is COUNTAI - COUNTP1)).

%.............................................................................................
% utility avec nombre de combinaisons où il manque 1 jeton pour avoir un alignement à 4
%.............................................................................................

% Fonction d'estimation basée sur le nombre de combinaisons  où il manque 1 jeton pour avoir un alignement à 4
utilityestimate_3aligned(B, U, M, Player, COL) :-
    inverse_mark(Player, Opponent),
    possible_3aligned(B, Player, CountP1),
    possible_3aligned(B, Opponent, CountP2),
    U is CountP1 - CountP2.

% possible_3aligned(+B, +M, -COUNT)
% Compte le nombre de combinaisons où il manque 1 jeton pour un alignement à 4 (dans les lignes, colonnes, et diagonales)
possible_3aligned(B, M, COUNT) :-
    findall(1, (rows(B, R), member(Row, R), three_in_a_row(Row, M)), RowCount),
    findall(1, (columns(B, C), member(Column, C), three_in_a_row(Column, M)), ColCount),
    findall(1, (diagonals(B, D), member(Diagonal, D), three_in_a_row(Diagonal, M)), DiagCount),
    length(RowCount, NbThreeAlignedInRow),
    length(ColCount, NbThreeAlignedInCol),
    length(DiagCount, NbThreeAlignedInDiag),
    COUNT is NbThreeAlignedInRow + NbThreeAlignedInCol + NbThreeAlignedInDiag.

% three_in_a_row(+L, +M)
% Vérifie s'il manque 1 jeton pour avoir un alignement à 4 dans la liste L
three_in_a_row([M, M, M, '.'|_], M).  % Trois jetons 'M' suivis d'un jeton vide
three_in_a_row(['.' ,M, M, M|_], M).  % Un jeton vide suivi de trois jetons 'M'
three_in_a_row([M, '.', M, M|_], M).  % Un jeton vide au milieu de 3 jetons 'M'
three_in_a_row([M, M, '.', M|_], M).  % Un jeton vide à la fin de 3 jetons 'M'
three_in_a_row([_|Tail], M) :- three_in_a_row(Tail, M).  % Recherche récursive dans la liste



%.............................................................................................
% utility avec nombre de combinaisons où il manque 1 jeton pour avoir un alignement à 3
%.............................................................................................

% Fonction d'estimation basée sur le nombre de combinaisons où il manque 1 jeton pour avoir un alignement à 3
utilityestimate_2aligned(B, U, M, Player, COL) :-
    inverse_mark(Player, Opponent),
    possible_2aligned(B, Player, CountP1),
    possible_2aligned(B, Opponent, CountP2),
    U is CountP1 - CountP2.

% possible_2aligned(+B, +M, -COUNT)
% Compte le nombre de combinaisons
possible_2aligned(B, M, COUNT) :-
    findall(1, (rows(B, R), member(Row, R), two_in_a_row(Row, M)), RowCount),
    findall(1, (columns(B, C), member(Column, C), two_in_a_row(Column, M)), ColCount),
    findall(1, (diagonals(B, D), member(Diagonal, D), two_in_a_row(Diagonal, M)), DiagCount),
    length(RowCount, NbTwoAlignedInLine),
    length(ColCount, NbTwoAlignedInCol),
    length(DiagCount, NbTwoAlignedInDiag),
    COUNT is NbTwoAlignedInLine + NbTwoAlignedInCol + NbTwoAlignedInDiag.

% two_in_a_row(+L, +M)
% Vérifie s'il manque 1 jeton pour un alignement à 3 dans la liste L
two_in_a_row([M, M, '.', _|_], M).  % Deux jetons 'M' suivis d'un jeton vide
two_in_a_row([M, '.', M, _|_], M).  % Un jeton vide au milieu de 2 jetons 'M'
two_in_a_row(['.', M, M, _|_], M).  % Un jeton vide suivi de 2 jetons 'M'
two_in_a_row([_|Tail], M) :- two_in_a_row(Tail, M).  % Recherche récursive dans la liste

%.............................................................................................
% utility avec pondérations pour 4aligned, 3aligned, 2aligned
%.............................................................................................

% Fonction d'estimation globale en tenant compte des alignements à 2, 3 jetons
utilityestimate_naif(B, U, M, Player, COL) :-
    inverse_mark(Player, Opponent),
    
    % Calcul des combinaisons pour chaque alignement (3 et 2 jetons)
    utilityestimate_3aligned(B, U_3aligned, M, Player, COL),
    utilityestimate_2aligned(B, U_2aligned, M, Player, COL),
    
    % Pondération des alignements 
    WeightP3 = 5,  
    WeightP2 = 1,    
    
    % Calcul de l'utilité : les scores pour le joueur et l'adversaire sont comparés
    U is (WeightP3 * (U_3aligned) +
          WeightP2 * (U_2aligned)).

%.............................................................................................
% utility avec nombre de jetons sur la colonne centrale et coups bloqués à l'adversaire.
%.............................................................................................

equals(A,B):- A == B.

utilityestimate_strategique(B, U, M, PLAYER, COL):-
    % Nombre de cases dans la colonne strategique du centre
    nth1(4,B,COLONNE_CENTRALE),
    include(equals(M),COLONNE_CENTRALE, CASES),
    length(CASES, NCASES),
    % Nombre de coup gagnants bloqués pour l'adversaire
    inverse_mark(M,OPPONENT), 
    move(B,COL,OPPONENT,BTEST), 
    findall(1, win(BTEST,OPPONENT), W), 
    length(W, COUNT), 
    % Estimate
    (M == Player -> U is (NCASES + COUNT*6); U is -(NCASES + COUNT*6)).

%.......................................
% minimax
%.......................................
% L'algorithme minimax considère toujours que l'adversaire fera le meilleur choix.



% Pour le coup d'ouverture, on choisit la meilleur stratégie connue : commencer au centre (colonne 4)
% Permet d'éviter d'attendre que l'ordinateur calcule l'entièreté de l'arbre minimax
minimax(_,[[E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E], [E,E,E,E,E,E]],M,COL,U, _, _, _, _) :-   
    blank_mark(E),
    COL = 4,
    !.

minimax(D,B,M,COL,U, ALPHA, BETA, PLAYER, Utility_func) :-
    D2 is D + 1,
    not(dmax(D2)),
    not(win(B, x)),  %%% Auccun gagnant sinon utility
    not(win(B, o)),
    moves(B,L),  %%% obtient la liste de tous les coups possible
    %writeln("moved"),
    %writeln(L),
    L\=[],       %%% not égalité
    %writeln("continued"),
    %writeln(L),
    !,          
    best(D2,B,M,L,COL,U, ALPHA, BETA,PLAYER, Utility_func), %%% détermine récursivement le meilleur coup possible
    %writeln('did best'),
    !. 


% si il y a pas de coup possible,
% alors la valeur minimax est l'utilité de la position du plateau donnée
% Si la profondeur maximal est atteint, alors retourne une estimation

minimax(_,B,_,_,U, _, _, PLAYER, Utility_func) :-
    % writeln("utilitied"),
    %not(dmax(D2)),
    %(win(B,x);win(B,o)),
    utility(B,U,PLAYER),

    %writeln(U),
    !. 

dmax(2).
minimax(D,B,M,COL,U, _, _, PLAYER, Utility_func) :-
    writeln("utilitied"),
    call(Utility_func, B, U, M, PLAYER, COL). 


   
    


%.......................................
% best
%.......................................
% détermine le meilleur coup possible d'une liste de coup donnée produit récursivement par minimax
%

% Pruning
alpha_beta_pruning(D,B,M,MOVES,U1, ALPHA, BETA, U2, COL2,PLAYER, Utility_func):-
    M \= PLAYER,    %%% Si je suis en train de minimiser, alors le joueur précédent était en train maximiser avec l'option ALPHA.
    U1 =< ALPHA,      %%% Donc si (n'importe quels autres après =< COL =< ALPHA) alors on sait que cette branche ne sera pas choisi
	!.
alpha_beta_pruning(D,B,M,MOVES,U1, ALPHA, BETA, U2, COL2,PLAYER, Utility_func):-
	NEWBETA is min(BETA,U1),
    best(D,B,M,MOVES,COL2,U2, ALPHA, NEWBETA,PLAYER, Utility_func)
    .

alpha_beta_pruning(D,B,M,MOVES,U1, ALPHA, BETA, U2, COL2,PLAYER, Utility_func):-
    M == PLAYER,    %%% Si je suis en train de maximiser, alors le joueur précédent était en train minimiser avec l'option BETA.
    U1 >= BETA,       %%% Donc si (n'importe quels autres après >= COL >= BETA) alors on sait que cette branche ne sera pas choisi
	!.

alpha_beta_pruning(D,B,M,MOVES,U1, ALPHA, BETA, U2, COL2,PLAYER, Utility_func):-
	NEWALPHA is max(ALPHA,U1),
    best(D,B,M,MOVES,COL2,U2, NEWALPHA, BETA,PLAYER, Utility_func)
    .


% si il n'y a plus de coup restant
best(_,_,_,[],_,_, _, _,_,_).


% si il y a seulement un seul coup restant dans la liste...
best(D,B,M,[COL1],COL,U, ALPHA, BETA,PLAYER, Utility_func) :-
    move(B,COL1,M,B2),        %%% applique ce coup au plateau, !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    inverse_mark(M,M2), 
    !,  
    minimax(D,B2,M2,_COL,U, ALPHA, BETA,PLAYER, Utility_func),  %%% et cherche récursivement la valeur d'utilité de ce coup.
    COL = COL1, !
    .

% si il y a plus d'un coup dans la liste...
best(D,B,M,[COL1|T],COL,U, ALPHA, BETA,PLAYER, Utility_func) :-
    move(B,COL1,M,B2),        %%% applique le premier coup (dans la liste) au plateau, !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    inverse_mark(M,M2), 
    !,
    minimax(D,B2,M2,_COL,U1, ALPHA, BETA,PLAYER, Utility_func),                  %%% cherche récursivement la valeur d'utilité de ce coup,
    %alpha_beta_pruning(D,B,M,T,U1, ALPHA, BETA, U2, COL2,PLAYER, Utility_func),  %%% arrête de chercher si on sait déjà qu'il ne sera pas choisi, sinon on continue
    best(D,B,M,T,COL2, U2, ALPHA, BETA, PLAYER, Utility_func),
    better(D,M,PLAYER,COL1,U1,COL2,U2,COL,U)                                            %%% et choisit le meilleur des 2 coups (selon leur valeur d'utilité respective).
    .                              


%.......................................
% better
%.......................................
% retourne le meilleur des deux coups selon leur valeur d'utilité respective
%
% si les deux coups ont la même valeur d'utilité, alors un des deux est choisi aléatoirement.
better(_, M, Player, COL1, U1, COL2, U2, COL, U) :-
    M == Player,    % Si le joueur maximise
    U1 > U2,
    COL = COL1,
    U = U1,
    !.

better(_, M, Player, COL1, U1, COL2, U2, COL, U) :-
    M \= Player,    % Si le joueur minimise
    U1 < U2,
    COL = COL1,
    U = U1,
    !.

better(_, _, _, COL1, U1, COL2, U2, COL, U) :-
    U1 == U2,              % Si les utilités sont égales
    COL = COL1,
    U = U1,
    !.

better(_, _, _, _, _, COL2, U2, COL, U) :- % Par défaut, on choisit le deuxième coup
    COL = COL2,
    U = U2,
    !.
