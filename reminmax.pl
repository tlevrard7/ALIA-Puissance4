:- module(reminmax, [minmax/6]).
:- use_module(utils,[win/2, moves/2, move/4]).


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

utility_4aligned(B, U, CP, MAXP) :-
  inverse_mark(MAXP, MINP),
  (
      win(B, MAXP) -> U = 1000;
      win(B, MINP) -> U = (-1000);
      (
          possible_combination(B, MAXP, COUNTMAX),   % Combinaisons gagnantes pour le joueur
          possible_combination(B, MINP, COUNTMIN), % Combinaisons gagnantes pour l adversaire
          U is COUNTMAX - COUNTMIN
      )
  ).

%.............................................................................................
% utility avec nombre de combinaisons où il manque 1 jeton pour avoir un alignement à 4
%.............................................................................................

% Fonction d'estimation basée sur le nombre de combinaisons  où il manque 1 jeton pour avoir un alignement à 4
utility_3aligned(B, U, CP, COL) :-
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
utility_2aligned(B, U, M, Player, COL) :-
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
utility_naif(B, U, M, Player, COL) :-
    inverse_mark(Player, Opponent),
    
    % Calcul des combinaisons pour chaque alignement (3 et 2 jetons)
    utility_3aligned(B, U_3aligned, M, Player, COL),
    utility_2aligned(B, U_2aligned, M, Player, COL),
    
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

utility_strategique(B, U, M, PLAYER, COL):-
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

minmax(B, Depth, CP, MAXP, U, COL):-
  (Depth == 0 ; win(B, x) ; win(B, o); blank_mark(E), extract_row(B, 6, R), not(member(E,R))),
  utility_4aligned(B,U,CP,MAXP),
  !.

minmax(B, Depth, CP, MAXP, U, COL):-
  moves(B,ChildMoves),
  NextDepth is (Depth - 1),
  (CP == MAXP -> max_value_move(B, NextDepth, CP, MAXP, ChildMoves, -inf, _,   U,COL); min_value_move(B, NextDepth, CP, MAXP, ChildMoves, +inf, _,  U,COL)) .



max_value_move(_, _, _, _, [], BestValue, BestMove, BestValue, BestMove).
max_value_move(B, Depth, CP, MAXP, [COL | Rest], CurrentBestValue, CurrentBestMove, BestValue, BestMove) :-
  move(B,COL,CP,B2),
  inverse_mark(CP,OPPONENT),
  minmax(B2, Depth, OPPONENT, MAXP, Value, _),
  %write(CP),write(Depth),write(Value),writeln(COL),
  (
      Value > CurrentBestValue ->
      NewBestValue = Value,
      NewBestMove = COL
  ;
      NewBestValue = CurrentBestValue,
      NewBestMove = CurrentBestMove
  ),
  max_value_move(B, Depth, CP, MAXP, Rest, NewBestValue, NewBestMove, BestValue, BestMove).

min_value_move(_, _, _, _, [], BestValue, BestMove, BestValue, BestMove).
min_value_move(B, Depth, CP, MAXP, [COL | Rest], CurrentBestValue, CurrentBestMove, BestValue, BestMove) :-
  move(B,COL,CP,B2),
  inverse_mark(CP,OPPONENT),
  minmax(B2, Depth, OPPONENT, MAXP, Value, _),
  %write(CP),write(Depth),write(Value),writeln(COL),
  (
      Value < CurrentBestValue ->
      NewBestValue = Value,
      NewBestMove = COL
  ;
      NewBestValue = CurrentBestValue,
      NewBestMove = CurrentBestMove
  ),
  min_value_move(B, Depth, CP, MAXP, Rest, NewBestValue, NewBestMove, BestValue, BestMove).