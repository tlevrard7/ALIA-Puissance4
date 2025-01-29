:- module(reminmax, [minmax/5]).
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

utilityestimate_4aligned(B, U) :-
  (
      win(B, o) -> U = 1000;
      win(B, x) -> U = (-1000);
      (
          possible_combination(B, o, COUNTAI),   % Combinaisons gagnantes pour le joueur
          possible_combination(B, x, COUNTPHUM), % Combinaisons gagnantes pour l adversaire
          U is COUNTAI - COUNTPHUM
      )
  ).

minmax(B,Depth,_, U, COL):-
  (Depth == 0 ; win(B, x) ; win(B, o); blank_mark(E), extract_row(B, 6, R), not(member(E,R))),
  utilityestimate_4aligned(B,U),
  !.

minmax(B,Depth,CP, U, COL):-
  moves(B,ChildMoves),
  NextDepth is (Depth - 1),
  (CP == o -> max_value_move(B, NextDepth, CP, ChildMoves, -inf, _,   U,COL); min_value_move(B, NextDepth, CP, ChildMoves, +inf, _,  U,COL)) .

max_value_move(_, _, _,[], BestValue, BestMove, BestValue, BestMove).
max_value_move(B, Depth, CP, [COL | Rest], CurrentBestValue, CurrentBestMove, BestValue, BestMove) :-
  move(B,COL,CP,B2),
  inverse_mark(CP,OPPONENT),
  minmax(B2, Depth, OPPONENT, Value, _),
  %write(CP),write(Depth),write(Value),writeln(COL),
  (
      Value > CurrentBestValue ->
      NewBestValue = Value,
      NewBestMove = COL
  ;
      NewBestValue = CurrentBestValue,
      NewBestMove = CurrentBestMove
  ),
  max_value_move(B, Depth, CP, Rest, NewBestValue, NewBestMove, BestValue, BestMove).

min_value_move(_, _, _,[], BestValue, BestMove, BestValue, BestMove).
min_value_move(B, Depth, CP, [COL | Rest], CurrentBestValue, CurrentBestMove, BestValue, BestMove) :-
  move(B,COL,CP,B2),
  inverse_mark(CP,OPPONENT),
  minmax(B2, Depth, OPPONENT, Value, _),
  %write(CP),write(Depth),write(Value),writeln(COL),
  (
      Value < CurrentBestValue ->
      NewBestValue = Value,
      NewBestMove = COL
  ;
      NewBestValue = CurrentBestValue,
      NewBestMove = CurrentBestMove
  ),
  min_value_move(B, Depth, CP, Rest, NewBestValue, NewBestMove, BestValue, BestMove).