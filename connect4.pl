:- use_module(utils).
:- use_module(output).
:- use_module(random_ai).
:- use_module(minmax).


run :-
    hello,          %%% Display welcome message, initialize game
    play(1);        %%% Play the game starting with player 1
    goodbye. %%% Display end of game message


hello :-
    initialize,
    write('Welcome to connect-4.'),
    read_players(T1,T2),
    asserta( player(1, T1) ),
    asserta( player(2, T2) ), !,
    output_players
    .

initialize :-
    blank_mark(E),
    asserta( board([
        [E, E, E, E, E, E],
        [E, E, E, E, E, E],
        [E, E, E, E, E, E],
        [E, E, E, E, E, E],
        [E, E, E, E, E, E],
        [E, E, E, E, E, E],
        [E, E, E, E, E, E]
    ]) )  %%% create a blank board
    .

goodbye :-
    write('Game over: '),
    nl,
    retract(board(_)),
    retract(player(_,_)),
    read_play_again(V), !,
    (V == 'Y' ; V == 'y'), 
    !,
    run
    .

read_play_again(V) :-
    write('Play again (Y/N)? '),
    read(V),
    (V == 'y' ; V == 'Y' ; V == 'n' ; V == 'N'), !
    .

read_play_again(V) :-
    write('Please enter Y or N.'),
    read_play_again(V)
    .


read_players(T1,T2) :-
    write('Number of human players? '),
    read(N),
    (N == 0 -> T1 = rng, T2 = rng;
     N == 1 -> human_playing(T1,T2);
     N == 2 -> T1 = human, T2 = human;
     write('Please enter 0, 1, or 2.'), nl, read_players(T1,T2))
    .

human_playing(T1,T2) :- 
    write('Is human playing x or o (x moves first)? '),
    read(M),
    (M == 'x' -> T1 = human, T2 = rng;
     M == 'o' -> T1 = rng, T2 = human;
     write('Please enter x or o.'), nl, human_playing(T1,T2)).

play(P) :-
    board(B), !,
    output_board(B), !,
    not(game_over(P, B)), !,
    make_move(P, B), !,
    next_player(P, P2), !,
    play(P2), !
    .

game_over(P, B) :-
    opponent_mark(P, M), win(B, M) -> output_winner(next_player(P));
    blank_mark(E), extract_row(B, 6, R), not(member(E,R)) -> output_winner(0).


make_move(P, B) :-
    player(P, Type),

    make_move2(Type, P, B, B2),

    retract( board(_) ),
    asserta( board(B2) )
    .

make_move2(human, P, B, B2) :-
    write('Player '),
    write(P),
    write(' move? '),
    read(S),
    player_mark(P, M),
    move(B, S, M, B2), !
    .

make_move2(human, P, B, B2) :-
    nl,
    nl,
    write('Please select a numbered square.'),
    make_move2(human,P,B,B2),
    nl
    .

make_move2(rng, P, B, B2) :-
    write('Random AI is thinking about next move...'),
    player_mark(P, M),
    random_ai_move(B, M, I),
    move(B,I,M,B2),
    write('Random IA places '), write(M), write(' in column '), write(I), write('.'), nl,
    sleep(0.5).

make_move2(minmax, P, B, B2) :-
    write('Computer is thinking about next move...'),
    player_mark(P, M),
    minimax(0, B, M, S, U),
    move(B,I,M,B2),
    nl,
    nl,
    write('Computer places '),
    write(M),
    write(' in column '),
    write(I),
    write('.'), nl
    .

% Appliquer un mouvement sur le plateau
move(Board, ColumnIndex, PlayerMark, NewBoard) :-
    nth1(ColumnIndex, Board, Column),                     % Obtenir la colonne spécifiée
    add_token(Column, PlayerMark, NewColumn),             % Ajouter le jeton dans cette colonne
    replace_column(Board, ColumnIndex, NewColumn, NewBoard). % Mettre à jour le plateau

% Ajouter un jeton dans une colonne spécifique
add_token(Column, PlayerMark, NewColumn) :-
    replace_first_empty(Column, PlayerMark, NewColumn).

% Remplacer la première case vide par le jeton du joueur
replace_first_empty([H|Tail], PlayerMark, [PlayerMark|Tail]) :-
    blank_mark(E),
    H == E, !
    .
replace_first_empty([Head|Tail], PlayerMark, [Head|NewTail]) :-
    replace_first_empty(Tail, PlayerMark, NewTail).


% Remplacer une colonne spécifique dans le plateau
replace_column([_|Tail], 1, NewColumn, [NewColumn|Tail]) :- !. % Remplace la première colonne
replace_column([Head|Tail], Index, NewColumn, [Head|NewTail]) :-
    Index > 1,
    NewIndex is Index - 1,
    replace_column(Tail, NewIndex, NewColumn, NewTail).
