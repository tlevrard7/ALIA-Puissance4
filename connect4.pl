:- use_module(utils).
:- use_module(output).
:- use_module(ai).
:- use_module(minmax).

controllers([human,random, minmax]).

run :- initialize, play(1); exit.

initialize :-
    write('Welcome to connect-4.'), nl,
    player_mark(1,M1),
    read_player(M1 , T1),
    player_mark(2,M2),
    read_player(M2 , T2),
    asserta( player(1, T1) ),
    asserta( player(2, T2) ), !,
    output_players,
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

exit :-
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
    write('Play again [y,n]? '),
    read(V),
    (V == 'y' ; V == 'n'), !.
read_play_again(V) :-
    write('Please enter y or n.'),
    read_play_again(V).

read_player(M,T) :-
    controllers(C),
    write(M), write(' controller '), write(C), write('?'),
    read(T), member(T, C).
read_player(M,T) :-
    controllers(C),
    write('Please enter a value in '), write(C), nl, read_player(M, T).

play(P) :-
    board(B), !,
    output_board(B), !,
    next_player(P, P2), !,
    not(game_over(P2, B)), !,
    make_move(P, B), !,
    play(P2), !.

game_over(P, B) :-
    (player_mark(P, M), win(B, M)) -> output_winner(P);
    (blank_mark(E), extract_row(B, 6, R), not(member(E,R))) -> output_winner(0).

human_move(B, M, I) :-
    moves(B, C),
    write('Player '), write(M), write(' move? '),
    read(I), member(I, C).
human_move(B, M, I) :-
    moves(B, C),
    write('Valid moves are '), write(C), write('. '), human_move(B, M, I).

make_move(P, B) :-
    player(P, Type),
    player_mark(P, M),
    write('Player'), write(M), write(' ('), write(Type), write(') is thinking about next move...'), nl,
    (
        Type == human -> human_move(B, M, I);
        Type == random -> random_ai_move(B, M, I);
        Type == minmax -> minmax_AI_move(B, M, I)
    ),
    move(B,I,M,B2),

    write('Player'), write(M), write(' ('), write(Type), write(') plays in column '), write(I), write('.'), nl,
    retract( board(_) ),
    asserta( board(B2) ).