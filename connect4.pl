:- use_module(utils).
:- use_module(output).
:- use_module(ai).
:- use_module(minmax).

controllers([human,random, minimax]).

run :-
    hello,          %%% Display welcome message, initialize game
    play(1);        %%% Play the game starting with player 1
    goodbye. %%% Display end of game message


hello :-
    initialize,
    write('Welcome to connect-4.'), nl,
    player_mark(1,M1),
    read_player(M1 , T1),
    player_mark(2,M2),
    read_player(M2 , T2),
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


read_player(M,T) :-
    controllers(C),
    write('Player \''), write(M), write('\' type '), write(C), write('?'),
    read(T),
    (member(T, C);
     write('Please enter a value in '), write(C), nl, read_player(M, T)).

human_playing(T1,T2) :- 
    write('Is human playing x or o (x moves first)? '),
    read(M),
    (M == 'x' -> T1 = human, T2 = rng;
     M == 'o' -> T1 = minimax, T2 = human;
     write('Please enter x or o.'), nl, human_playing(T1,T2)).

play(P) :-
    board(B), !,
    output_board(B), !,
    make_move(P, B), !,
    board(B2), !,
    not(game_over(P, B2)), !,
    next_player(P, P2), !,
    play(P2), !
    .

game_over(P, B) :-
    (player_mark(P, M), win(B, M)) -> output_winner(P);
    (blank_mark(E), extract_row(B, 6, R), not(member(E,R))) -> output_winner(0).

make_move(P, B) :-
    player(P, Type),
    player_mark(P, M),
    write('Player'), write(M), write(' ('), write(Type), write(') is thinking about next move...'), nl,
    (
        Type == human -> write('Player '), write(P), write(' move? '), read(I);
        Type == random -> random_ai_move(B, M, I);
        Type == minmax -> minmax_AI_move(B, M, I)
    ),
    move(B,I,M,B2),

    write('Player'), write(P), write(' ('), write(Type), write(') plays in column '), write(I), write('.'), nl,
    retract( board(_) ),
    asserta( board(B2) ).