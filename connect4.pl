:- use_module(utils).
:- use_module(output).


run :-
    hello,          %%% Display welcome message, initialize game
    play(1).        %%% Play the game starting with player 1

run :- %%% Display end of game message
    goodbye.


hello :-
    initialize,
    write('Welcome to connect-4.'),
    read_players,
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


read_players :-
    write('Number of human players? '),
    read(N),
    set_players(N)
    .

set_players(0) :- 
    asserta( player(1, computer) ),
    asserta( player(2, computer) ), !
    .

set_players(1) :-
    nl,
    write('Is human playing X or O (X moves first)? '),
    read(M),
    human_playing(M), !
    .

set_players(2) :- 
    asserta( player(1, human) ),
    asserta( player(2, human) ), !
    .

set_players(_) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players
    .


human_playing(M) :- 
    (M == 'x' ; M == 'X'),
    asserta( player(1, human) ),
    asserta( player(2, computer) ), !
    .

human_playing(M) :- 
    (M == 'o' ; M == 'O'),
    asserta( player(1, computer) ),
    asserta( player(2, human) ), !
    .

human_playing(_) :-
    nl,
    write('Please enter X or O.'),
    set_players(1)
    .


play(P) :-
    board(B), !,
    output_board(B), !,
    not(game_over(P, B)), !,
    make_move(P, B), !,
    next_player(P, P2), !,
    play(P2), !
    .

game_over(P, B) :-
    game_over2(P, B)
    .

game_over2(P, B) :-
    opponent_mark(P, M),   %%% game is over if player wins
    win(B, M),
    output_winner(P)
    .

game_over2(P, B) :- %%% game is over board empty
    blank_mark(E),
    extract_row(B, 6, R),
    not(member(E,R)), %%% game no empty cell
    output_winner(0)
    .


% Vérifier si un joueur a gagné
win(B, M) :-
    rows(B, R),
    member(Row, R),
    four_in_a_row(Row, M).

win(B, M) :-
    columns(B, Columns),
    member(Column, Columns),
    four_in_a_row(Column, M).

win(B, M) :-
    diagonals(B, Diagonals),
    member(Diagonal, Diagonals),
    four_in_a_row(Diagonal, M).

% Vérifier 4 jetons consécutifs dans une liste
four_in_a_row([M, M, M, M|_], M) :-
    blank_mark(E),
    M \= E.
four_in_a_row([_|Tail], M) :- four_in_a_row(Tail, M).


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

make_move2(computer, P, B, B2) :-
    nl,
    nl,
    write('Computer is thinking about next move...'),
    player_mark(P, M),
    % minimax(0, B, M, S, U),
    I == 5,
    move(B,I,M,B2),
    nl,
    nl,
    write('Computer places '),
    write(M),
    write(' in column '),
    write(I),
    write('.')
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
