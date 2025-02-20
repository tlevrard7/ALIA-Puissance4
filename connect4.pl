:- use_module(utils).
:- use_module(output).
:- use_module(ai).
:- use_module(reminmax).

player_types([human,random, minmax_winnings, minmax_naif, minmax_strategique]). % Liste contenant tous les types de joueurs possibles
ai_types([random, minmax_winnings, minmax_naif, minmax_strategique]). % Liste contenant tous les types de joueurs possibles

% Point d'entrée du programme
run :- 
    initialize(B,C1,C2), 
    player_mark(1, M1),           % On associe à chacun des joueurs un symbole (M1 ou M2)
    play_output(B, C1, C2, M1, _), % Le joueur 1 commence à jouer
    exit.

% Initialise le jeu en configurant les joueurs (selon les entrées de l'utilisateur) et la grille de jeu
% initialize(-B, -C1, -C2)
initialize(B, C1, C2) :-
    write('Welcome to connect-4.'), nl,
    player_mark(1, M1),           % On associe à chacun des joueurs un symbole (M1 ou M2)
    read_player(M1, C1),          % Puis pour chaque symbole, on demande à l'utilisateur quel type (T1 et T2) de joueur jouera avec
    player_mark(2, M2),            
    read_player(M2, C2),           
    output_players(M1, C1, M2, C2),               % Affiche la liste des joueurs
    empty_board(B),
    !.

% Quitte le jeu ou redémarre une nouvelle partie si l'utilisateur le souhaite
exit :-
    write('Game over: '), nl,
    read_play_again(V), !,  % Demande à l'utilisateur s'il veut rejouer
    (V == 'Y' ; V == 'y'), 
    !,
    run
    .

% Simule toutes les combinaisons possibles de parties entre les différents types d'IA.
simulate :-
    ai_types(T),
    forall(member(C1, T), (
        forall(member(C2, T), (
            simulate_match(C1, C2)
        ))
    )).

% Simule une partie entre deux IA C1 et C2 en jouant un coup dans chaque colonne
% simulate_match(+C1, +C2)
simulate_match(C1, C2):-
    write(C1) , write('-'), write(C2), write(':'),
    empty_board(B),
    forall(between(1, 7, M), (
        move(B, M, x, B2),
        % nl, output_board(B2), nl,
        play(B2, C2, C1, o, W),
        write(W)
    )),
    nl.

% read_play_again(-V)
% Demande à l'utilisateur s'il veut rejouer et récupère sa réponse dans V
read_play_again(V) :-
    write('Play again [y,n]? '),
    read(V),
    (V == 'y' ; V == 'n'), !.       % Vérifie la validité de la réponse
read_play_again(V) :-               % Si la réponse n'est pas valide alors on redemande
    write('Please enter y or n.'),  
    read_play_again(V).

% read_player (+M, -T)
% Lit et valide le type de joueur 'T' (choisi par l'utilisateur) associé au symbole 'M'
read_player(M,T) :-
    player_types(C),
    write(M), write(' controller '), write(C), write('?'),
    read(T), member(T, C). % On vérifie bien que le type de joueur saisi existe
read_player(M,T) :-        % Si ce n'est pas le cas alors on redemande
    player_types(C),
    write('Please enter a value in '), write(C), nl, read_player(M, T).

% play_output(+B, +C1, +C2, +P, -W)
% Boucle de jeu principale : alterne les tours entre les joueurs jusqu'à la fin de la partie. P est le joueur dont c'est le tour actuellement. W représente le gagnant de la partie
play_output(B, C1, C2, P, W) :-
    output_board(B), !,       % Affiche la grille actuelle
    write('Player '), write(P), write(' ('), write(C1), write(') is thinking about next move...'), nl,
    pick_move(P, B, C1, I), !,       % Joue le coup de P
    write('Player '), write(P), write(' ('), write(C1), write(') plays in column '), write(I), write('.'), nl,
    move(B, I, P, B2), % Ajoute le jeton 'P' du joueur dans la colonne 'I' du plateau 'B' et calcule le nouveau plateau 'B2'
    (
     (win(B2, P) -> W = P, output_board(B2), output_winner(W));
     ((blank_mark(E), extract_row(B2, 6, R), not(member(E,R))) -> W = E, output_board(B2), output_winner(W));
     (inverse_mark(P, P2), play_output(B2, C2, C1, P2, W))
    ).

% play(+B, +C1, +C2, +P, -W)
play(B, C1, C2, P, W) :-
    pick_move(P, B, C1, I), !,       % Joue le coup de P
    move(B, I, P, B2), % Ajoute le jeton 'P' du joueur dans la colonne 'I' du plateau 'B' et calcule le nouveau plateau 'B2'
    (
     win(B2, P) -> W = P;
     (blank_mark(E), extract_row(B2, 6, R), not(member(E,R))) -> W = E;
     (inverse_mark(P, P2), play(B2, C2, C1, P2, W))
    ).

% human_move(+B,+M,-I)
% Demande au joueur humain (associé au symbole 'M') son choix de coup 'I' pour le plateau 'B'
human_move(B, M, I) :-
    moves(B, C),  % Calcule la liste 'C' des coups valides 
    write('Player '), write(M), write(' move? '), 
    read(I), member(I, C). % Vérifie que le coup choisi est valide
human_move(B, M, I) :-     % Si ce n'est pas le cas alors on redemande
    moves(B, C),
    write('Valid moves are '), write(C), write('. '), human_move(B, M, I).

% pick_move(+P,+B, +C, -I)
% Effectue le coup du joueur 'P' dans la grille 'B'
% Utilities: utilityestimate_4aligned, utilityestimate_naif, utilityestimate_strategique
pick_move(P, B, C, I) :-
    (
        C == human -> human_move(B, P, I);                                % Détermine le coup si le joueur est humain
        C == random -> random_move(B, P, I);                           % Détermine le coup si IA aléatoire
        C == minmax_winnings -> minmax_winnings_move(B, P, I);            % Détermine le coup si IA Minimax
        C == minmax_naif -> minmax_naif_move(B, P, I);                    % Détermine le coup si IA Minimax
        C == minmax_strategique -> minmax_strategique_move(B, P, I)        % Détermine le coup si IA Minimax
    ).