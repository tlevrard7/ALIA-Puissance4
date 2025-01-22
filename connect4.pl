:- use_module(utils).
:- use_module(output).
:- use_module(ai).
:- use_module(minmax).

player_types([human,random, minmax]). % Liste contenant tous les types de joueurs possibles

% Point d'entrée du programme
run :- 
    initialize, 
    play(1); % Le joueur 1 commence à jouer
    exit.

% Initialise le jeu en configurant les joueurs (selon les entrées de l'utilisateur) et la grille de jeu
initialize :-
    write('Welcome to connect-4.'), nl,
    player_mark(1, M1),           % On associe à chacun des joueurs un symbole (M1 ou M2)
    read_player(M1, T1),          % Puis pour chaque symbole, on demande à l'utilisateur quel type (T1 et T2) de joueur jouera avec
    player_mark(2, M2),            
    read_player(M2, T2),           
    asserta( player(1, T1) ),     % Pour chaque joueur on ajoute un fait dynamique avec son numéro et son type
    asserta( player(2, T2) ), !,  
    output_players,               % Affiche la liste des joueurs
    blank_mark(E),                
    asserta( board([              % Initialise la grille de jeu avec le symbole correspondant à une case vide (récupéré au dessus) 
        [E, E, E, E, E, E],       % On ajoute dynamiquement un fait pour celle-ci
        [E, E, E, E, E, E],        
        [E, E, E, E, E, E],
        [E, E, E, E, E, E],
        [E, E, E, E, E, E],
        [E, E, E, E, E, E],
        [E, E, E, E, E, E]
    ]) )  
    .

% Quitte le jeu ou redémarre une nouvelle partie si l'utilisateur le souhaite
exit :-
    write('Game over: '),
    nl,
    retract(board(_)),      % Supprime la grille actuelle
    retract(player(_,_)),   % Supprime les informations des joueurs
    read_play_again(V), !,  % Demande à l'utilisateur s'il veut rejouer
    (V == 'Y' ; V == 'y'), 
    !,
    run
    .

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

% play(+P)
% Boucle de jeu principale : alterne les tours entre les joueurs jusqu'à la fin de la partie. P est le joueur dont c'est le tour actuellement
play(P) :-
    board(B), !,              
    output_board(B), !,       % Affiche la grille actuelle
    next_player(P, P2), !,    % Récupère le joueur suivant dans P2

    not(game_over(P2, B)), !, % Vérifie que la partie n'est pas terminée suite au dernier coup
    make_move(P, B), !,       % Joue le coup de P
    play(P2), !.              % Alterne le tour

% game_over(+P, +B)
% Vérifie si la partie est terminée et affiche la conclusion :
% Pour la grille 'B' donnée : (joueur 'P' a gagné) OU (égalité car la grille est pleine )
game_over(P, B) :-
    (player_mark(P, M), win(B, M)) -> output_winner(P); % Vérifie si P a gagné
    (blank_mark(E), extract_row(B, 6, R), not(member(E,R))) -> output_winner(0). % Vérifie s'il y a égalité (grille pleine)

% human_move(+B,+M,-I)
% Demande au joueur humain (associé au symbole 'M') son choix de coup 'I' pour le plateau 'B'
human_move(B, M, I) :-
    moves(B, C),  % Calcule la liste 'C' des coups valides 
    write('Player '), write(M), write(' move? '), 
    read(I), member(I, C). % Vérifie que le coup choisi est valide
human_move(B, M, I) :-     % Si ce n'est pas le cas alors on redemande
    moves(B, C),
    write('Valid moves are '), write(C), write('. '), human_move(B, M, I).

% make_move(+P,+B)
% Effectue le coup du joueur 'P' dans la grille 'B'
make_move(P, B) :-
    player(P, Type),                       % Détermine le type de joueur (human, random, minmax)
    player_mark(P, M),                     % Récupère le marqueur du joueur ('x' ou 'o')
    write('Player '), write(M), write(' ('), write(Type), write(') is thinking about next move...'), nl,
    (
        Type == human -> human_move(B, M, I);          % Détermine le coup si le joueur est humain
        Type == random -> random_ai_move(B, M, I);     % Détermine le coup si IA aléatoire
        Type == minmax -> minmax_AI_move(B, M, I)      % Détermine le coup si IA Minimax
    ),
    move(B, I, M, B2), % Ajoute le jeton 'M' du joueur dans la colonne 'I' du plateau 'B' et calcule le nouveau plateau 'B2'

    write('Player'), write(M), write(' ('), write(Type), write(') plays in column '), write(I), write('.'), nl,
    retract( board(_) ),   % Retire l'ancien plateau
    asserta( board(B2) ).  % Stocke le nouveau plateau