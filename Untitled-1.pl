equals(A,B):- A == B.

utilityestimate_strategique(B,U,PLAYER, COL):- 
    include(equals(PLAYER), B[COL], cases), % Nombre de cases dans la colonne strategique du centre 
    % Nombre de coup gagnants bloqués pour l'adversaire
    inverse_mark(PLAYER,ADV), 
    move(B,COL,ADV,BTEST), 
    findall(1, win(B2,ADV), W), 
    length(W, COUNT), 
    % Estimate
    U is (length(cases) + COUNT*6).