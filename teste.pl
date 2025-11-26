% Arquivo teste para testar o prolog
pai(joao, maria).
pai(joao, pedro).

% Regra
irmao(X, Y) :- pai(Z, X), pai(Z, Y), X \= Y.
