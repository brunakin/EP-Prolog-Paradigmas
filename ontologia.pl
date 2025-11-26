% ONTOLOGIA.PL - Herança e Verificação de Instâncias

% Herança transitiva
herda_trans(F, P) :- herda(F, P).
herda_trans(F, Avo) :- herda(F, P), herda_trans(P, Avo).

% Verificação de instância com herança
instancia_de(Entidade, Classe) :-
    instancia(Entidade, ClasseDireta),
    (ClasseDireta = Classe ; herda_trans(ClasseDireta, Classe)).
