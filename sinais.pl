% SINAIS.PL - Detecção de Riscos

:- discontiguous sinal/3.
:- discontiguous sinal_neg/3.

% === PREDICADOS AUXILIARES ===

% Diferença absoluta
absdiff(A, B, D) :- A >= B, !, D is A - B.
absdiff(A, B, D) :- D is B - A.

% Minutos entre timestamps (mesmo dia)
minutos_entre(t(Y,M,D,H1,Min1), t(Y,M,D,H2,Min2), Delta) :-
    M1 is H1*60 + Min1,
    M2 is H2*60 + Min2,
    absdiff(M1, M2, Delta).
minutos_entre(t(Y1,M1,D1,_,_), t(Y2,M2,D2,_,_), 9999) :-
    (Y1 \= Y2 ; M1 \= M2 ; D1 \= D2).

% Contar transações em intervalo
conta_transacoes_intervalo(Cliente, TAtual, JanelaMin, N) :-
    findall(1, (
        trans_hist(Cliente, _, _, _, THist, _, _, _),
        minutos_entre(TAtual, THist, Delta),
        Delta =< JanelaMin,
        Delta >= 0
    ), Lista),
    length(Lista, N).

% === SINAIS POSITIVOS (Aumentam Risco) ===

% 1. Valor acima do perfil (3x ou mais)
sinal(ID, valor_acima_perfil, 25) :-
    transacao(ID, Cliente, _, Valor, _, _, _, _, _, _, _),
    gasto_medio(Cliente, Media),
    Valor >= Media * 3.

% 2. País de alto risco
sinal(ID, pais_alto_risco, 20) :-
    transacao(ID, _, _, _, _, Pais, _, _, _, _, _),
    pais_de_alto_risco(Pais).

% 3. MCC sensível
sinal(ID, mcc_sensivel, 10) :-
    transacao(ID, _, _, _, _, _, MCC, _, _, _, _),
    mcc_sensivel(MCC).

% 4. IP em blacklist
sinal(ID, ip_blacklist, 30) :-
    transacao(ID, _, _, _, _, _, _, _, _, IP, _),
    blacklist_ip(IP).

% 5. Cartão em blacklist
sinal(ID, cartao_blacklist, 40) :-
    transacao(ID, _, _, _, _, _, _, _, _, _, Cartao),
    blacklist_cartao(Cartao).

% 6. Dispositivo em blacklist
sinal(ID, dispositivo_blacklist, 30) :-
    transacao(ID, _, _, _, _, _, _, _, Dispositivo, _, _),
    blacklist_dispositivo(Dispositivo).

% 7. Horário sensível
sinal(ID, horario_sensivel, 5) :-
    transacao(ID, _, _, _, _, _, _, t(_,_,_,H,_), _, _, _),
    horario_sensivel(H).

% 8. Cliente com chargeback prévio
sinal(ID, risco_chargeback_previo, 20) :-
    transacao(ID, Cliente, _, _, _, _, _, _, _, _, _),
    teve_chargeback(Cliente).

% 9. Alta velocidade de transações (3+ em 30 min)
sinal(ID, alta_velocidade_cliente, 15) :-
    transacao(ID, Cliente, _, _, _, _, _, TAtual, _, _, _),
    conta_transacoes_intervalo(Cliente, TAtual, 30, N),
    N >= 3.

% 10. KYC insuficiente para valor alto
sinal(ID, kyc_insuficiente_para_valor, 15) :-
    transacao(ID, Cliente, _, Valor, _, _, _, _, _, _, _),
    Valor >= 1000,
    kyc_nivel(Cliente, Nivel),
    Nivel < 2.

% 11. Geovelocidade improvável (mudança de país em < 2h)
sinal(ID, geovelocidade_improvavel, 25) :-
    transacao(ID, Cliente, _, _, _, PaisAtual, _, TAtual, _, _, _),
    ultima_localizacao(Cliente, PaisAnterior, TAnterior),
    PaisAtual \= PaisAnterior,
    minutos_entre(TAtual, TAnterior, Delta),
    Delta < 120,
    Delta \= 9999.

% === SINAIS NEGATIVOS (Reduzem Risco) ===

% 1. Dispositivo e país habituais
sinal_neg(ID, dispositivo_e_pais_habituais, -10) :-
    transacao(ID, Cliente, _, _, _, Pais, _, _, Dispositivo, _, _),
    usa_dispositivo(Cliente, Dispositivo),
    ultima_localizacao(Cliente, Pais, _).

% 2. Valor dentro do perfil (80% a 120% do gasto médio)
sinal_neg(ID, valor_dentro_perfil, -5) :-
    transacao(ID, Cliente, _, Valor, _, _, _, _, _, _, _),
    gasto_medio(Cliente, Media),
    Valor >= Media * 0.8,
    Valor =< Media * 1.2.



% EXTENSÃO: GRAPH LINK ANALYSIS
% Relacionamento entre entidades (compartilham recursos)
relacionado(E1, E2) :-
    % Compartilham dispositivo
    (usa_dispositivo(E1, D), usa_dispositivo(E2, D), E1 \= E2) ;
    % Compartilham IP em transações
    (transacao(_, E1, _, _, _, _, _, _, _, IP, _),
     transacao(_, E2, _, _, _, _, _, _, _, IP, _), E1 \= E2) ;
    (trans_hist(E1, _, _, _, _, _, IP, _),
     trans_hist(E2, _, _, _, _, _, IP, _), E1 \= E2) ;
    % Compartilham cartão
    (transacao(_, E1, _, _, _, _, _, _, _, _, C),
     transacao(_, E2, _, _, _, _, _, _, _, _, C), E1 \= E2) ;
    (trans_hist(E1, _, _, _, _, _, _, C),
     trans_hist(E2, _, _, _, _, _, _, C), E1 \= E2).

% SINAL 12: Fraude em rede (pelo menos 1 conexão)
sinal(ID, fraude_em_rede, 35) :-
    transacao(ID, Cliente, _, _, _, _, _, _, _, _, _),
    setof(F, (relacionado(Cliente, F), fraude_confirmada(F), Cliente \= F), Fraudadores),
    length(Fraudadores, N),
    N >= 1,
    N < 2.

% SINAL 13: Rede altamente suspeita (2+ conexões)
sinal(ID, rede_altamente_suspeita, 50) :-
    transacao(ID, Cliente, _, _, _, _, _, _, _, _, _),
    setof(F, (relacionado(Cliente, F), fraude_confirmada(F), Cliente \= F), Fraudadores),
    length(Fraudadores, N),
    N >= 2.
