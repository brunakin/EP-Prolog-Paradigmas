% DECISAO.PL - Pontuação e Decisão

% Agregação de todos os sinais (positivos + negativos)
sinais_ativos(ID, Sinais) :-
    findall((Label, Peso), sinal(ID, Label, Peso), SinaisPos),
    findall((Label, Peso), sinal_neg(ID, Label, Peso), SinaisNeg),
    append(SinaisPos, SinaisNeg, Sinais).

% Cálculo de score total
pontuacao_transacao(ID, Score, Evidencias) :-
    sinais_ativos(ID, Evidencias),
    findall(P, member((_, P), Evidencias), Pesos),
    sum_list(Pesos, Score).

% Decisão baseada em limiares
decisao(ID, aprovar) :-
    pontuacao_transacao(ID, Score, _),
    limiar_revisar(LimRev),
    Score < LimRev.

decisao(ID, revisar) :-
    pontuacao_transacao(ID, Score, _),
    limiar_revisar(LimRev),
    limiar_recusar(LimRec),
    Score >= LimRev,
    Score < LimRec.

decisao(ID, recusar) :-
    pontuacao_transacao(ID, Score, _),
    limiar_recusar(LimRec),
    Score >= LimRec.
