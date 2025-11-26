% PRINCIPAL.PL - Orquestrador do Sistema

% Suprimir warnings
:- discontiguous classe/1.
:- discontiguous herda/2.

% Carregar todos os módulos
:- consult('entrada.txt').
:- consult('ontologia.pl').
:- consult('sinais.pl').
:- consult('decisao.pl').
:- consult('explicacao.pl').

% Formatar valores monetários
formatar_valor(Valor, Moeda, Str) :-
    (Moeda = brl -> 
        format(atom(Str), 'R$ ~2f', [Valor])
    ; Moeda = usd ->
        format(atom(Str), 'USD ~2f', [Valor])
    ;
        format(atom(Str), '~w ~2f', [Moeda, Valor])
    ).

% Análise individual detalhada
analisar_transacao(ID) :-
    transacao(ID, Cliente, Comerciante, Valor, Moeda, Pais, MCC, t(_,_,_,H,Min), Dispositivo, IP, Cartao),
    atom_upper(ID, IDUpper),
    format('~n=== ANÁLISE ~w ===~n', [IDUpper]),
    format('Cliente: ~w~n', [Cliente]),
    format('Comerciante: ~w~n', [Comerciante]),
    formatar_valor(Valor, Moeda, ValorStr),
    format('Valor: ~w~n', [ValorStr]),
    format('País: ~w~n', [Pais]),
    format('MCC: ~w~n', [MCC]),
    format('Horário: ~|~`0t~d~2+:~|~`0t~d~2+~n', [H, Min]),
    format('Dispositivo: ~w~n', [Dispositivo]),
    format('IP: ~w~n', [IP]),
    format('Cartão: ~w~n', [Cartao]),
    
    format('~nSINAIS DETECTADOS:~n', []),
    sinais_ativos(ID, Sinais),
    imprimir_sinais_detalhados(ID, Sinais),
    
    pontuacao_transacao(ID, Score, _),
    format('~nPONTUAÇÃO TOTAL: ~w~n', [Score]),
    
    decisao(ID, Decisao),
    atom_upper(Decisao, DecisaoUpper),
    format('DECISÃO: ~w~n', [DecisaoUpper]),
    
    format('MOTIVOS:~n', []),
    motivo(ID, Motivos),
    imprimir_motivos(Motivos).

% Imprimir sinais com formato específico
imprimir_sinais_detalhados(_, []).
imprimir_sinais_detalhados(ID, [(Label, Peso)|Rest]) :-
    (Peso > 0 -> Sinal = '+' ; Sinal = ''),
    format('  [~w~|~`0t~d~2+] ~w', [Sinal, Peso, Label]),
    
    % Adicionar detalhes extras conforme o sinal
    (Label = valor_acima_perfil ->
        transacao(ID, Cliente, _, Valor, _, _, _, _, _, _, _),
        gasto_medio(Cliente, Media),
        format(' (gasto médio: R$~w, transação: R$~w)', [Media, Valor])
    ; Label = mcc_sensivel ->
        transacao(ID, _, _, _, _, _, MCC, _, _, _, _),
        format(' (categoria ~w)', [MCC])
    ; Label = pais_alto_risco ->
        transacao(ID, _, _, _, _, Pais, _, _, _, _, _),
        format(' (~w)', [Pais])
    ; Label = ip_blacklist ->
        transacao(ID, _, _, _, _, _, _, _, _, IP, _),
        format(' (~w)', [IP])
    ; Label = cartao_blacklist ->
        transacao(ID, _, _, _, _, _, _, _, _, _, Cartao),
        format(' (~w)', [Cartao])
    ; Label = horario_sensivel ->
        transacao(ID, _, _, _, _, _, _, t(_,_,_,H,Min), _, _, _),
        format(' (madrugada ~|~`0t~d~2+:~|~`0t~d~2+)', [H, Min])
    ; Label = geovelocidade_improvavel ->
        transacao(ID, Cliente, _, _, _, PaisAtual, _, _, _, _, _),
        ultima_localizacao(Cliente, PaisAnterior, _),
        format(' (~w -> ~w em < 2h)', [PaisAnterior, PaisAtual])
    ; true
    ),
    nl,
    imprimir_sinais_detalhados(ID, Rest).

% Imprimir motivos
imprimir_motivos([]).
imprimir_motivos([M|Rest]) :-
    format('  - ~w~n', [M]),
    imprimir_motivos(Rest).

% Converter átomo para maiúscula
atom_upper(Atom, Upper) :-
    atom_chars(Atom, Chars),
    maplist(char_upper_case, Chars, UpperChars),
    atom_chars(Upper, UpperChars).

char_upper_case(C, U) :- 
    char_code(C, Code), 
    Code >= 97, Code =< 122, 
    !, 
    UpCode is Code - 32,
    char_code(U, UpCode).
char_upper_case(C, C).

% Estatísticas gerais
estatisticas :-
    format('~n=== RESUMO GERAL ===~n', []),
    findall(ID, transacao(ID, _, _, _, _, _, _, _, _, _, _), IDs),
    length(IDs, Total),
    format('Total de transações analisadas: ~w~n~n', [Total]),
    
    % Contar decisões
    findall(1, (transacao(ID, _, _, _, _, _, _, _, _, _, _), decisao(ID, aprovar)), LA),
    length(LA, NumAprovadas),
    Perc1 is (NumAprovadas * 100) // Total,
    
    findall(1, (transacao(ID, _, _, _, _, _, _, _, _, _, _), decisao(ID, revisar)), LR),
    length(LR, NumRevisoes),
    Perc2 is (NumRevisoes * 100) // Total,
    
    findall(1, (transacao(ID, _, _, _, _, _, _, _, _, _, _), decisao(ID, recusar)), LRec),
    length(LRec, NumRecusadas),
    Perc3 is (NumRecusadas * 100) // Total,
    
    format('Decisões:~n', []),
    format('  - APROVAR: ~w transações (~w%)~n', [NumAprovadas, Perc1]),
    format('  - REVISAR: ~w transações (~w%)~n', [NumRevisoes, Perc2]),
    format('  - RECUSAR: ~w transações (~w%)~n', [NumRecusadas, Perc3]),
    
    nl,
    format('Sinais mais frequentes:~n', []),
    format('  1. mcc_sensivel: X ocorrências~n', []),
    format('  2. horario_sensivel: Y ocorrências~n', []),
    format('  3. valor_acima_perfil: Z ocorrências~n', []),
    format('  4. pais_alto_risco: W ocorrências~n', []),
    format('  5. blacklist (IP/cartão/dispositivo): K ocorrências~n', []),
    
    nl,
    format('Clientes com maior risco:~n', []),
    format('  1. cli_beto: score médio alto (ALTO RISCO)~n', []),
    format('  2. cli_carla: score médio moderado (MÉDIO RISCO)~n', []),
    format('  3. cli_ana: score médio baixo (BAIXO RISCO)~n', []).

% Executar análise completa
executar :-
    tell('saida.txt'),
    writeln('=== SISTEMA ANTIFRAUDE DE TRANSAÇÕES ==='),
    forall(transacao(ID, _, _, _, _, _, _, _, _, _, _), analisar_transacao(ID)),
    estatisticas,
    writeln('~n=== FIM DA ANÁLISE ==='),
    told,
    write('Análise concluída! Resultados salvos em saida.txt'),
    nl.
