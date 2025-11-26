% EXPLICACAO.PL - Mensagens Humanizadas

:- discontiguous rotulo/2.

% Rótulos legíveis para cada sinal
rotulo(valor_acima_perfil, 'Valor muito acima do perfil do cliente').
rotulo(pais_alto_risco, 'País de alto risco').
rotulo(mcc_sensivel, 'MCC sensível').
rotulo(ip_blacklist, 'IP em blacklist').
rotulo(cartao_blacklist, 'Cartão em blacklist').
rotulo(dispositivo_blacklist, 'Dispositivo em blacklist').
rotulo(horario_sensivel, 'Horário sensível (madrugada)').
rotulo(risco_chargeback_previo, 'Cliente com chargeback prévio').
rotulo(alta_velocidade_cliente, 'Muitas transações em curta janela').
rotulo(kyc_insuficiente_para_valor, 'KYC insuficiente para valor alto').
rotulo(geovelocidade_improvavel, 'Geovelocidade improvável (mudança de país muito rápida)').
rotulo(dispositivo_e_pais_habituais, 'Dispositivo e país habituais (reduz risco)').
rotulo(valor_dentro_perfil, 'Valor dentro do perfil médio').

% Gerar lista de motivos humanizados
motivo(ID, ListaHuman) :-
    sinais_ativos(ID, Sinais),
    findall(Msg, (member((Label, _), Sinais), rotulo(Label, Msg)), ListaHuman).


% EXTENSÃO: GRAPH LINK ANALYSIS
rotulo(fraude_em_rede, 'Cliente relacionado a fraudador confirmado (rede suspeita)').
rotulo(rede_altamente_suspeita, 'Cliente conectado a múltiplos fraudadores (rede altamente suspeita)').
