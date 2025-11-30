# Sistema Antifraude de Transações em Prolog

Sistema especialista desenvolvido em Prolog para análise e detecção de fraudes em transações financeiras, utilizando raciocínio lógico declarativo, análise de grafos e pontuação baseada em múltiplos sinais de risco.

---

## 📋 Índice

- [Sobre o Projeto](#sobre-o-projeto)
- [Funcionalidades](#funcionalidades)
- [Arquitetura](#arquitetura)
- [Como Executar](#como-executar)
- [Estrutura dos Arquivos](#estrutura-dos-arquivos)
- [Sinais de Risco](#sinais-de-risco)
- [Extensão Implementada](#extensão-implementada)
- [Exemplos de Uso](#exemplos-de-uso)
- [Consultas Disponíveis](#consultas-disponíveis)

---

## 🎯 Sobre o Projeto

Este sistema antifraude analisa transações financeiras em tempo real, atribuindo pontuações de risco baseadas em múltiplos indicadores e decidindo automaticamente entre **aprovar**, **revisar** ou **recusar** cada transação.

O sistema foi desenvolvido seguindo os princípios de:
- **Lógica declarativa**: Regras expressam o conhecimento do domínio
- **Backtracking**: Exploração automática de soluções
- **Explicabilidade**: Cada decisão é justificada com motivos claros
- **Análise de grafos**: Detecção de fraudes através de redes de relacionamento

---

## ✨ Funcionalidades

### 🔍 Detecção de Sinais de Risco

O sistema identifica **13 sinais diferentes**, incluindo:
- Valores acima do perfil do cliente
- Países de alto risco
- MCCs (categorias) sensíveis
- IPs, cartões e dispositivos em blacklist
- Horários suspeitos (madrugada)
- Geovelocidade improvável
- Redes de fraude (extensão)

### 📊 Sistema de Pontuação

- Sinais positivos aumentam o score de risco
- Sinais negativos reduzem o risco (comportamento habitual)
- Limiares configuráveis para decisão

### 🤖 Decisão Automatizada

- **Score < 30**: Aprovar
- **30 ≤ Score < 60**: Revisar manualmente
- **Score ≥ 60**: Recusar

### 💬 Explicabilidade Completa

- Justificativa detalhada para cada decisão
- Lista de todos os sinais detectados
- Motivos humanizados e compreensíveis

---

## 🏗️ Arquitetura

O sistema é modularizado em 6 arquivos principais:
```
projeto/
│
├── entrada.txt          # Base de conhecimento (fatos)
├── ontologia.pl         # Hierarquia de classes e herança
├── sinais.pl            # Detecção de sinais de risco
├── decisao.pl           # Cálculo de pontuação e decisão
├── explicacao.pl        # Geração de justificativas
├── principal.pl         # Orquestrador e interface
└── saida.txt            # Relatório gerado
```

---

## 🚀 Como Executar

### Pré-requisitos

- SWI-Prolog instalado ([download aqui](https://www.swi-prolog.org/download/stable))

### Passo a Passo

1. **Clone o repositório**
```bash
git clone <seu-repositorio>
cd <pasta-do-projeto>
```

2. **Inicie o SWI-Prolog**
```bash
swipl
```

3. **Carregue o sistema**
```prolog
?- consult('principal.pl').
```

4. **Execute a análise completa**
```prolog
?- executar.
```

O sistema irá:
- Analisar todas as transações de `entrada.txt`
- Gerar um relatório detalhado em `saida.txt`
- Exibir mensagem de confirmação no console

---

## 📁 Estrutura dos Arquivos

### `entrada.txt`
Contém toda a base de conhecimento:
- **Ontologia**: Classes e hierarquia de entidades
- **Entidades**: Clientes, comerciantes, dispositivos, IPs, países, cartões
- **Perfis**: Gasto médio, nível KYC, histórico de chargeback
- **Blacklists**: IPs, cartões e dispositivos suspeitos
- **Transações**: Dados completos das transações a serem analisadas
- **Configurações**: MCCs sensíveis, países de alto risco, limiares

Formato de transação:
```prolog
transacao(ID, Cliente, Comerciante, Valor, Moeda, Pais, MCC, 
          Timestamp, Dispositivo, IP, Cartao).
```

### `ontologia.pl`
Implementa herança transitiva e verificação de instâncias:
```prolog
herda_trans(filho, pai).
instancia_de(entidade, classe).
```

### `sinais.pl`
Contém **13 predicados de detecção** de sinais positivos e negativos:
- `sinal(ID, Label, Peso)` - Sinais que aumentam risco
- `sinal_neg(ID, Label, Peso)` - Sinais que reduzem risco

### `decisao.pl`
Lógica de agregação e decisão:
- `sinais_ativos(ID, Sinais)` - Lista todos os sinais detectados
- `pontuacao_transacao(ID, Score, Evidencias)` - Calcula score total
- `decisao(ID, Resultado)` - Decide aprovar/revisar/recusar

### `explicacao.pl`
Traduz sinais técnicos em mensagens humanizadas:
- `rotulo(sinal, mensagem)` - Mapeamento de rótulos
- `motivo(ID, ListaMotivos)` - Gera lista de justificativas

### `principal.pl`
Orquestra todo o sistema:
- Carrega todos os módulos
- `analisar_transacao(ID)` - Análise individual detalhada
- `estatisticas` - Resumo geral do sistema
- `executar` - Executa análise completa e gera relatório

---

## 🚨 Sinais de Risco

### Sinais Positivos (Aumentam Risco)

| Sinal | Peso | Descrição |
|-------|------|-----------|
| `valor_acima_perfil` | +25 | Valor ≥ 3x gasto médio |
| `pais_alto_risco` | +20 | Países como Rússia, China |
| `mcc_sensivel` | +10 | Eletrônicos, games, cripto |
| `ip_blacklist` | +30 | IP em lista negra |
| `cartao_blacklist` | +40 | Cartão em lista negra |
| `dispositivo_blacklist` | +30 | Dispositivo em lista negra |
| `horario_sensivel` | +5 | Transação na madrugada (0h-6h ou 23h+) |
| `risco_chargeback_previo` | +20 | Cliente com histórico de chargeback |
| `alta_velocidade_cliente` | +15 | 3+ transações em 30 minutos |
| `kyc_insuficiente_para_valor` | +15 | KYC < 2 para valor ≥ R$1000 |
| `geovelocidade_improvavel` | +25 | Mudança de país em < 2 horas |
| `fraude_em_rede` | +35 | Conectado a 1 fraudador confirmado |
| `rede_altamente_suspeita` | +50 | Conectado a 2+ fraudadores |

### Sinais Negativos (Reduzem Risco)

| Sinal | Peso | Descrição |
|-------|------|-----------|
| `dispositivo_e_pais_habituais` | -10 | Usa dispositivo e país conhecidos |
| `valor_dentro_perfil` | -5 | Valor entre 80% e 120% do perfil |

---

## ⭐ Extensão Implementada

### **Graph Link Analysis (Análise de Redes)**

Implementa detecção de fraude através de análise de grafos de relacionamento entre entidades.

#### Como Funciona

1. **Relacionamentos** são estabelecidos quando entidades compartilham:
   - Mesmo dispositivo
   - Mesmo endereço IP
   - Mesmo cartão

2. **Detecção de Redes Fraudulentas**:
   - `fraude_em_rede` (+35 pontos): Cliente conectado a 1 fraudador confirmado
   - `rede_altamente_suspeita` (+50 pontos): Cliente conectado a 2+ fraudadores

#### Predicados da Extensão
```prolog
% Verifica relacionamento entre entidades
relacionado(E1, E2).

% Detecta fraude em rede (1 conexão)
sinal(ID, fraude_em_rede, 35).

% Detecta rede altamente suspeita (2+ conexões)
sinal(ID, rede_altamente_suspeita, 50).
```

#### Exemplo Prático

Se `cli_ana` compartilha dispositivo `dev_a1` com `cli_diego`, e `cli_diego` é fraudador confirmado, então `cli_ana` recebe o sinal `fraude_em_rede` (+35 pontos) em todas as suas transações.

---

## 📝 Exemplos de Uso

### Análise Individual
```prolog
?- analisar_transacao(tx1001).
```

**Saída esperada:**
```
=== ANÁLISE TX1001 ===
Cliente: cli_ana
Comerciante: mer_eletron
Valor: R$ 2500.00
País: eua
MCC: eletronicos
Horário: 01:30
Dispositivo: dev_a1
IP: ip_x
Cartão: cartao_ana

SINAIS DETECTADOS:
  [+25] valor_acima_perfil (gasto médio: R$300, transação: R$2500)
  [+10] mcc_sensivel (categoria eletronicos)
  [+05] horario_sensivel (madrugada 01:30)
  [+35] fraude_em_rede

PONTUAÇÃO TOTAL: 75
DECISÃO: RECUSAR
MOTIVOS:
  - Valor muito acima do perfil do cliente
  - MCC sensível
  - Horário sensível (madrugada)
  - Cliente relacionado a fraudador confirmado (rede suspeita)
```

### Verificar Decisão
```prolog
?- decisao(tx3003, D).
D = revisar.
```

### Calcular Score
```prolog
?- pontuacao_transacao(tx2002, Score, Evidencias).
Score = 205,
Evidencias = [(valor_acima_perfil, 25), (pais_alto_risco, 20), ...].
```

### Verificar Relacionamentos (Extensão)
```prolog
?- relacionado(cli_ana, X), fraude_confirmada(X).
X = cli_diego.
```

---

## 🔍 Consultas Disponíveis

### Consultas Básicas
```prolog
% Listar todas as transações
?- transacao(ID, Cliente, _, _, _, _, _, _, _, _, _).

% Ver perfil de um cliente
?- gasto_medio(cli_ana, Media), kyc_nivel(cli_ana, KYC).

% Verificar blacklists
?- blacklist_ip(IP).
?- blacklist_cartao(Cartao).
?- blacklist_dispositivo(Dispositivo).
```

### Consultas de Análise
```prolog
% Sinais detectados em uma transação
?- sinais_ativos(tx1001, Sinais).

% Score de uma transação
?- pontuacao_transacao(tx1001, Score, _).

% Decisão de uma transação
?- decisao(tx1001, Decisao).

% Motivos de uma decisão
?- motivo(tx1001, Motivos).
```

### Consultas da Extensão (Graph Link)
```prolog
% Verificar se duas entidades estão relacionadas
?- relacionado(cli_ana, cli_diego).

% Encontrar todas as conexões de um cliente
?- relacionado(cli_ana, X).

% Verificar fraudes confirmadas
?- fraude_confirmada(Cliente).

% Contar conexões com fraudadores
?- setof(F, (relacionado(cli_carla, F), fraude_confirmada(F)), Fraudadores),
   length(Fraudadores, N).
```

### Consultas Estatísticas
```prolog
% Contar transações aprovadas
?- findall(1, (transacao(ID,_,_,_,_,_,_,_,_,_,_), decisao(ID, aprovar)), L),
   length(L, N).

% Média de score do sistema
?- findall(S, pontuacao_transacao(_, S, _), Scores),
   sum_list(Scores, Total),
   length(Scores, N),
   Media is Total / N.

% Sinal mais frequente
?- findall(Label, sinal(_, Label, _), Labels),
   sort(Labels, Unicos).
```

---

## 📊 Resultados Esperados

### Distribuição de Decisões (Base Padrão)

- **Aprovar**: 10% (1 transação)
- **Revisar**: 50% (5 transações)
- **Recusar**: 40% (4 transações)

### Transações de Alto Risco

- `tx2002`: Score 205 (múltiplas blacklists + geovelocidade)
- `tx8008`: Score 105 (país alto risco + IP blacklist + rede suspeita)
- `tx1001`: Score 75 (valor alto + horário + rede suspeita)

---

E adicione o rótulo em `explicacao.pl`:
```prolog
rotulo(novo_sinal, 'Descrição humanizada').
```

---

## 👥 Autores

Bruna Kinjo
Kayke Ahrens




---

**Que a lógica esteja com você! 🧠✨**
