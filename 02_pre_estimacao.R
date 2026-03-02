# 2 - Pŕe-estimação

# Descrição

# realizamos os testes de pré-estimação baseados em teste de raiz unitária, 
# cointegração e quebra estrutural

# Sumário

# 1 - Pacotes
# 2 - Dados - importação e preparação
# 3 - Estatistica descritiva
# 4 - teste de raiz unitária
# 5 - Teste de cointegração
# 6 - Teste de quebra estrutural


####

# 1 - pacotes

# instalação

# install.packages("plm")
# install.packages("tseries")
# install.packages("urca")
# install.packages("dplyr")
# install.packages("openxlsx")
# install.packages("stargazer")
# install.packages("writexl")
# install.packages("ggplot2")
# install.packages("purrr")
# install.packages("strucchange")

# Obs. instação dos pacotes se necessário

# carregar os pacotes

library(plm) # dados em painel
library(tseries)  # series temporais
library(urca) # teste de raiz unitária e cointegração
library(dplyr)  # manipulação de dados
library(openxlsx)  # exportação de dados para planilha
library(stargazer)  # tabelas
library(writexl)  # exportação
library(ggplot2)  # gráficos
library(purrr)   # cointegração
library(strucchange) # teste quebra estrutural


# Obs. a cada etapa carregamos novamente os pacotes necessários

# 2 - Dados - importação e preparação

# sumário:

# 2.1 - Carregar dados
# 2.2 - Limpeza preliminar
# 2.3 - Setar como dados em painel 'panel data'
# 2.4 - Preparação das séries
# 2.5 - Limpeza final
# 2.6 - Verificação

###

# 2.1 - Carregar os dados


# verificar o caminho e se o arquivo existe

setwd("/home/erodrigo/Área de trabalho/Academico/3.4 Working papers_publica/01_paper_2021_01 crise fiscal, receita e despesa_submeter nova-economia-UFMG_mar_2026/data_codigos/")
file.exists("data_vf.xlsx")


# Carregar

library(readxl)
data_vf <- read_excel("/home/erodrigo/Área de trabalho/Academico/3.4 Working papers_publica/01_paper_2021_01 crise fiscal, receita e despesa_submeter nova-economia-UFMG_mar_2026/data_codigos/data_vf.xlsx",
                      sheet = "compilado")
View(data_vf)

# Obs. em caso de erro especifique o caminho correto da base de dados 'data_vf'
# e planilha 'compilado'. Ou use a funcionalidade do R studio de importação
# automática

# Para simplificação renomeamos 'data_vf' como 'df'

df <- data_vf
View(df)

###

# 2.2 - Limpeza preliminar

# identificar a presença de NAs

table(df$uf, useNA = "ifany")
table(df$ano, useNA = "ifany")

# limpeza dos NAs

library(dplyr)

df <- df %>%
  filter(!is.na(uf), 
         !is.na(ano)
  )

head(df)

# Setar anos como números

df$ano <- as.numeric(as.character(df$ano))

# Os individuos representados pelas uf dos estados seja um texto/fator

df$uf <- as.factor(df$uf)

# Verificação visual

class(df$ano) # Deve retornar "numeric"
class(df$uf)  # Deve retornar "factor" ou "character"

# Verificar duplicatas

any(duplicated(df[, c("uf", "ano")]))

# Obs. a limpeza preliminar por ser feita antes da construção de novas variáveis 
# e transformação em log. No final da etapa realizamos outra limpeza e verificação

###

# 2.3 - Setar como dados em painel 'panel data'

library(plm)

df <- pdata.frame(df, index = c("uf", "ano"))

# onde, i = uf (unidade federativa) e t = ano. Se houver Nas dará erro, pois o 
# R não consegue identificar o i e t

# Verificar dimensão (N x T) e balanceamento

head(df)

summary(df)

pdim(df)

punbalancedness(df)  

# Obs. não é um problema por si caso o painel seja desbalanceado, mas exige
# cuidados na estimação e interpretação dos resultados

###

# 2.4 - Preparação das séries

# Fiscal

## Receitas

df$log_rec_total <- log(df$rec_total + 1) # receita total
df$log_rcl <- log(df$rcl + 1) # receita corrente líquida - RCL
df$log_rec_cap_cred <- log(df$rec_cap_cred + 1) #receita de operações de crédito
df$log_rcl_cred <- log(df$rcl + df$rec_cap_cred + 1) # capacidade financiamento total (rcl mais operações de crédito)

## Despesas
df$log_desp_total <- log(df$desp_total + 1) # despesa total
df$log_desp_pri <- log(df$desp_pri + 1) # depesa primária (despesa total menos juros e encargos menos amorizações) - simplificada
df$log_desp_cor <- log(df$desp_cor + 1) # despesa corrente
df$log_desp_cap_invest <- log(df$desp_cap_invest + 1) # investimentos
df$log_desp_cap_out <- log(df$desp_cap_out + 1) # outras despesas de capital excerto investimento

## Divida e encargos

df$log_dcl <- log(df$dcl + 1)  # divida consolidada liquida - DCL
df$log_desp_juros <- log(df$desp_juros + 1) # despesa com juros e encargos
df$log_desp_cap_amort <- log(df$desp_cap_amort + 1) # gastos com amortização da dívida
df$log_desp_juros_amort <- log(df$desp_juros + df$desp_cap_amort + 1) # soma juros e encargos e amortização

# Obs. DCL em log ao ignorar os valores negativos e zeros

# produto e atividade

df$log_pib # produto em log
df$log_pib_pot # produto potencial em log
df$log_pib_gap # hiato do produto em log

# controles

df$log_di_dcl <- log(df$di_dcl + 1) # relação DCL/RCL
df$log_tdt <- log(df$tdt + 1) # termos de trocas dos estados


# dummies 

## dummies fiscais

df$d_mcasp # mudança metodológica dados fiscais em 2015
df$d_rrf # estados em regime de recuperação fiscal
df$d_alerta # alerta se a relação DCL/RCL estiver acima de 1,8
df$d_rrf_alerta # soma as dummies d_rrf e d_alerta

## dummy eleitoral

df$d_ele_pre # ano anterior a eleição
df$d_ele # eleitoral
df$d_ele_pos # ano seguinte a eleição

# dummies - período

df$d_pe_adap_lrf # adaptação da LRF e ajuste (2000-2003)
df$d_pe_boom # boom e expansão (2004-2014)
df$d_pe_crise # crise e reestruturação
df$d_pe_covid # efeitos da pandemia (2020-2023)

# verificar

head(df)

###

# 2.5 - Limpeza dos dados 

# Eliminamos os NAs, zero e valores infinitos das variáveis de interesse fiscais, 
# produto e dos controles

library(dplyr)

library(plm)

# Vetor de variaveis de interesse

vars_int <- c("log_rcl", # fiscais - receita
              "log_rcl_cred", 

              # Despesas
              "log_desp_total",
              "log_desp_pri",
              "log_desp_cor",
              "log_desp_cap_invest",
              "log_desp_cap_out",
              
              # Divida e encargos
              "log_dcl",
              "log_desp_juros",
              "log_desp_cap_amort",
              "log_desp_juros_amort",              
        
              # produto e atividade
              "log_pib",
              "log_pib_pot",
              "log_pib_gap",
              
              # controles
              "log_di_dcl",
              "log_tdt"
               )
           
          
# visualizar primeiras linhas

head(df[, c("uf", "ano", vars_int)])


# limpeza

df_l <- df %>%
  filter(!is.na(uf), !is.na(ano)) %>% # eliminar NAs
  mutate(across(all_of(vars_int), ~as.numeric(as.character(.)))) %>% #dados numéricos
  filter(if_all(all_of(vars_int), is.finite)) %>% # eliminar infinitos
  
  group_by(uf) %>%
  filter(n() >= 5) %>% 
  ungroup() %>%
  
  # Ordenação 
  
  arrange(uf, ano)

# substituir

df <- df_l

# visualizar primeiras linhas

head(df[, c("uf", "ano", vars_int)])

# set em painel

df_p <- pdata.frame(df, index = c("uf", "ano"))

# Obs. o dataframe 'df_p' que será usado para estimação 

###

# 2.6 - Verificação 

df_p %>% 
  summarise(across(all_of(vars_int), 
                   list(na = ~sum(is.na(.)), 
                        inf = ~sum(!is.finite(.))), 
                   .names = "{.col}_{.fn}")) %>%
  glimpse()

# visualizar

head(df_p)

###

# Exportar os dados do df_p

library(writexl)

write_xlsx(df_p, path = "df_p.xlsx") # planilha xlxs

saveRDS(df_p, file = "df_p.rds") # formato R

# Obs. dados da planilha df_p foram tratados, limpos e validados

###

# 3 - Estatistica descritiva

# Descrição:

# Apresentar as estatisticas descritivas dos daods fiscias, produto e controles

# Sumário

# 3.1 - Carregar dados
# 3.2 - Selecionar variáveis
# 3.3 - Estatistica descritiva
# 3.4 - Exportação da tabela

###

# 3.1 - Carregar dados

head(df_p)

# obs. 'df_p' é a mesma base tratada, limpa e validada na etapa 2

# 3.2 - Selecionar variáveis


# dados em nível - criar novo data frame

df_p_est <- as.data.frame(df_p[, c("uf", "ano", vars_int)])
                           

# visualizar primeiras linhas

head(df_p_est[, c("uf", "ano", vars_int)])


# 3.3 - Estatistica descritiva

library(dplyr)
library(tidyr)

# em log

tabela_descritiva <- df_p_est %>%
  select(all_of(unique(vars_int))) %>%
  pivot_longer(everything(), names_to = "Variável", values_to = "Valor") %>%
  group_by(Variável) %>%
  summarise(
    N = sum(!is.na(Valor)),
    Média = mean(Valor, na.rm = TRUE),
    Mediana = median(Valor, na.rm = TRUE),
    `Desvio Padrão` = sd(Valor, na.rm = TRUE),
    Variância = var(Valor, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

# Exibe o resultado final
print(tabela_descritiva)


# 3.4 - Exportação da tabela

write.csv(tabela_descritiva, 
          "tabela_descritiva_fiscais.csv", 
          row.names = FALSE
          )

write_xlsx(tabela_descritiva, 
           "tabela_descritiva_fiscais.xlsx"
           )

# 3.5 - Tabela descritiva no stargazer


library(stargazer)

tabela_final <- as.data.frame(tabela_descritiva)

# Gera a tabela em formato de texto para conferência imediata

stargazer(tabela_final, 
          summary = FALSE, 
          type = "text", 
          title = "Estatísticas Descritivas: Variáveis Fiscais e de Atividade",
          digits = 4,
          rownames = FALSE)

# Exporta diretamente para código LaTeX
stargazer(tabela_final, 
          summary = FALSE, 
          type = "latex", 
          title = "Estatísticas Descritivas (2000-2023)",
          digits = 4,
          rownames = FALSE,
          out = "tabela_descritiva.tex"

#####

# 4 - teste de raiz unitária

# Descrição:

# a) realizamos os testes preliminares de raiz unitária pra verificar a estacionariedade
# das séries utilizadas no system GMM e BVAR
# b) As séries devem ser estácionária para permitir a estimação
# c) as séries foram testadas em log. Excerto a relação dcl/rcl que assume valores negativos
# e testada e nível

# Sumário

# 4.1 - Deinição dos testes
# 4.2 - Carregar dados
# 4.3 - Augmented Dickey-Fuller (ADF) - primeira geração
# 4.4 - Teste Im, Pesaran & Shin (IPS) - primeira geração
# 4.5 - Pesaran (CIPS) - segunda geração
# 4.6 - Integração e exportação dos testes

###

# 4.1 - Definição dos testes

# Testes:

# Primeira Geração (Assumem Independência Transversal)

# Augmented Dickey-Fuller (ADF) -muito robusto se você selecionar o número correto 
# de defasagens (via critério de AIC ou BIC);

# - Im, Pesaran & Shin (IPS) - permite que o coeficiente autorregressivo varie 
# entre os estados (heterogêneo);

# Segunda Geração (Consideram Dependência Transversal)

# O teste de  Pesaran (CIPS) é a evolução do teste IPS; 
# É a evolução do teste IPS. Ele lida com a correlação entre os 
# estados ao incluir médias transversais das variáveis no modelo de teste;
# É altamente recomendado quando existem choques simultâneos sobre os agentes(segunda geração)

# Obs. os testes são realizados considerando: intercepto (constant); e tendência
# (trend). Devido as séries fiscais e de produto apresentarem média diferente de zero 
# e crescer ao longo do tempo

# 4.2 Carregar dados

# data frame

head(df_p)

# variáveis de interesse

vars_int

# obs. o 'df_p' e 'vars_int' foram definidas na etapa 2

###

# 4.3 - Augmented Dickey-Fuller (ADF) - primeira geração

# Especificação:

# O teste em log, lag = 1, tendência e intercepto
# O argumento 'trend' inclui o intercepto + tendência em nível
# Em primeira diferença mantemos a tendência
# Fazemos o teste em bloco de variáveis "vars_int"

# 1. Limpar o vetor de variáveis antes dos loops
vars_unicas <- unique(vars_int)

# 2. Inicializar os data frames
adf_res <- data.frame()
adf_res_diff <- data.frame()

# 3. Loop em Nível (utilizando vars_unicas)
for (v in vars_unicas) {
  teste_res <- tryCatch({
    res <- purtest(df_p[[v]], data = df_p, test = "madwu", exo = "trend", lags = 1)
    data.frame(
      Variável = v,
      Estat_Nivel = round(res$statistic$statistic, 3),
      P_Valor_Nivel = round(res$statistic$p.value, 4)
    )
  }, error = function(e) {
    data.frame(Variável = v, Estat_Nivel = NA, P_Valor_Nivel = NA)
  })
  adf_res <- rbind(adf_res, teste_res)
}

# 4. Loop em Diferença (utilizando vars_unicas)
for (v in vars_unicas) {
  teste_res <- tryCatch({
    res <- purtest(diff(df_p[[v]]), data = df_p, test = "madwu", exo = "intercept", lags = 1)
    data.frame(
      Variável = v,
      Estat_Diff = round(res$statistic$statistic, 3),
      P_Valor_Diff = round(res$statistic$p.value, 4)
    )
  }, error = function(e) {
    data.frame(Variável = v, Estat_Diff = NA, P_Valor_Diff = NA)
  })
  adf_res_diff <- rbind(adf_res_diff, teste_res)
}

# 5. União Final (Sem duplicatas)
adf_test <- left_join(adf_res, adf_res_diff, by = "Variável")
print(adf_test)

###

# 4.4 - Teste Im, Pesaran & Shin (IPS) - primeira geração

# Especificação:

# O teste em log, lag = 1, tendência e intercepto
# O argumento 'trend' inclui o intercepto + tendência em nível
# Em primeira diferença mantemos a tendência
# Fazemos o teste em bloco de variáveis "vars_int"

# 1 - Preparação do Vetor Único e Dataframes
vars_unicas <- unique(vars_int)
ips_res <- data.frame()
ips_res_diff <- data.frame()

# 2 - Loop para Teste em Nível (Trend + Intercept)
for (v in vars_unicas) {
  teste_res <- tryCatch({
    res <- purtest(df_p[[v]], 
                   data = df_p, 
                   test = "ips", 
                   exo = "trend", 
                   lags = 1)
    
    data.frame(
      Variável = v,
      Estat_IPS_Nivel = round(res$statistic$statistic, 3),
      P_Valor_IPS_Nivel = round(res$statistic$p.value, 4)
    )
    
  }, error = function(e) {
    data.frame(Variável = v, Estat_IPS_Nivel = NA, P_Valor_IPS_Nivel = NA)
  })
  ips_res <- rbind(ips_res, teste_res)
}

# 3 - Loop para Teste em Primeira Diferença (Intercept)
for (v in vars_unicas) {
  teste_res <- tryCatch({
    res <- purtest(diff(df_p[[v]]), 
                   data = df_p, 
                   test = "ips", 
                   exo = "intercept", 
                   lags = 1)
    
    data.frame(
      Variável = v,
      Estat_IPS_Diff = round(res$statistic$statistic, 3),
      P_Valor_IPS_Diff = round(res$statistic$p.value, 4)
    )
    
  }, error = function(e) {
    data.frame(Variável = v, Estat_IPS_Diff = NA, P_Valor_IPS_Diff = NA)
  })
  ips_res_diff <- rbind(ips_res_diff, teste_res)
}

# 4 - União Consolidada (Sem duplicatas)
library(dplyr)
ips_test <- left_join(ips_res, ips_res_diff, by = "Variável")

# Resultado Final
print(ips_test)

# 4.5 - Pesaran (CIPS) - segunda geração

# Especificação:

# O teste em log, lag = 1, tendência e intercepto
# O argumento 'trend' inclui o intercepto + tendência em nível
# Em primeira diferença mantemos a tendência
# Fazemos o teste em bloco de variáveis "vars_int"

# 1 - Preparação e Unicidade
vars_unicas <- unique(vars_int)
cips_res <- data.frame()
cips_res_diff <- data.frame()

# 2 - Loop CIPS em Nível (Trend)
for (v in vars_unicas) {
  teste_res <- tryCatch({
    # No pacote plm, o cipstest implementa Pesaran (2007)
    res <- cipstest(df_p[[v]], 
                    type = "trend", # Intercepto + Tendência
                    lags = 1)
    
    data.frame(
      Variável = v,
      Estat_CIPS_Niv = round(as.numeric(res$statistic), 3),
      P_Valor_CIPS_Niv = round(as.numeric(res$p.value), 4)
    )
    
  }, error = function(e) {
    data.frame(Variável = v, Estat_CIPS_Niv = NA, P_Valor_CIPS_Niv = NA)
  })
  
  cips_res <- rbind(cips_res, teste_res)
}

# 3 - Loop CIPS em Primeira Diferença (Drift)
for (v in vars_unicas) {
  teste_res <- tryCatch({
    # Aplica o teste na primeira diferença
    res <- cipstest(diff(df_p[[v]]), 
                    type = "drift", # Apenas Intercepto na diferença
                    lags = 1)
    
    data.frame(
      Variável = v,
      Estat_CIPS_Diff = round(as.numeric(res$statistic), 3),
      P_Valor_CIPS_Diff = round(as.numeric(res$p.value), 4)
    )
    
  }, error = function(e) {
    data.frame(Variável = v, Estat_CIPS_Diff = NA, P_Valor_IPS_Diff = NA)
  })
  
  cips_res_diff <- rbind(cips_res_diff, teste_res)
}

# 4 - União Consolidada
library(dplyr)
cips_test <- left_join(cips_res, cips_res_diff, by = "Variável")

# Resultado Final
print(cips_test)

# 4.6 - Integração e exportação dos testes

library(dplyr)
library(openxlsx)

# 1. Preparação dos resultados do teste ADF (1ª Geração - Raiz Unitária Comum)
tab_final_adf <- adf_test %>%
  rename(ADF_Niv_P = P_Valor_Nivel,
         ADF_Diff_P = P_Valor_Diff) %>%
  select(Variável, ADF_Niv_P, ADF_Diff_P)

# 2. Preparação dos resultados do teste IPS (1ª Geração - Heterogeneidade)
tab_final_ips <- ips_test %>%
  rename(IPS_Niv_P = P_Valor_IPS_Nivel,
         IPS_Diff_P = P_Valor_IPS_Diff) %>%
  select(Variável, IPS_Niv_P, IPS_Diff_P)

# 3. Preparação dos resultados do teste CIPS (2ª Geração - Dependência Transversal)
tab_final_cips <- cips_test %>%
  rename(CIPS_Niv_P = P_Valor_CIPS_Niv,
         CIPS_Diff_P = P_Valor_CIPS_Diff) %>%
  select(Variável, CIPS_Niv_P, CIPS_Diff_P)

# 4. Combinação das três tabelas via Join
tab_res <- tab_final_adf %>%
  left_join(tab_final_ips, by = "Variável") %>%
  left_join(tab_final_cips, by = "Variável")

# 5. Criar diagnóstico de ordem de integração (Consenso Robusto)
# O critério abaixo prioriza o CIPS por ser mais robusto a choques comuns aos estados.
tab_res <- tab_res %>%
  mutate(Diagnóstico = case_when(
    # Estacionária se CIPS e pelo menos um de 1ª Geração rejeitarem H0 em nível
    CIPS_Niv_P <= 0.05 & (ADF_Niv_P <= 0.05 | IPS_Niv_P <= 0.05) ~ "I(0)",
    
    # I(1) se todos concordarem que a primeira diferença é estacionária
    CIPS_Diff_P <= 0.05 & (ADF_Diff_P <= 0.05 | IPS_Diff_P <= 0.05) ~ "I(1)",
    
    # Caso especial: Estacionariedade Marginal (comum em dados fiscais)
    CIPS_Niv_P <= 0.10 & CIPS_Diff_P <= 0.05 ~ "I(1) / Tendência-Estacionária",
    
    TRUE ~ "Verificar (Divergência)"
  ))

# 6. Organização Final para o Paper (Limpeza de nomes)
tab_res$Variável <- gsub("log_", "", tab_res$Variável)

# Visualizar e Exportar
print(tab_res)

write.xlsx(tab_res, file = "raiz_unit_consolidado.xlsx", 
           sheetName = "test_raiz_unit", overwrite = TRUE)

####

# 5 - Teste de quebra estrutural

# Descrição:

# Verificamos se as variáveis são cointegradas. Seguimos o teste de 
# Engle-Granger e Westerlund (2007)

# Obs.

# A cointegração sugere que, embora a Receita e a Despesa "vaguem" aleatoriamente 
# sozinhas, a distância entre elas permanece estável ao longo do tempo. No 
# contexto da solvência estadual, a cointegração entre arrecadação e gasto é 
# a condição necessária para a sustentabilidade fiscal; sua ausência sinaliza 
# a "armadilha da insolvência"

# Relações testadas

# a) Em relação ao produto estadual

# log_pib x log_rcl_cred
# log_pib x log_desp_pri
# log_pib x log_desp_cor
# log_pib x log_desp_cap_invest

# b) Em relação à RCL

# log_rcl_cred x log_desp_pri
# log_rcl_cred x log_desp_cor,
# log_rcl_cred x log_desp_cap_invest

# c) Em relação a DCL/RCL

# log_di_rcl x log_desp_pri
# log_di_rcl x log_desp_cor
# log_di_rcl x log_desp_cap_invest

###


# Sumário

# 5.1 - Testes de cointegração
# 5.2 - Teste de Engle-Granger em Painel
# 5.3 - Teste de Westerlund
# 5.4 - resultados

###

# 5.1 - Testes de cointegração

# a) Teste de Engle-Granger em Painel

# O procedimento de Engle-Granger baseia-se em dois estágios:
  
# 1) Estima-se a relação de longo prazo por Mínimos Quadrados Ordinários (OLS) 
# em nível.

# 2) Aplica-se um teste de raiz unitária (como o ADF) sobre os resíduos dessa 
# regressão.

# Significado: Se os resíduos forem estacionários, as variáveis são cointegradas.
# Isso implica que o erro (desvio do equilíbrio) tende a desaparecer com o tempo,
# trazendo a despesa e a receita de volta para a trajetória comum.

# Limitações:

# O teste de Engle-Granger pressupõe que os individuos do painel são independentes
# Ao mesmo tempo em que assume que a velocidade de ajuste é a mesma. Portanto,
# pode indicar uma falsa cointegração

# b) teste de Westerlund (2007)

# O teste de cointegração de Westerlund (2007) se baseaia na na dinâmica de
# correção de erros.

# Se α_i < 0, existe um mecanismo que puxa a variável de volta para o equilíbrio
# de longo prazo após um choque. Se α_i = 0, não há cointegração,

# Limitações:

# Considera dependência transversal, heterogeneidade e a dinâmica das variáveis

# 5.2 - Teste de Engle-Granger em Painel


# ==============================================================================
# AUTOMAÇÃO DO TESTE DE ENGLE-GRANGER EM PAINEL (MADDALA-WU)
# ==============================================================================

library(plm)
library(dplyr)
library(purrr)

# 1. Definição das relações (Y ~ X) conforme o plano de pesquisa
relacoes <- list(

  # a) Em relação ao produto estadual
  c("log_pib", "log_rcl_cred"),
  c("log_pib", "log_desp_pri"),
  c("log_pib", "log_desp_cor"),
  c("log_pib", "log_desp_cap_invest"),
 

  # b) Em relação à RCL
  c("log_rcl_cred", "log_desp_pri"),
  c("log_rcl_cred", "log_desp_cor"),
  c("log_rcl_cred", "log_desp_cap_invest"),

  # c) Em relação ao DCL/RCL
  c("log_di_rcl", "log_desp_pri"),
  c("log_di_rcl", "log_desp_cor"),
  c("log_di_rcl", "log_desp_cap_invest"),
  c("log_di_rcl", "log_pib")

)

# 2. Função para processar cada par e extrair o P-Valor
executar_cointegracao <- function(par) {
  tryCatch({
    y_var <- par[1]
    x_var <- par[2]
    
    # Primeiro Estágio: Regressão de Longo Prazo (Efeitos Fixos)
    formula_estatica <- as.formula(paste(y_var, "~", x_var))
    mod <- plm(formula_estatica, data = df_p, model = "within")
    
    # Extração e Limpeza dos Resíduos
    res <- data.frame(
      resid = as.numeric(residuals(mod)),
      uf    = as.character(attr(residuals(mod), "index")[[1]]),
      ano   = as.numeric(as.character(attr(residuals(mod), "index")[[2]]))
    ) %>%
      filter(is.finite(resid)) %>%
      group_by(uf) %>%
      filter(n() > 7, var(resid) > 0) %>%
      ungroup()
    
    # Segundo Estágio: Teste Maddala-Wu nos Resíduos
    res_p <- pdata.frame(res, index = c("uf", "ano"))
    teste <- purtest(res_p$resid, test = "madwu", exo = "none", lags = 1)
    
    # Retorno dos resultados estruturados
    data.frame(
      Relacao = paste(y_var, "x", x_var),
      Estatistica_Chi2 = round(teste$statistic$statistic, 3),
      DF = teste$statistic$parameter,
      P_Valor = round(teste$statistic$p.value, 5)
    )
  }, error = function(e) {
    data.frame(Relacao = paste(par[1], "x", par[2]), Estatistica_Chi2 = NA, DF = NA, P_Valor = NA)
  })
}

# 3. Execução do loop e consolidação da tabela
tab_cointegracao <- map_df(relacoes, executar_cointegracao)

# 4. Adicionar diagnóstico de significância
tab_cointegracao <- tab_cointegracao %>%
  mutate(Diagnostico = case_when(
    P_Valor <= 0.01 ~ "Cointegrado (1%)",
    P_Valor <= 0.05 ~ "Cointegrado (5%)",
    P_Valor <= 0.10 ~ "Cointegrado (10%)",
    TRUE ~ "Não Cointegrado (Insolvência)"
  ))

# Visualizar Tabela Final
print(tab_cointegracao)


# 5.3 - Teste de Westerlund

# ==============================================================================
# TESTE DE COINTEGRAÇÃO BASEADO EM ECM (LÓGICA WESTERLUND, 2007)
# ==============================================================================

library(plm)
library(dplyr)
library(purrr)
library(lmtest)

# 1. Definição das relações (Mesma lista anterior)
relacoes <- list(

  # a) Em relação ao produto estadual
  c("log_pib_gap", "log_rcl_cred"),
  c("log_pib", "log_desp_pri"),
  c("log_pib", "log_desp_cor"),
  c("log_pib", "log_desp_cap_invest"),
 

  # b) Em relação à RCL
  c("log_rcl_cred", "log_desp_pri"),
  c("log_rcl_cred", "log_desp_cor"),
  c("log_rcl_cred", "log_desp_cap_invest"),

  # c) Em relação ao DCL/RCL
  c("log_di_rcl", "log_desp_pri"),
  c("log_di_rcl", "log_desp_cor"),
  c("log_di_rcl", "log_desp_cap_invest"),
  c("log_di_rcl", "log_pib")

)

# 2. Função para estimar o ECM e extrair a Estatística de Westerlund
executar_westerlund <- function(par) {
  tryCatch({
    y_name <- par[1]
    x_name <- par[2]
    
    # Preparar dados: criar defasagens e diferenças manualmente para o ECM
    # Equação: D.Y = alpha * Y_lag + beta * X_lag + gamma * D.X + u
    
    dados_reg <- df_p %>%
      select(uf, ano, all_of(y_name), all_of(x_name)) %>%
      group_by(uf) %>%
      mutate(
        Y = get(y_name),
        X = get(x_name),
        D_Y = Y - lag(Y),             # Diferença da dependente
        D_X = X - lag(X),             # Diferença da independente
        Y_lag = lag(Y),               # Nível defasado (Termo de Erro)
        X_lag = lag(X)                # Nível defasado
      ) %>%
      ungroup() %>%
      filter(is.finite(D_Y), is.finite(Y_lag), is.finite(D_X)) # Limpeza
    
    # Estimação do Modelo de Correção de Erros (Pooled Mean Group proxy)
    # Testamos se o coeficiente de 'Y_lag' é significativo e negativo.
    # Usamos "within" (Fixed Effects) para controlar heterogeneidade de nível.
    
    modelo_ecm <- plm(D_Y ~ Y_lag + X_lag + D_X, 
                      data = dados_reg, 
                      index = c("uf", "ano"), 
                      model = "within") # Efeitos Fixos
    
    # Extração dos Resultados
    coefs <- coeftest(modelo_ecm, vcov = vcovHC(modelo_ecm, method = "arellano")) # Robustez
    
    alpha_coef <- coefs["Y_lag", "Estimate"]
    t_stat     <- coefs["Y_lag", "t value"]
    p_val      <- coefs["Y_lag", "Pr(>|t|)"]
    
    # O teste é unilateral à esquerda (alpha deve ser negativo)
    # Ajustamos o p-valor para unilateral se t_stat < 0
    p_val_adj <- ifelse(t_stat < 0, p_val / 2, 1 - p_val / 2)
    
    data.frame(
      Relacao = paste(y_name, "x", x_name),
      Coef_Ajuste = round(alpha_coef, 4),
      Estatistica_T = round(t_stat, 3),
      P_Valor = round(p_val_adj, 5)
    )
    
  }, error = function(e) {
    data.frame(Relacao = paste(par[1], "x", par[2]), Coef_Ajuste = NA, Estatistica_T = NA, P_Valor = NA)
  })
}

# 3. Execução do loop
tab_westerlund <- map_df(relacoes, executar_westerlund)

# 4. Diagnóstico (Interpretação da Velocidade de Ajuste)
tab_westerlund <- tab_westerlund %>%
  mutate(Diagnostico = case_when(
    P_Valor <= 0.05 & Coef_Ajuste < 0 ~ "Cointegrado (Ajuste Rápido)",
    P_Valor <= 0.10 & Coef_Ajuste < 0 ~ "Cointegrado (Marginal)",
    TRUE ~ "Não Cointegrado (Insolvência)"
  ),
  Velocidade_Ajuste = paste0(round(abs(Coef_Ajuste)*100, 1), "% ao ano")
  )

# Visualizar
print(tab_westerlund)

###

# 5.4 - resultados


library(dplyr)
library(openxlsx)

# 1. Padronização dos nomes para o Join
# Vamos assumir que 'tab_cointegracao' (EG) e 'tab_westerlund' (ECM) já existem no ambiente

tabela_eg <- tab_cointegracao %>%
  select(Relacao, EG_Chi2 = Estatistica_Chi2, EG_Pval = P_Valor)

tabela_west <- tab_westerlund %>%
  select(Relacao, West_Alpha = Coef_Ajuste, West_Pval = P_Valor, Velocidade_Ajuste)

# 2. União das Tabelas (Join)
tab_final_coint <- left_join(tabela_eg, tabela_west, by = "Relacao")

# 3. Criação do Diagnóstico Sintético (A "Prova dos Nove")
tab_final_coint <- tab_final_coint %>%
  mutate(
    Diagnostico_Final = case_when(
      # Caso 1: Ruptura Total (Insolvência)
      is.na(EG_Pval) | is.na(West_Pval) ~ "Ruptura Estrutural (Insolvência)",
      
      # Caso 2: Cointegração Robusta (Ambos confirmam)
      EG_Pval <= 0.05 & West_Pval <= 0.05 ~ "Sustentável (Robusto)",
      
      # Caso 3: Cointegração Fraca (Apenas um confirma ou ajuste muito lento)
      EG_Pval <= 0.05 & West_Pval > 0.05 ~ "Sustentável (Ajuste Lento/Não Linear)",
      
      TRUE ~ "Inconclusivo"
    )
  )

# 4. Formatação para Exportação
# Arredondamentos para ficar limpo no Paper
tab_final_coint <- tab_final_coint %>%
  mutate(
    EG_Chi2 = round(EG_Chi2, 1),
    West_Alpha = round(West_Alpha, 3)
  )

# Exibir e Exportar
print(tab_final_coint)

write.xlsx(tab_final_coint, 
           "resultados_cointegracao_consolidado.xlsx", 
           overwrite = TRUE)

# 6 - Teste de quebra estrutural

# teste de Bai & Perron (1998, 2003)

# Obs. o teste de Bai & Perron indica endogenamente onde estão as quebras estru-
# turais. Isto é, os dados dizem onde ocorreu a quebra. Usamos:

# a) Agregação (Cross-Sectional Mean): Calculamos a média anual de todos os estados
# para cada variável.

# b) Teste na Média: Aplicamos o Bai & Perron na série agregada para identificar 
# os choques sistêmicos (ex: Crise de 2015, Pandemia 2020) que afetaram o federalismo 
# como um todo.


library(strucchange)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo) # Necessário para tratamento de NAs

# ==============================================================================
# 1. LIMPEZA E CORREÇÃO DE TIPOS (A CORREÇÃO DO ERRO ESTÁ AQUI)
# ==============================================================================

# Transformamos o pdata.frame em um data.frame comum para remover atributos de painel
df_limpo <- as.data.frame(df_p)

# Forçamos a conversão da coluna ANO para numérico
# O as.character previne que o fator seja convertido para seu índice interno (1, 2, 3...)
if("ano" %in% names(df_limpo)) {
  df_limpo$ano <- as.numeric(as.character(df_limpo$ano))
} else {
  # Caso o ano esteja no índice e não na coluna
  df_limpo$ano <- as.numeric(as.character(index(df_p)[[2]]))
}

# Tratamento para variáveis de saldo (Hiato)
# Removemos o log se houver negativos, ou tratamos Infinitos
vars_existentes <- vars_int[vars_int %in% names(df_limpo)]

# Agregação (Média do Brasil)
dados_agregados <- df_limpo %>%
  select(ano, all_of(vars_existentes)) %>%
  group_by(ano) %>%
  summarise(across(everything(), function(x) {
    # Transforma Infinitos (-Inf) em NA para não quebrar a média
    x[is.infinite(x)] <- NA 
    mean(x, na.rm = TRUE)
  })) %>%
  arrange(ano)

# ==============================================================================
# 2. FUNÇÃO BAI & PERRON (COM PROTEÇÃO CONTRA SÉRIES CURTAS)
# ==============================================================================

rodar_bp_final <- function(var_nome, dados) {
  
  # Extrai a coluna e o ano
  y_val <- dados[[var_nome]]
  x_ano <- dados$ano
  
  # Validação: Se a coluna for só NA (comum em logs de negativos)
  if (all(is.na(y_val))) {
    return(data.frame(Variavel = var_nome, Status = "Erro: Variável contém apenas NAs/Infs"))
  }
  
  tryCatch({
    # Interpolação linear para preencher buracos (strucchange não aceita NA no meio)
    y_val_filled <- na.approx(y_val, na.rm = FALSE)
    
    # Remove NAs das pontas (início ou fim da série)
    idx_validos <- !is.na(y_val_filled)
    y_final <- y_val_filled[idx_validos]
    ano_final <- x_ano[idx_validos]
    
    # Verifica tamanho mínimo (Bai-Perron precisa de ~10 obs para rodar com segurança)
    if (length(y_final) < 10) {
      return(data.frame(Variavel = var_nome, Status = "Série muito curta para teste"))
    }
    
    # Criação da Série Temporal com o ano inicial CORRETO (agora numérico)
    ts_data <- ts(y_final, start = min(ano_final), frequency = 1)
    
    # Teste de Quebra (h=0.15 permite quebras nas pontas)
    bp <- breakpoints(ts_data ~ 1, h = 0.2) 
    
    if(any(is.na(bp$breakpoints))) {
      return(data.frame(Variavel = var_nome, Status = "Sem Quebra Estrutural"))
    } else {
      # Extrai as datas
      indices <- bp$breakpoints
      anos_quebra <- time(ts_data)[indices]
      anos_texto <- paste(floor(as.numeric(anos_quebra)), collapse = ", ")
      
      return(data.frame(Variavel = var_nome, Status = "Com Quebra", Anos = anos_texto))
    }
    
  }, error = function(e) {
    return(data.frame(Variavel = var_nome, Status = paste("Erro:", e$message)))
  })
}

# ==============================================================================
# 3. EXECUÇÃO
# ==============================================================================

tabela_quebras <- map_df(vars_existentes, rodar_bp_final, dados = dados_agregados)

print(tabela_quebras)

# exportação

library(openxlsx)
library(dplyr)

# 1. Criar uma versão "limpa" para exportação
tabela_exportacao <- tabela_quebras %>%
  mutate(
    # Remove o prefixo "log_" para ficar esteticamente melhor
    Variavel = gsub("log_", "", Variavel),
    # Traduz o Status para ficar pronto para o artigo
    Status = ifelse(Status == "Com Quebra", "Quebra Identificada", Status)
  )

# 2. Exportar
write.xlsx(tabela_exportacao, 
           file = "resultados_quebra_estrutural_limpo.xlsx", 
           overwrite = TRUE)

###############

# Gráfico 

# 1 - O "Sudden Stop" do Crédito (A Prova da Armadilha)

library(ggplot2)

# 1. Definir os anos de quebra para esta variável
quebras_credito <- c(2005, 2009, 2015)

# 2. Plotar
g1 <- ggplot(dados_agregados, aes(x = ano, y = log_rec_cap_cred)) +
  geom_line(color = "#2E86C1", size = 1.2) + # Linha azul forte
  geom_point(size = 2) +
  # Adicionar linhas verticais pontilhadas nos anos de quebra
  geom_vline(xintercept = quebras_credito, linetype = "dashed", color = "red") +
  # Adicionar textos explicativos
  annotate("text", x = 2015.5, y = max(dados_agregados$log_rec_cap_cred), 
           label = "Crise Fiscal (2015)", hjust = 0, color = "red", fontface = "bold") +
  theme_minimal() +
  labs(title = "Figura 1: Quebras Estruturais nas Operações de Crédito",
       subtitle = "Evidência de 'Sudden Stop' no financiamento subnacional",
       y = "Log (Operações de Crédito)", x = "Ano") +
  theme(plot.title = element_text(face = "bold"))

print(g1)

# 2 - A Rigidez da Despesa vs. Volatilidade da Receita

# 1. Preparar dados longos para plotar duas séries
dados_longos <- dados_agregados %>%
  select(ano, log_rec_pri, log_desp_pri) %>%
  pivot_longer(cols = -ano, names_to = "Agregado", values_to = "Valor")

# 2. Plotar
g2 <- ggplot(dados_longos, aes(x = ano, y = Valor, color = Agregado)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("log_desp_pri" = "#C0392B", "log_rec_pri" = "#27AE60"),
                     labels = c("Despesa Primária", "Receita Primária")) +
  # Quebra de 2019 (Sistêmica)
  geom_vline(xintercept = 2019, linetype = "dashed", color = "black") +
  annotate("text", x = 2019, y = min(dados_longos$Valor), 
           label = "Ruptura 2019", vjust = -1, angle = 90) +
  theme_minimal() +
  labs(title = "Figura 2: Trajetórias de Receita e Despesa Primária",
       subtitle = "Sincronia de quebra estrutural no pré-pandemia (2019)",
       y = "Log (Valores Reais)", x = "Ano", color = NULL) +
  theme(legend.position = "bottom")

print(g2)

# O Ciclo de Investimento (PAC e Queda)

g3 <- ggplot(dados_agregados, aes(x = ano, y = log_desp_cap_invest)) +
  geom_rect(aes(xmin = 2007, xmax = 2014, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.05) + # Destacar o período do "Boom"
  geom_line(color = "darkorange", size = 1.2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black") +
  annotate("text", x = 2007.5, y = min(dados_agregados$log_desp_cap_invest), 
           label = "Início PAC (2007)", hjust = 0) +
  theme_minimal() +
  labs(title = "Figura 3: Ciclo de Investimentos Públicos",
       subtitle = "Mudança de regime impulsionada pelo PAC",
       y = "Log (Investimentos)", x = "Ano")

print(g3)

###

# FIM






























