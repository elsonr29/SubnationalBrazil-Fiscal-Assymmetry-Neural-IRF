## 1.a - Filtro HP para dados em painel

# Preparação dos dados 

# Descrição:
# a) aplicação do filtro HP para o produto estadual (2000-2023)
# b) considera o produto em termos reais, deflacionados pelo IPCA e base 2023 (2023 = 100)
# c) Para cada estado é estimado o produto potencial e o hiato do produto em nível e log

# Obs. O R é muito eficiente para lidar com painéis. O filtro hp usa o parâmetro $\lambda = 6.25$ 
# para dados anuais, conforme a literatura padrão (Ravn & Uhlig, 2002).


# 1) Instalação dos pacotes

install.packages(c("mFilter", "dplyr", "writexl", "ggplot2", "readxl"))

# Pacotes 

# mFilter - aplicação do filtro Hodrick-Prescott (HP)
# dplyer - manipulação de dados
# writexl - exportação de dados para formato de planilha
# ggplot2 - gráficos

#carregar pacotes

library(mFilter)
library(dplyr)
library(writexl)
library(ggplot2)
library(readxl)

# 2) Dados

library(here)
library(readxl)

# verificar o caminho e se o arquivo existe

setwd("/home/erodrigo/Área de trabalho/Academico/3.4 Working papers_publica/01_paper_2021_01 crise fiscal, receita e despesa_submeter nova-economia-UFMG_mar_2026/data_codigos/01_trata_data/")
file.exists("r_filtro_hp_panel_data_estado.xlsx")

r_filtro_hp_panel_data_estado <- read_excel("r_filtro_hp_panel_data_estado.xlsx", 
                                            sheet = "pib_est")
                                            

View(r_filtro_hp_panel_data_estado)


# Obs. especifique o caminho correto da base de dados. Está pré-determinado que a
# base de dados está na mesma pasta do script R

# renomear para a base de dados para 'df' como simplificação

df <- r_filtro_hp_panel_data_estado
View(df)

## 3) Aplicar o filtro HP

# Considere as colunas: uf, ano, pib_est_ipca

# Obs. O filtro Hodrick-Prescott (HP) usa o parâmetro $\lambda = 6.25$ para dados anuais, 
# conforme a literatura padrão (Ravn & Uhlig, 2002).

# 3.1) filtro hp em nível
# 3.2) filtro hp em log

df_hp <- df %>%
  group_by(uf) %>%
  arrange(ano) %>%
  mutate(
    # Em nível - para gráficos
    pib_pot = as.numeric(hpfilter(pib_est_ipca, freq = 6.25)$trend), #produto potencial 
    pib_gap = as.numeric(hpfilter(pib_est_ipca, freq = 6.25)$cycle), #hiato do produto
    
    # Em log - usado no modelo econométrico
    log_pib     = log(pib_est_ipca), # produto observado em log
    log_pib_pot = as.numeric(hpfilter(log_pib, freq = 6.25)$trend), #produto potencial em log 
    log_pib_gap = as.numeric(hpfilter(log_pib, freq = 6.25)$cycle) #hiato do produto em log
  ) %>%
  ungroup()

# Visualizar o resultado final com todas as colunas
View(df_hp)

# Obs. cuidado com o filtro HP na medida em que é um filtro estatístico e distorce os pontos
# extremos das observações. No entanto, é uma indicação do produto potencial ao longo do tempo

# 4) gráficos do produto potencial e hiato do produto

# obs. consideramos os estados selecionados do Paraná (PR), São Paulo (Sp), Mato Grosso do Sul (MS),
# Pernambuco (PE). 

## 5) Gráficos

#Obs. setar coluna ano como numérica

df_hp$ano <- as.numeric(as.character(df_hp$ano))

# 5.1 Gráfico do produto real, potencial e hiato (EM NÍVEL)

# 5.1.a - Produto real vs potencial

df_hp %>%
  filter(uf %in% c("PR", "SP", "MS", "PE")) %>%
  ggplot(aes(x = ano)) +
  geom_line(aes(y = pib_est_ipca / 1e9, color = "Produto Real"), linewidth = 1) + # 'linewidth' no lugar de 'size'
  geom_line(aes(y = pib_pot / 1e9, color = "Produto Potencial (Filtro HP)"), linetype = "dashed", linewidth = 1) +
  facet_wrap(~uf, scales = "free_y", ncol = 2) + 
  labs(title = "Produto Real vs. Produto Potencial (Em nível)",
       subtitle = "Estados Selecionados (2000-2023)",
       x = "Ano", 
       y = "R$ (Bilhões - Preços de 2023)",
       color = "Legenda") +
  scale_x_continuous(breaks = seq(2000, 2023, by = 5)) + 
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

# 5.1.b - Hiato do produto

df_hp %>%
  filter(uf %in% c("PR", "SP", "MS", "PE")) %>%
  ggplot(aes(x = ano, y = pib_gap / 1e9)) +
  geom_area(aes(fill = pib_gap > 0), alpha = 0.5) +
  geom_line(color = "black", linewidth = 0.3) + # 'linewidth' aqui também
  geom_hline(yintercept = 0, linetype = "solid", color = "darkred") +
  facet_wrap(~uf, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"), 
                    labels = c("FALSE" = "Hiato Negativo", "TRUE" = "Hiato Positivo")) +
  labs(title = "Hiato do Produto por Estado (Em Nível)",
       subtitle = "Desvios em relação à tendência de longo prazo (R$ Bilhões)",
       x = "Ano", 
       y = "Hiato (R$ Bilhões)",
       fill = "Ciclo Econômico") +
  scale_x_continuous(breaks = seq(2000, 2023, by = 5)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))


# 5.2 Gráfico do produto real, potencial e hiato (EM LOG)

# 5.2.a - Produto real vs potencial (EM LOG)

df_hp %>%
  filter(uf %in% c("PR", "SP", "MS", "PE")) %>%
  ggplot(aes(x = ano)) +
  # Usamos as colunas log_pib e log_pib_pot diretamente
  geom_line(aes(y = log_pib, color = "Produto Real"), linewidth = 1) +
  geom_line(aes(y = log_pib_pot, color = "Produto Potencial"), linetype = "dashed", linewidth = 1) +
  facet_wrap(~uf, scales = "free_y", ncol = 2) + 
  labs(title = "Produto Real vs. Produto Potencial (Em log)",
       subtitle = "Estados Selecionados (2000-2023)",
       x = "Ano", 
       y = "Log (Produto Real)",
       color = "Legenda") +
  scale_x_continuous(breaks = seq(2000, 2023, by = 5)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

# 5.2.b - Hiato do produto (EM LOG)

df_hp %>%
  filter(uf %in% c("PR", "SP", "MS", "PE")) %>%
  ggplot(aes(x = ano, y = log_pib_gap)) + 
  # A área colorida agora se baseia no sinal do log_pib_gap
  geom_area(aes(fill = log_pib_gap > 0), alpha = 0.5) +
  geom_line(color = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "solid", color = "darkred") +
  facet_wrap(~uf, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"), 
                    labels = c("FALSE" = "Hiato Negativo", "TRUE" = "Hiato Positivo")) +
  labs(title = "Hiato do Produto por Estado (Em log)",
       subtitle = "Valores próximos de 0.01 representam desvios de 1% da tendência",
       x = "Ano", 
       y = "Hiato (Diferença em Log)",
       fill = "Ciclo Econômico") +
  scale_x_continuous(breaks = seq(2000, 2023, by = 5)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

# Obs. podem haver divergências entre o hiato em nível e em log, mas deve indicar uma 
# tendência similar de longo prazo

# 6) Dummies de hiato positivo e negativo

# Obs. a partir do hiato em log criamos as dummíes de hiato. Sendo:

# Hiato positivo - se produto real > produto potencial
# Hiato negativo - se produto real <= produto potencial

# utilizamos os resultados em log por serem mais precisos

df_hp <- df_hp %>%
  group_by(uf) %>%
  mutate(
    # Baseia a dummy no sinal do componente cíclico (hiato) calculado
    d_gap_pos = ifelse(log_pib_gap > 0, 1, 0), # hiato positivo
    d_gap_neg = ifelse(log_pib_gap <= 0, 1, 0)  # hiato negativo
  ) %>%
  ungroup()




# Visualizar o resultado final com todas as colunas
View(df_hp)

# Gráfico 

# 1. Preparação com áreas de fundo (Shaded Regions)

df_plot <- df_hp %>%
  filter(uf %in% c("PR", "SP", "MS", "PE")) %>%
  group_by(uf) %>%
  mutate(
    regime = ifelse(d_gap_pos == 1, "Hiato Positivo (Boom)", "Hiato Negativo (Crise)")
  ) %>%
  ungroup()

# 2. Gráfico com Geoms de Área e Linha

ggplot(df_plot, aes(x = ano)) +
  # Adiciona áreas coloridas de fundo para os regimes
  geom_rect(aes(xmin = ano - 0.5, xmax = ano + 0.5, 
                ymin = -Inf, ymax = Inf, fill = regime), 
            alpha = 0.2) +
  
  # Linha do PIB Real em Log (Linha contínua)
  geom_line(aes(y = log_pib, color = "Produto Real"), linewidth = 0.8) +
  
  # Linha da Tendência/Potencial (Tracejada)
  geom_line(aes(y = log_pib_pot, color = "Produto Potencial"), 
            linetype = "dashed", linewidth = 0.8) +
  
  # Facetamento
  facet_wrap(~uf, scales = "free_y", ncol = 2) +
  
  # Definição de Cores
  scale_fill_manual(values = c("Hiato Positivo (Boom)" = "steelblue", 
                               "Hiato Negativo (Crise)" = "firebrick")) +
  scale_color_manual(values = c("Produto Real" = "black", 
                                "Produto Potencial" = "grey30")) +
  
  # Títulos e Labels
  labs(title = "Ciclos Econômicos e Tendência Estrutural",
       subtitle = "Regimes de Hiato (Sombreado) vs. Produto (Em Log)",
       x = "Ano", y = "Produto (Em Log)",
       fill = "Ciclo (Dummy)", color = "Séries") +
  
  scale_x_continuous(breaks = seq(2000, 2023, by = 5)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 11),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA))

# 7) Exportar dados para planilha

library(writexl)

write_xlsx(df_hp, "df_hp.xlsx")

#obs. exportado para planilha xlsx para a mesma pasta onde o script foi aberto

## FIM

