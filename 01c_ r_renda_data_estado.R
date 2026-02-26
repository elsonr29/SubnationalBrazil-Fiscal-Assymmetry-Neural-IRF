## 1.c - Critérios para determinação dos grupos por faixa de renda

# Preparação dos dados

# Descrição:

# a) A partir dos dados de população e produto do ano de 2023 estabelecemos os critérios
# para classificação por grupo de renda dos estados
# b) Consideramos como critério o desvio da média renda per capita nacional para classificar 
# em três faixas de renda (alta, média e baixa)

# Critérios:

# alta - acima de 1,2 da média da renda per capita nacional
# média - entre 1,2 e 0,8 da média da renda per capita nacional
# baixa - abaixo de 0,8 da média renda per capita nacional

# Obs. base nos dados de renda per capita de 2023. E a média da renda per capita nacional
# não é a renda per capita nacional. O criteŕio atende a busca da distribuição da renda

# 1) Instalação dos pacotes

install.packages(c("dplyr", "writexl", "ggplot2"))

# Pacotes 

# dplyer - manipulação de dados
# writexl - exportação de dados para formato de planilha
# ggplot2 - gráficos

#carregar pacotes

library(dplyr)
library(writexl)
library(ggplot2)
library(readxl)

# 2) Dados

library(readxl)

# verificar o caminho e se o arquivo existe

setwd("/home/erodrigo/Área de trabalho/Academico/3.4 Working papers_publica/01_paper_2021_01 crise fiscal, receita e despesa_submeter nova-economia-UFMG_mar_2026/data_codigos/01_trata_data/")
file.exists("r_renda_data_estado.xlsx")

# carregar

r_renda_data_estado <- read_excel("r_renda_data_estado.xlsx", 
                                sheet = "renda")

View(r_renda_data_estado)


# Obs. especifique o caminho correto da base de dados. Está pré-determinado que a
# base de dados está na mesma pasta do script R

# renomear para a base de dados para 'df' como simplificação

df <- r_renda_data_estado

# Visualizar

View(df)

# 3) Aplicando os criteŕios

library(dplyr)

# Consideramos que pib_per_capita já está na base df_hp
df_renda <- df %>%
  mutate(
    media_nacional = mean(pib_per_est, na.rm = TRUE),
    ratio_media = pib_per_est / media_nacional,
    grupo_renda = case_when(
      ratio_media > 1.20 ~ "Alta Renda",
      ratio_media < 0.80 ~ "Baixa Renda",
      TRUE               ~ "Média Renda"
    )
  ) %>%
  ungroup() %>%
    mutate(grupo_renda = factor(grupo_renda, 
                              levels = c("Baixa Renda", 
                                         "Média Renda", 
                                         "Alta Renda")
                              )
         )

# Visualizar

View(df_renda)

# 4) Análise descritiva

df_renda %>%
  select(uf, ratio_media, grupo_renda) %>%
  arrange(desc(ratio_media))

# 5) Dummy de faixa de renda

df_renda <- df_renda %>%
  mutate(
    renda_relativa = pib_per_est / mean(pib_per_est, na.rm = TRUE),
    dummy_renda_alta  = ifelse(renda_relativa > 1.20, 1, 0),
    dummy_renda_baixa = ifelse(renda_relativa < 0.80, 1, 0),
    dummy_renda_media = ifelse(renda_relativa >= 0.80 & renda_relativa <= 1.20, 1, 0)
  ) %>%
  ungroup()

# Visualizar

View(df_renda)

# 6) Gráfico da desigualdade de renda

# 6.1 - Gráfico em barras

# Calculando a média nacional para a linha de referência

media_ref <- mean(df_renda$pib_per_est, na.rm = TRUE)

# Gráfico

ggplot(df_renda, aes(x = reorder(uf, pib_per_est), y = pib_per_est, fill = grupo_renda)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = media_ref, linetype = "dashed", color = "black", linewidth = 1) +
  geom_hline(yintercept = media_ref * 1.2, linetype = "dotted", color = "darkblue") +
  geom_hline(yintercept = media_ref * 0.8, linetype = "dotted", color = "darkred") +
    coord_flip() + 
  scale_fill_manual(values = c("Baixa Renda" = "#e41a1c", 
                               "Média Renda" = "#377eb8", 
                               "Alta Renda" = "#4daf4a")) +
  labs(title = "Desigualdade de Renda Per Capita entre Estados",
       subtitle = "Linha tracejada indica a média; linhas pontuadas indicam os limites das dummies (0.8 e 1.2)",
       x = "Estado", y = "PIB Per Capita (R$)",
       fill = "Faixa de Renda") +
  theme_minimal()

# 6.2 - Dispersão

ggplot(df_renda, aes(x = grupo_renda, y = pib_per_est, fill = grupo_renda)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5) + # Mostra os pontos (estados) dentro da caixa
  labs(title = "Dispersão da Renda por Grupo",
       x = "Grupo de Renda", y = "PIB Per Capita") +
  theme_light() +
  scale_fill_brewer(palette = "Set1")

# 6.3 - Distribuição e linha

# Linha de referência

media_nacional <- mean(df_renda$pib_per_est, na.rm = TRUE)

# Gráfico

ggplot(df_renda, aes(x = grupo_renda, y = pib_per_est, color = grupo_renda)) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
  geom_hline(yintercept = media_nacional, linetype = "dashed", color = "black", linewidth = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "black") +
  geom_text(aes(label = uf), vjust = -1, size = 3, check_overlap = TRUE) +
  scale_color_manual(values = c("Baixa Renda" = "#e41a1c", 
                                "Média Renda" = "#377eb8", 
                                "Alta Renda" = "#4daf4a")) +
  labs(title = "Dispersão da Renda Per Capita por Faixa",
       subtitle = "A linha tracejada indica a média nacional; os diamantes indicam a média do grupo",
       x = "Faixa de Renda (Classificação)", 
       y = "PIB Per Capita (R$)",
       caption = "Fonte: Elaboração própria com base nos dados de 2023.",
       color = "Grupo") +
  
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold"))

# 7) Exportar dados para planilha

library(writexl)

write_xlsx(df_renda, "df_renda.xlsx")

#obs. exportado para planilha xlsx para a mesma pasta onde o script foi aberto

## FIM