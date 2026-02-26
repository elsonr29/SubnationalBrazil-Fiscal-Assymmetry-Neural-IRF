## 1.b - Caĺculo dos termos de troca dos estados

# Preparação dos dados

# Descrição: 
# a) Construir os termos de troca (tdt) a partir dos dados de exportações e importações (US$ FOB)
# e da quantidade (em kg) para cada estado
# b) utilizamos como fonte os dados primário da Fonte: SECEX/MDIC

# Obs. Usamos os termos de troca (tdt) em log como controle, a mudança de base apenas desloca a série 
# para cima ou para baixo (muda o intercepto), mas a elasticidade (como o TdT afeta a sua 
# variável fiscal) permanece idêntica. Portanto, escolhemos o ano base 1997 por ser o ano de 
# inicio da série disponibilizada pelo SECEX/MDIC


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


# 2) Dados


library(here)
library(readxl)

# verificar o caminho e se o arquivo existe

setwd("/home/erodrigo/Área de trabalho/Academico/3.4 Working papers_publica/01_paper_2021_01 crise fiscal, receita e despesa_submeter nova-economia-UFMG_mar_2026/data_codigos/01_trata_data/")
file.exists("r_tdt_data_estado.xlsx")

library(readxl)
r_tdt_data_estado <- read_excel("r_tdt_data_estado.xlsx", 
                                            sheet = "tdt")

View(r_tdt_data_estado)


# Obs. especifique o caminho correto da base de dados. Está pré-determinado que a
# base de dados está na mesma pasta do script R

# renomear para a base de dados para 'df' como simplificação

df <- r_tdt_data_estado
View(df)

# 3) Cálculo dos preços únitários

df_tdt <- df %>%
  group_by(uf) %>%
  arrange(ano) %>%
  mutate(
    p_unit_imp = imp_fob / imp_kg,
    p_unit_exp = exp_fob / exp_kg
  ) %>%
  ungroup()

# Visualizar os dados

View(df_tdt)

# 4) Transformação em índices (base 2000 = 100)

df_tdt <- df_tdt %>%
  group_by(uf) %>%
  arrange(ano) %>%
  mutate(
    idx_exp = (p_unit_exp / first(na.omit(p_unit_exp))) * 100,
    idx_imp = (p_unit_imp / first(na.omit(p_unit_imp))) * 100
          ) %>%
  ungroup()
  
# Visualizar os dados

View(df_tdt)

# 5) Termos de troca - tdt

df_tdt <- df_tdt %>%
  group_by(uf) %>%
  arrange(ano) %>%
  mutate(
    tdt = (idx_exp / idx_imp) * 100
    ) %>%
  ungroup()

# em log

df_tdt <- df_tdt %>%
  group_by(uf) %>%
  arrange(ano) %>%
  mutate(
    log_tdt = log(tdt)
  ) %>%
  ungroup()

# Visualizar os dados

View(df_tdt)

# 6) Gráficos

# 6.1 - Evolução dos termos de troca (base 2000 = 100)

df_tdt %>%
  filter(uf %in% c("PR", "SP", "MS", "PE")) %>%
  ggplot(aes(x = ano, y = tdt, color = uf)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey40") +
  labs(title = "Evolução dos Termos de Troca (2000-2023)",
       subtitle = "Comparação entre estados selecionados (Base 100 = Ano 2000)",
       x = "Ano", y = "Índice de Termos de Troca",
       color = "Estado") +
  scale_x_continuous(breaks = seq(2000, 2023, by = 5)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

# Em log

df_tdt %>%
  filter(uf %in% c("PR", "SP", "MS", "PE")) %>%
  ggplot(aes(x = ano, y = tdt, color = uf, group = uf)) +
  # Linha de referência no 100 (Equilíbrio do Ano Base)
  geom_hline(yintercept = 100, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(2000, 2023, by = 5)) +
  labs(title = "Evolução dos Termos de Troca (TdT)",
       subtitle = "Comparação por Estado (Base 100 = Ano 2000)",
       caption = "Valores acima de 100 indicam ganho de poder de compra internacional.",
       x = "Ano", 
       y = "Índice (Base 100)",
       color = "Estado") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold"))

# 6.2 - Destaque dos ciclos relacionado ao tdt

df_tdt %>%
  filter(uf %in% c("PR", "SP", "MS", "PE")) %>%
  ggplot(aes(x = ano, y = tdt)) +
  geom_area(aes(fill = tdt > 100), alpha = 0.3) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 100, linetype = "solid", color = "darkred") +
  facet_wrap(~uf, ncol = 2) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
                    labels = c("FALSE" = "Deterioração", "TRUE" = "Melhora")) +
  labs(title = "Ciclos de Poder de Compra Externo",
       subtitle = "Termos de Troca em relação ao ano base",
       x = "Ano", y = "Índice (Base 100)",
       fill = "Situação") +
  theme_light() +
  theme(legend.position = "bottom")

# Em log

df_tdt %>%
  filter(uf %in% c("PR", "SP", "MS", "PE")) %>%
  ggplot(aes(x = ano, y = log_tdt)) +
  # A lógica de preenchimento agora baseia-se no log de 100 (4.605)
  geom_area(aes(fill = log_tdt > log(100)), alpha = 0.3) +
  geom_line(color = "black", linewidth = 0.8) +
  # A linha horizontal de referência agora é o log de 100
  geom_hline(yintercept = log(100), linetype = "solid", color = "darkred") +
  facet_wrap(~uf, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
                    labels = c("FALSE" = "Deterioração (Log)", "TRUE" = "Melhora (Log)")) +
  labs(title = "Ciclos de Poder de Compra Externo (Escala Log)",
       subtitle = "Termos de Troca: Log(TdT) com referência em log(100)",
       x = "Ano", 
       y = "Log (Índice TdT)",
       fill = "Situação") +
  theme_light() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

# 6.3 - Distribuição e volatilidade

df_tdt %>%
  filter(uf %in% c("PR", "SP", "MS", "PE")) %>%
  ggplot(aes(x = uf, y = tdt, fill = uf)) +
  geom_boxplot(outlier.colour = "red", alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "Dispersão e Volatilidade dos Termos de Troca",
       subtitle = "A linha preta indica a mediana; o diamante indica a média",
       x = "Estado", y = "Índice de TdT") +
  theme_minimal() +
  theme(legend.position = "none")

# Em log

df_tdt %>%
  filter(uf %in% c("PR", "SP", "MS", "PE")) %>%
  ggplot(aes(x = uf, y = tdt, fill = uf)) +
  geom_boxplot(outlier.colour = "red", alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "Dispersão e Volatilidade dos Termos de Troca",
       subtitle = "A linha preta indica a mediana; o diamante indica a média",
       x = "Estado", y = "Índice de TdT") +
  theme_minimal() +
  theme(legend.position = "none")


# 7) Exportar dados para planilha

library(writexl)

write_xlsx(df_tdt, "df_tdt.xlsx")

#obs. exportado para planilha xlsx para a mesma pasta onde o script foi aberto

## FIM