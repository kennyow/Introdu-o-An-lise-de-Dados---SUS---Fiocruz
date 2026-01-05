# ==============================================================================
# MÓDULO 1 - AULA 2: INTRODUÇÃO À LINGUAGEM DE PROGRAMAÇÃO R 
# Curso: Introdução à Análise de Dados para Pesquisa no SUS
# ==============================================================================

# Carregar pacotes necessários
library(tidyverse)
library(lubridate)
library(readr)

df_csv <- read_csv("../dados/sim_salvador_2023_processado.csv")

head(df_csv)

# #1.1:Crie uma nova variável chamada "faixa_etaria" que classifique as idades em quatro categorias: 
# "Criança" para idades de 0 a 12 anos, "Adolescente" para 13 a 17 anos, "Adulto" para 18 a 59 anos e 
# "Idoso" para 60 anos ou mais.

df_csv <- df_csv %>% 
  mutate(
    faixa_etaria = case_when(
      idade_anos >= 0 & idade_anos <= 12 ~ 'crianca',
      idade_anos >= 13 & idade_anos <= 17 ~ 'adolescente',
      idade_anos >= 18 & idade_anos <= 59 ~ 'adulto',
      idade_anos >= 60  ~ 'idoso',
      is.na(idade_anos) ~ NA_character_
    )
  )

df_csv[c("sexo_p", 'idade_anos', "faixa_etaria")]


# Verificar a criação da variável
glimpse(df_csv)

#1.2 Conte quantos óbitos há em cada faixa etária criada. 
df_csv %>%
  count(faixa_etaria, sort = TRUE)

# Opção 2
df_csv %>%
  group_by(faixa_etaria) %>%
  summarise(total_obitos = n()) %>%
  arrange(desc(total_obitos))


# Atividade 2: Manipulação de Datas e Agrupamento
# 2.1 Crie uma variável chamada "trimestre" que identifique em qual trimestre do ano ocorreu o 
# óbito. Os trimestres devem ser classificados como: "1º Trimestre" para janeiro, fevereiro e março; 
# "2º Trimestre" para abril, maio e junho; "3º Trimestre" para julho, agosto e setembro; e "4º 
# Trimestre" para outubro, novembro e dezembro. 


df_csv <- df_csv %>%
  mutate(
    mes_numero = month(DTOBITO_dt),
    trimestre = case_when(
      mes_numero %in% c(1, 2, 3) ~ '1º Trimestre',
      mes_numero %in% c(4, 5, 6)  ~ '2º Trimestre',
      mes_numero %in% c(7, 8, 9)  ~ '3º Trimestre',
      mes_numero %in% c(10, 11, 12)   ~ '4º Trimestre',
      is.na(mes_numero) ~ NA_character_
    )
  )

df_csv[c("sexo_p", 'idade_anos', "faixa_etaria", 'trimestre')]

#2.2 Calcule o total de óbitos e a idade média por trimestre e por sexo.

df_csv %>%
  group_by(trimestre, sexo_p) %>%
  summarise(total_obitos = n(), idade_media = mean(idade_anos)) %>%
  arrange(trimestre, desc(total_obitos))


# Atividade 3: Análise Integrada
# 
# 3.1 Identifique qual foi o mês com maior número de óbitos no ano de 2023.
df_csv %>%
  mutate(mes_obito = month(DTOBITO_dt, label = TRUE, abbr = FALSE)) %>%
  count(mes_obito, sort = TRUE) %>%
  slice(1)  # Pega apenas a primeira linha (maior valor)

glimpse(df_csv)


# 3.2 Calcule a diferença percentual entre o número de óbitos masculinos e feminino
df_csv %>%
  filter(sexo_p %in% c('Masculino', 'Feminino')) %>% 
  count(sexo_p) %>%
  mutate(
    percentual = (n / sum(n)) * 100,
    percentual = round(percentual, 2)
  )
  

# 3.3 Determine qual faixa etária teve o maior número de óbitos ao longo do ano

df_csv %>%
  group_by(faixa_etaria) %>% 
  summarise(total_obitos = n()) %>% 
  arrange(desc(total_obitos))
  

# Salvar resumo das análises em CSV
resumo_atividades <- df_csv %>%
  group_by(trimestre, faixa_etaria, sexo_p) %>%
  summarise(
    total_obitos = n(),
    idade_media = round(mean(idade_anos, na.rm = TRUE), 1),
    .groups = "drop"
  )

write_csv(resumo_atividades, "resumo_atividades_aula2.csv")

# Mensagem final
cat("\n✓ Gabarito das atividades concluído!\n")
cat("✓ Todas as análises foram executadas com sucesso.\n")
cat("✓ Explore os gráficos e resultados gerados.\n")
