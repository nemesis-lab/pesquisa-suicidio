library(dplyr)
library(tidyverse)
library(haven)

# EXTRAIR DADOS DO BANCO DA COLOMBIA

colombia <- function(df_choice){
  
  y <- as.character(df_choice)
  
  # ABRE O ARQUIVO DO BANCO

  file_name <- paste("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Colombia/Defun_20",as.character(y),".sav", sep = "")

  df <- read_sav(file_name)
  
  # SELECIONA AS COLUNAS DE INTERESSE
  
  df <- df %>% select(SEXO, GRU_ED1, C_BAS1)
  
  # FILTRA AS IDADES DE INTERESSE
  
  df <- df %>% filter(GRU_ED1 == "10" | GRU_ED1 == "11")

  # FILTRA AS CAUSAS DE INTERESSE
  
  df <- df %>% filter(str_detect(C_BAS1, "^X6|^X7|^X81|^X82|^X83|^X84"))
  
  # AGRUPA OS RESULTADOS POR SEXO E CAUSA

  df$QT_TEMP = 1
  df <- df %>% group_by(C_BAS1, SEXO) %>%
    summarise(QT = sum(QT_TEMP))
  
  # CRIA AS COLUNAS ANO E PAÍS
  
  df$ANO = y
  df$PAIS = "Colombia"
  
  # RENOMEIA AS VARIÁVEIS
  
  df <- df %>% rename(CAUSA = C_BAS1)
  
  # SALVA O ARQUIVO EM CSV
  
  file_name_final <- paste("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Colombia/20",as.character(y),"_CO_final.csv", sep = "") 
  write.csv(df, file_name_final, row.names = FALSE)
  
}

periodo <- c("09", "10", "11", "12", "13", "15", "16", "17", "18")

sapply(periodo, colombia)



# ANO 2014

# PARA ESSE ANO (2014) HÁ MUDANÇA NO PADRÃO DOS NOMES DAS VARIÁVEIS, mudança para caixa baixa, PORTANTO NÃO FOI UTILIZADA A FUNÇÃO ACIMA

file_name <- paste("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Colombia/Defun_2014.sav", sep = "")

df <- read_sav(file_name)

# SELECIONA AS COLUNAS DE INTERESSE

df %>% select(sexo, gru_ed1, c_bas1)

# FILTRA AS IDADES DE INTERESSE

df <- df %>% filter(gru_ed1 == "10" | gru_ed1 == "11")

# FILTRA AS CAUSAS DE INTERESSE

df <- df %>% filter(str_detect(c_bas1, "^X6|^X7|^X8"))

# AGRUPA OS RESULTADOS POR SEXO E CAUSA

df$QT_TEMP = 1
df <- df %>% group_by(c_bas1, sexo) %>%
  summarise(QT = sum(QT_TEMP))

# CRIA AS COLUNAS ANO E PAÍS

df$ANO = "14"
df$PAIS = "Colombia"

# RENOMEIA AS VARIÁVEIS

df <- df %>% rename(CAUSA = c_bas1)
df <- df %>% rename(SEXO = sexo)


# SALVA O ARQUIVO EM CSV

file_name_final <- "C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Colombia/2014_CO_final.csv"
write.csv(df, file_name_final, row.names = FALSE)


# AMDG


