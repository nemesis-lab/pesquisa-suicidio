library(dplyr)
library(tidyverse)
library(readr)

# EXTRAIR DADOS DO BANCO DA ARGENTINA

argentina <- function(df_choice){
    
  y <- as.character(df_choice)
  
  # ABRE O ARQUIVO DO BANCO

  file_name <- paste("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Argentina/DefWeb",as.character(y),".csv", sep = "") 
  
  # DROP COLUNAS QUE NÃO SERÃO UTILIZADAS
  
  df <- read_csv(file_name, col_types = cols(MAT = col_skip(), PROVRES = col_skip()))

  # QUEBRA COLUNA "GRUPEDAD" EM DUAS "COD_IDADE" E "IDADE" E DROP A COLUNA "COD_IDADE"
  
  df <- separate(df, col = GRUPEDAD, into = c("COD_IDADE","IDADE"), sep = "_")

  df$COD_IDADE <- NULL

  # FILTRA AS IDADES DE INTERESSE
  
  df <- df %>% filter(IDADE == "10 a 14" | IDADE == "15 a 19")

  # FILTRA AS CAUSAS DE INTERESSE
  
  df <- df %>% filter(str_detect(CAUSA, "^X6|^X7|^X81|^X82|^X83|^X84"))
  
  # AGRUPA OS RESULTADOS POR SEXO E CAUSA
  
  df <- df %>% group_by(CAUSA, SEXO) %>%
    summarise(QT = sum(CUENTA))
  
  # CRIA AS COLUNAS ANO E PAÍS
  
  df$ANO = y
  df$PAIS = "Argentina"
  
  # SALVA O ARQUIVO EM CSV
  
  file_name_final <- paste("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Argentina/20",as.character(y),"_AR_final.csv", sep = "") 
  write.csv(df, file_name_final, row.names = FALSE)
  
}

periodo <- c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18")

sapply(periodo, argentina)

# AMDG