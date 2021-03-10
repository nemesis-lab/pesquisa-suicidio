library(dplyr)
library(tidyverse)
library(readr)

# EXTRAIR DADOS DO BANCO DA ARGENTINA

argentina <- function(df_choice){
    
  y <- as.character(df_choice)

  file_name <- paste("C:/Users/Carlos Garcia Filho/Desktop/Argentina/DefWeb",as.character(y),".csv", sep = "") 
  
  # DROP COLUNAS QUE NÃO SERÃO UTILIZADAS
  
  df <- read_csv(file_name, col_types = cols(MAT = col_skip(), PROVRES = col_skip()))

  # QUEBRA COLUNA "GRUPEDAD" EM DUAS "COD_IDADE" E "IDADE" E DROP A COLUNA "COD_IDADE"
  
  df <- separate(df, col = GRUPEDAD, into = c("COD_IDADE","IDADE"), sep = "_")

  df$COD_IDADE <- NULL

  # FILTRA AS IDADES DE INTERESSE
  
  df <- df %>% filter(IDADE == "10 a 14" | IDADE == "15 a 19")

  # FILTRA AS CAUSAS DE INTERESSE
  
  df <- df %>% filter(CAUSA == "X60" | 
                      CAUSA == "X61" |
                      CAUSA == "X62" |
                      CAUSA == "X63" |
                      CAUSA == "X64" |
                      CAUSA == "X65" |
                      CAUSA == "X66" |
                      CAUSA == "X67" |
                      CAUSA == "X68" |
                      CAUSA == "X69" |
                      CAUSA == "X70" |
                      CAUSA == "X71" |
                      CAUSA == "X72" |
                      CAUSA == "X73" |
                      CAUSA == "X74" |
                      CAUSA == "X75" |
                      CAUSA == "X76" |
                      CAUSA == "X77" |
                      CAUSA == "X78" |
                      CAUSA == "X79" |
                      CAUSA == "X80" |
                      CAUSA == "X81" |
                      CAUSA == "X82" |
                      CAUSA == "X83" |
                      CAUSA == "X84")

  # SALVA O ARQUIVO EM CSV
  
  file_name_final <- paste("C:/Users/Carlos Garcia Filho/Desktop/Argentina/",as.character(y),"final.csv", collapse = NULL) 
  write.csv(df, file_name_final, row.names = FALSE)
  
}

periodo <- c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18")

sapply(periodo, argentina)