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
  
  df <- df %>% filter(str_detect(CAUSA, "^X6|^X7|^80|^X81|^X82|^X83|^X84|^Y1"))
  
  # CRIA NOVA COLUNA ORGANIZANDO OS RESULTADOS PELOS CIDs
  
  df$CAUSA_CAT <- recode(df$CAUSA, "X70" = "enforcamento", 
                         "X71" = "enforcamento",
                         "X60" = "intoxicação",
                         "X61" = "intoxicação",
                         "X62" = "intoxicação",
                         "X63" = "intoxicação",
                         "X64" = "intoxicação",
                         "X65" = "intoxicação",
                         "X66" = "intoxicação",
                         "X67" = "intoxicação",
                         "X69" = "intoxicação",
                         "Y10" = "intoxicação",
                         "Y11" = "intoxicação",
                         "Y12" = "intoxicação",
                         "Y13" = "intoxicação",
                         "Y14" = "intoxicação",
                         "Y15" = "intoxicação",
                         "Y16" = "intoxicação",
                         "Y17" = "intoxicação",
                         "Y19" = "intoxicação",
                         "X68" = "pesticidas",
                         "Y18" = "pesticidas",
                         "X72" = "armas",
                         "X73" = "armas",
                         "X74" = "armas",
                         "X75" = "armas",
                         "X76" = "armas",
                         "X77" = "armas",
                         "X78" = "armas",
                         "X79" = "armas",
                         "X80" = "altura",
                         "X81" = "veículo",
                         "X82" = "veículo",
                         "X83" = "outros",
                         "X84" = "outros")
  df$CAUSA <- NULL
  
  # AGRUPA OS RESULTADOS POR SEXO E CAUSA
  
  df <- df %>% group_by(CAUSA_CAT, SEXO) %>%
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