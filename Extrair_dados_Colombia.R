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
  
  df <- df %>% filter(str_detect(C_BAS1, "^X6|^X7|^80|^X81|^X82|^X83|^X84|^Y1"))
  
  # QUEBRA A COLUNA "C_BAS1" E DESCARTA O TRECHO COM A DESCRIÇÃO DO CID
  
  df <- df %>% separate(C_BAS1, into = c('C_BAS_3_DIGITOS', 'C_BAS_DESCARTAR'), sep = 3)
  df$C_BAS_DESCARTAR <- NULL
  
  # CRIA NOVA COLUNA ORGANIZANDO OS RESULTADOS PELOS CIDs
  
  df$CAUSA_CAT <- recode(df$C_BAS_3_DIGITOS, "X70" = "enforcamento", 
                         "X71" = "enforcamento",
                         "X60" = "intoxicacao",
                         "X61" = "intoxicacao",
                         "X62" = "intoxicacao",
                         "X63" = "intoxicacao",
                         "X64" = "intoxicacao",
                         "X65" = "intoxicacao",
                         "X66" = "intoxicacao",
                         "X67" = "intoxicacao",
                         "X69" = "intoxicacao",
                         "Y10" = "intoxicacao",
                         "Y11" = "intoxicacao",
                         "Y12" = "intoxicacao",
                         "Y13" = "intoxicacao",
                         "Y14" = "intoxicacao",
                         "Y15" = "intoxicacao",
                         "Y16" = "intoxicacao",
                         "Y17" = "intoxicacao",
                         "Y19" = "intoxicacao",
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
                         "X81" = "veiculo",
                         "X82" = "veiculo",
                         "X83" = "outros",
                         "X84" = "outros")
  
  df$C_BAS_3_DIGITOS <- NULL
  df$C_BAS1 <- NULL
  
  # AGRUPA OS RESULTADOS POR SEXO E CAUSA

  df$QT_TEMP = 1
  df <- df %>% group_by(CAUSA_CAT, SEXO) %>%
    summarise(QT = sum(QT_TEMP))
  
  # CRIA AS COLUNAS ANO E PAÍS
  
  df$ANO = y
  df$PAIS = "Colombia"
  
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

df <- df %>% filter(str_detect(c_bas1, "^X6|^X7|^80|^X81|^X82|^X83|^X84|^Y1"))

# QUEBRA A COLUNA "c_bas1" E DESCARTA O TRECHO COM A DESCRIÇÃO DO CID

df <- df %>% separate(c_bas1, into = c('C_BAS_3_DIGITOS', 'C_BAS_DESCARTAR'), sep = 3)
df$C_BAS_DESCARTAR <- NULL

# CRIA NOVA COLUNA ORGANIZANDO OS RESULTADOS PELOS CIDs

df$CAUSA_CAT <- recode(df$C_BAS_3_DIGITOS, "X70" = "enforcamento", 
                       "X71" = "enforcamento",
                       "X60" = "intoxicacao",
                       "X61" = "intoxicacao",
                       "X62" = "intoxicacao",
                       "X63" = "intoxicacao",
                       "X64" = "intoxicacao",
                       "X65" = "intoxicacao",
                       "X66" = "intoxicacao",
                       "X67" = "intoxicacao",
                       "X69" = "intoxicacao",
                       "Y10" = "intoxicacao",
                       "Y11" = "intoxicacao",
                       "Y12" = "intoxicacao",
                       "Y13" = "intoxicacao",
                       "Y14" = "intoxicacao",
                       "Y15" = "intoxicacao",
                       "Y16" = "intoxicacao",
                       "Y17" = "intoxicacao",
                       "Y19" = "intoxicacao",
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
                       "X81" = "veiculo",
                       "X82" = "veiculo",
                       "X83" = "outros",
                       "X84" = "outros")

df$C_BAS_3_DIGITOS <- NULL
df$c_bas1 <- NULL

# AGRUPA OS RESULTADOS POR SEXO E CAUSA

df$QT_TEMP = 1
df <- df %>% group_by(CAUSA_CAT, sexo) %>%
  summarise(QT = sum(QT_TEMP))

# CRIA AS COLUNAS ANO E PAÍS

df$ANO = "2014"
df$PAIS = "Colombia"

# RENOMEIA AS VARIÁVEIS

df <- df %>% rename(SEXO = sexo)

# SALVA O ARQUIVO EM CSV

file_name_final <- "C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Colombia/2014_CO_final.csv"
write.csv(df, file_name_final, row.names = FALSE)


# AMDG


