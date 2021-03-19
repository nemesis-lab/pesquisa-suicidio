library(dplyr)
library(tidyverse)
library(readr)
library(stringr)

# ABRE O ARQUIVO DO BANCO FEMININO

fem <- read_delim("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Brasil/fem.txt", 
                  ";", escape_double = FALSE, col_types = cols(`2009` = col_double(), 
                                                               `2010` = col_double(), `2011` = col_double(), 
                                                               `2012` = col_double(), `2013` = col_double(), 
                                                               `2014` = col_double(), `2015` = col_double(), 
                                                               `2016` = col_double(), `2017` = col_double(), 
                                                               `2018` = col_double(), Total = col_skip()), 
                  trim_ws = TRUE)

# QUEBRA A COLUNA "Categoria CID10" E DESCARTA O TRECHO COM A DESCRIÇÃO DO CID

brasil_F <- separate(fem, col = `Categoria CID10`, into = c("CAUSA","DESCARTAR"), sep = "   ")
brasil_F$DESCARTAR <- NULL

# AGRUPA OS CIDs EM CATEGORIAS E DESCARTA A COLUNA COM OS CIDs

brasil_F$CAUSA_CAT <- recode(brasil_F$CAUSA,"X70" = "enforcamento", 
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
brasil_F$CAUSA <- NULL

# FUNÇÃO PARA SEPARAR OS DADOS DO BRASIL SEXO FEMININO

brasil_feminino <- function(ano, df_choice){
  
  y <- as.character(ano)
  df <- df_choice
  
  # SELECIONA O ANO
  
  df <- df %>%  select(CAUSA_CAT, y)
  
  # RETIRA AS LINHAS QUE ESTÃO COM N/A
  
  df <- df[complete.cases(df), ]
  
  # RENOMEIA A COLUNA Y
  
  df <- df %>% rename(QT_TEMP = y)
  
  # AGRUPA OS RESULTADOS POR SEXO E CAUSA
  
  df <- df %>% group_by(CAUSA_CAT) %>%
    summarise(QT = sum(QT_TEMP))
  
  # CRIA COLUNAS COM ANO, PAIS E SEXO
  
  a <- unlist(str_split(y, "20", 2))[2]
  df$ANO = a
  df$PAIS = "Brasil"
  df$SEXO= "2"
  
  # SALVA ARQUIVO
  
  file_name_final <- paste("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Brasil/",as.character(y),"_BR_F.csv", sep = "") 
  write.csv(df, file_name_final, row.names = FALSE)
  
}
  
# CHAMA A FUNÇÃO

brasil_feminino(2009, brasil_F)
brasil_feminino(2010, brasil_F)
brasil_feminino(2011, brasil_F)
brasil_feminino(2012, brasil_F)
brasil_feminino(2013, brasil_F)
brasil_feminino(2014, brasil_F)
brasil_feminino(2015, brasil_F)
brasil_feminino(2016, brasil_F)
brasil_feminino(2017, brasil_F)
brasil_feminino(2018, brasil_F)

# ABRE O ARQUIVO DO BANCO MASCULINO

masc <- read_delim("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Brasil/masc.txt", 
                  ";", escape_double = FALSE, col_types = cols(`2009` = col_double(), 
                                                               `2010` = col_double(), `2011` = col_double(), 
                                                               `2012` = col_double(), `2013` = col_double(), 
                                                               `2014` = col_double(), `2015` = col_double(), 
                                                               `2016` = col_double(), `2017` = col_double(), 
                                                               `2018` = col_double(), Total = col_skip()), 
                  trim_ws = TRUE)

# QUEBRA A COLUNA "Categoria CID10" E DESCARTA O TRECHO COM A DESCRIÇÃO DO CID

brasil_M <- separate(masc, col = `Categoria CID10`, into = c("CAUSA","DESCARTAR"), sep = "   ")
brasil_M$DESCARTAR <- NULL

# AGRUPA OS CIDs EM CATEGORIAS E DESCARTA A COLUNA COM OS CIDs

brasil_M$CAUSA_CAT <- recode(brasil_M$CAUSA, "X70" = "enforcamento", 
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
brasil_M$CAUSA <- NULL

# FUNÇÃO PARA SEPARAR OS DADOS DO BRASIL SEXO MASCULINO

brasil_masculino <- function(ano, df_choice){
  
  y <- as.character(ano)
  df <- df_choice
  
  # SELECIONA O ANO
  
  df <- df %>%  select(CAUSA_CAT, y)
  
  # RETIRA AS LINHAS QUE ESTÃO COM N/A
  
  df <- df[complete.cases(df), ]
  
  # RENOMEIA A COLUNA Y
  
  df <- df %>% rename(QT_TEMP = y)
  
  # AGRUPA OS RESULTADOS POR SEXO E CAUSA
  
  df <- df %>% group_by(CAUSA_CAT) %>%
    summarise(QT = sum(QT_TEMP))
  
  # CRIA COLUNAS COM ANO, PAIS E SEXO
  
  a <- unlist(str_split(y, "20", 2))[2]
  df$ANO = a
  df$PAIS = "Brasil"
  df$SEXO= "1"

  # SALVA ARQUIVO
  
  file_name_final <- paste("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Brasil/",as.character(y),"_BR_M.csv", sep = "") 
  write.csv(df, file_name_final, row.names = FALSE)
  
}

# CHAMA A FUNÇÃO

brasil_masculino(2009, brasil_M)
brasil_masculino(2010, brasil_M)
brasil_masculino(2011, brasil_M)
brasil_masculino(2012, brasil_M)
brasil_masculino(2013, brasil_M)
brasil_masculino(2014, brasil_M)
brasil_masculino(2015, brasil_M)
brasil_masculino(2016, brasil_M)
brasil_masculino(2017, brasil_M)
brasil_masculino(2018, brasil_M)


# AMDG





