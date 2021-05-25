library(dplyr)
library(tidyverse)
library(readr)
library(pastecs)
library(fmsb)
library(writexl)


# CARREGA O BANCO

banco <- read_csv("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/data_final.csv")


# INSPECIONA AS VARIÁVEIS

df <- as.data.frame(table(banco$SEXO))
df <- rename(df, SEXO = Var1 )
df$perc <- round((df$Freq / sum(df$Freq))*100,2)
write.table(df, file = "SEXO.txt", sep = ";", quote = FALSE)
df

df <- as.data.frame(table(banco$CAUSA_CAT))
df <- rename(df, CAUSA_CAT = Var1 )
df$perc <- round((df$Freq / sum(df$Freq))*100,2)
write.table(df, file = "CAUSA_CAT.txt", sep = ";", quote = FALSE)
df

df <- as.data.frame(table(banco$ANO))
df <- rename(df, ANO = Var1 )
df$perc <- round((df$Freq / sum(df$Freq))*100,2)
write.table(df, file = "ANO.txt", sep = ";", quote = FALSE)
df

df <- as.data.frame(table(banco$PAIS))
df <- rename(df, PAIS = Var1 )
df$perc <- round((df$Freq / sum(df$Freq))*100,2)
write.table(df, file = "PAIS.txt", sep = ";", quote = FALSE)
df

res <- stat.desc(banco[, 3])
write.table(res, file = "QUANTITATIVA.txt", sep = ";", quote = FALSE)
res


# RETIRA O SEXO CODIFICADO COMO "9" - ARGENTINA

df_excluido <- subset(banco, SEXO=="9")
write.table(df_excluido, file = "EXCLUIDO.txt", sep = ";", quote = FALSE)
banco <- subset(banco, !SEXO=="9")


# ANÁLISE

# DEFINIR PAIS E ANO, NO FORMATO "Pais" E "09".

escolha_pais <- "Brasil"
escolha_ano <- "09"


# RODAR A ANÁLISE A PARTIR DAQUI

# CRIA PRIMEIRA PARTE DA TABELA DE SAÍDA (TABELA INSTRUMENTO E SEXO POR PAIS)

pais_instrumento <- banco %>%
  filter(PAIS == escolha_pais & ANO == escolha_ano) %>%
  select(CAUSA_CAT, SEXO, QT) %>%
  pivot_wider(names_from = SEXO, values_from = QT) %>% 
  rename(Masculino = "1", Feminino = "2") %>%
  mutate(Porc_Masculino = 100*Masculino/sum(Masculino)) %>%
  mutate(Porc_Feminino = 100*Feminino/sum(Feminino)) %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "TOTAL")) %>% 
  mutate(obs = 1:n())

# FUNÇÃO PARA CALCULAR O ODDS RATIO  

chama <- function(x){
 
  df <- oddsratio(
    as.numeric(pais_instrumento[x,2]), 
    (as.numeric(pais_instrumento[8,2])-as.numeric(pais_instrumento[x,2])), 
    as.numeric(pais_instrumento[x,3]), 
    (as.numeric(pais_instrumento[8,3])-as.numeric(pais_instrumento[x,3]))
  )
  
  z <- c(x,
         as.numeric(df[["p.value"]]),
         as.numeric(df[["conf.int"]]),
         as.numeric(df[["estimate"]])
  )

}

# RODA A FUNÇÃO PARA CALCULAR O ODDS RATIO POR LINHA DA TABELA DE SAÍDA

linha <- c(1,2,3,4,5,6,7,8)

odds_trans <- sapply(linha, chama)

# TRANSPÕE LINHAS E COLUNAS DA TABELA CRIADA PELA FUNÇÃO ODDS

odds_trans <- data.frame(odds_trans)
odds <- data.frame(t(odds_trans))

# RENOMEIA AS VARIÁVEIS DA TABELA CRIADA PELA FUNÇÃO ODDS

odds <- rename(odds, obs = X1, valor_p = X2, limite_inferior = X3, limite_superior = X4, estimativa = X5)

# JUNTA A TABELA INSTRUMENTO E SEXO POR PAIS COM A TABELA CRIADA PELA FUNÇÃO ODDS

pais_instrumento_odds <- merge(pais_instrumento, odds, by="obs")
pais_instrumento_odds$obs <- NULL

nova_ordem_colunas <- c("CAUSA_CAT", 
                        "Masculino", "Porc_Masculino", 
                        "Feminino", "Porc_Feminino", 
                        "estimativa", "limite_inferior", "limite_superior", "valor_p")
pais_instrumento_odds <- pais_instrumento_odds[, nova_ordem_colunas]


file_name_final <- paste("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/",as.character(escolha_pais),"_",as.character(escolha_ano),".xlsx", sep = "") 
write_xlsx(pais_instrumento_odds, file_name_final)




