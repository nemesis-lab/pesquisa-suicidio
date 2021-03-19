library(dplyr)
library(tidyverse)
library(readr)

# CARREGA OS BANCOS INDIVIDUAIS

load_argentina <- function(ano){
  
  y <- as.character(ano)
  
  # ABRE OS ARQUIVOS PADRONIZADOS
  
  file_name <- paste("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Argentina/",as.character(y),"_AR_final.csv", sep = "") 
  df <- read_csv(file_name)

}


load_colombia <- function(ano){
  
  y <- as.character(ano)
  
  # ABRE OS ARQUIVOS PADRONIZADOS
  
  file_name <- paste("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Colombia/",as.character(y),"_CO_final.csv", sep = "") 
  df <- read_csv(file_name)
  
}


load_brasil_M <- function(ano){
  
  y <- as.character(ano)
  
  # ABRE OS ARQUIVOS PADRONIZADOS
  
  file_name <- paste("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Brasil/",as.character(y),"_BR_M.csv", sep = "") 
  df <- read_csv(file_name)
  
}


load_brasil_F <- function(ano){
  
  y <- as.character(ano)
  
  # ABRE OS ARQUIVOS PADRONIZADOS
  
  file_name <- paste("C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/Brasil/",as.character(y),"_BR_F.csv", sep = "") 
  df <- read_csv(file_name)
  
}


AR_2009 <- load_argentina(2009)
AR_2010 <- load_argentina(2010)
AR_2011 <- load_argentina(2011)
AR_2012 <- load_argentina(2012)
AR_2013 <- load_argentina(2013)
AR_2014 <- load_argentina(2014)
AR_2015 <- load_argentina(2015)
AR_2016 <- load_argentina(2016)
AR_2017 <- load_argentina(2017)
AR_2018 <- load_argentina(2018)


CO_2009 <- load_colombia(2009)
CO_2010 <- load_colombia(2010)
CO_2011 <- load_colombia(2011)
CO_2012 <- load_colombia(2012)
CO_2013 <- load_colombia(2013)
CO_2014 <- load_colombia(2014)
CO_2015 <- load_colombia(2015)
CO_2016 <- load_colombia(2016)
CO_2017 <- load_colombia(2017)
CO_2018 <- load_colombia(2018)


BR_M_2009 <- load_brasil_M(2009)
BR_M_2010 <- load_brasil_M(2010)
BR_M_2011 <- load_brasil_M(2011)
BR_M_2012 <- load_brasil_M(2012)
BR_M_2013 <- load_brasil_M(2013)
BR_M_2014 <- load_brasil_M(2014)
BR_M_2015 <- load_brasil_M(2015)
BR_M_2016 <- load_brasil_M(2016)
BR_M_2017 <- load_brasil_M(2017)
BR_M_2018 <- load_brasil_M(2018)


BR_F_2009 <- load_brasil_F(2009)
BR_F_2010 <- load_brasil_F(2010)
BR_F_2011 <- load_brasil_F(2011)
BR_F_2012 <- load_brasil_F(2012)
BR_F_2013 <- load_brasil_F(2013)
BR_F_2014 <- load_brasil_F(2014)
BR_F_2015 <- load_brasil_F(2015)
BR_F_2016 <- load_brasil_F(2016)
BR_F_2017 <- load_brasil_F(2017)
BR_F_2018 <- load_brasil_F(2018)

# JUNTA OS BANCOS EM UM ÚNICO

data <- rbind(AR_2009,
      AR_2010,
      AR_2011,
      AR_2012,
      AR_2013,
      AR_2014,
      AR_2015,
      AR_2016,
      AR_2017,
      AR_2018,
      
      CO_2009,
      CO_2010,
      CO_2011,
      CO_2012,
      CO_2013, 
      CO_2014, 
      CO_2015, 
      CO_2016, 
      CO_2017, 
      CO_2018, 
      
      BR_M_2009,
      BR_M_2010,
      BR_M_2011,
      BR_M_2012,
      BR_M_2013,
      BR_M_2014,
      BR_M_2015,
      BR_M_2016,
      BR_M_2017,
      BR_M_2018,
      
      BR_F_2009,
      BR_F_2010,
      BR_F_2011,
      BR_F_2012,
      BR_F_2013,
      BR_F_2014,
      BR_F_2015,
      BR_F_2016,
      BR_F_2017,
      BR_F_2018)

write.csv(data, "C:/Users/Carlos Garcia Filho/Desktop/Comparativo - Brasil, Argentina e Colômbia/data_final.csv", row.names = FALSE)
