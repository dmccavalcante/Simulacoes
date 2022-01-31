library(readr)
library(dplyr)
library(foreach)
library(data.table)
setwd("C:/Users/Diogo/Desktop/Ipea/Simulações")

lista.novos.pensionistas <- readRDS("C:/Users/Diogo/Desktop/Ipea/Simulações/Programação CH/simul_novos_pensionistas/lista.novos_pensionistas.rds")
lista.novos.pensionistas.dinat <- readRDS("C:/Users/Diogo/Desktop/Ipea/Simulações/Programação CH/simul_novos_pensionistas_dinat/lista.novos_pensionistas_dinat.rds")

# Fun??es ----------------------------------------------------------------------
sintetico <- function(n,y) {set.seed(124) 
  rbinom(1, n, y)}

mortes <- function(ativos_ano){
  #CALCULANDO O N?MERO DE SERVIDORES QUE DEVEM MORRER EM ano
  ativos_ano_agrup <- group_by(ativos_ano, idade, genero)
  ativos_ano_agrup <- summarise(ativos_ano_agrup, qtde = n()) 
  
  ativos_ano_agrup <- left_join(ativos_ano_agrup, tabua, by.x = idade, by.y = genero)
  ativos_ano_agrup <- ativos_ano_agrup[complete.cases(ativos_ano_agrup), ]
  
  c <- ativos_ano_agrup$qtde
  d <- ativos_ano_agrup$mortal_0
  
  ativos_ano_agrup$mortos <- foreach(i = c, j = d) %do% {sintetico(i,j)}
  ativos_ano_agrup$mortos <- as.numeric(ativos_ano_agrup$mortos)
  mortos_est <- sum(ativos_ano_agrup$mortos, na.rm =TRUE)
  
  # DEFININDO QUEM, DE FATO, VAI MORRER EM ano ENTRE OS ATIVOS
  e <- ativos_ano_agrup$genero
  f <- ativos_ano_agrup$idade
  h <- 1:nrow(ativos_ano_agrup)
  
  df_list <- list()
  filtering <- function(i,j) {filter(ativos_ano, genero==i & idade ==j)}
  df_list <- foreach(i = e, j = f) %do% {filtering(i,j)}
  
  creeping_death <- function(i) {if (ativos_ano_agrup$mortos[i] == 0) { df_list[[i]]}
    else {head(df_list[[i]], - ativos_ano_agrup$mortos[i])}}
  df_list <- foreach(i = h) %do% {creeping_death(i)}
  
  sobrev_ano <<- rbindlist(df_list)
  mortos_ano <<- setdiff(ativos_ano, sobrev_ano)
}

atualizar.pensionistas <- function(sobrev_ano, novos_pensionistas_ano,
                                   novos_pensionistas_dinat_ano){
  pensionistas_ano <- full_join(sobrev_ano, novos_pensionistas_ano)
  pensionistas_ano <- full_join(pensionistas_ano, novos_pensionistas_dinat_ano) %>% 
    mutate(idade = idade+1)
  rm(sobrev_ano, novos_pensionistas_ano,  novos_pensionistas_dinat_ano)
  
  set.seed(42)
  rows <- sample(nrow(pensionistas_ano))
  pensionistas_ano <- pensionistas_ano[rows, ]
  return(pensionistas_ano)
}

# 2021 -------------------------------------------------------------------------
# TRANSFORMANDO O ANO BASE DOS DADOS DE 2020 PARA 2021
pensionistas_2021 <- lista.novos.pensionistas[[1]]
pensionistas_2021 <- mutate(pensionistas_2021, idade=idade+1)

set.seed(42)
rows <- sample(nrow(pensionistas_2021))
pensionistas_2021 <- pensionistas_2021[rows, ]
rm(rows)

# CALCULANDO O NUMERO DE SERVIDORES QUE DEVEM MORRER EM 2021
tabua_m <- read.csv2("tabua_nm_mul.csv")
tabua_h <- read.csv2("tabua_nm_hom.csv")
tabua_h$genero <- "M"
tabua_m$genero <- "F"
tabua <- rbind(tabua_h,tabua_m)
rm(tabua_h,tabua_m)
names(tabua)[1] <- "idade"
tabua <- select(tabua, genero, idade, mortal_0)

mortes(pensionistas_2021)
mortos_2021 <- mortos_ano
pensionistas_sobrev_2021 <- sobrev_ano

#ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
novos_pensionistas_2021 <- lista.novos.pensionistas[[2]]
novos_pensionistas_dinat_2021 <- lista.novos.pensionistas.dinat[[1]]

pensionistas_2022 <- full_join(pensionistas_sobrev_2021, novos_pensionistas_2021)
pensionistas_2022 <- full_join(pensionistas_2022, novos_pensionistas_dinat_2021)

rm(pensionistas_sobrev_2021, novos_pensionistas_2021, novos_pensionistas_dinat_2021)
pensionistas_2022$idade <- pensionistas_2022$idade+1

# 2022 at? 2060 ----------------------------------------------------------------
anos <- 2022:(2022+length(lista.novos.pensionistas))
lista.pensionistas <- vector(mode = "list", length = length(anos))
lista.mortos <- vector(mode = "list", length = length(anos))
for(i in 1:length(anos)){
  if(i == 1){lista.pensionistas[[1]] <- pensionistas_2022}
  mortes(lista.pensionistas[[i]])
  lista.mortos[[i]] <- mortos_ano
  
    #ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
  if(i < length(anos)){
    lista.pensionistas[[i+1]] <- atualizar.pensionistas(sobrev_ano, lista.novos.pensionistas[[i]],
                                                        lista.novos.pensionistas.dinat[[i]])}
}
names(lista.pensionistas) <- anos
names(lista.mortos) <- anos

# 2061 at? 2090 ----------------------------------------------------------------
atualizar.pensionistas.sem.fluxo <- function(sobrev_ano){
  pensionistas_ano <- mutate(sobrev_ano, idade = idade+1)

  set.seed(42)
  rows <- sample(nrow(pensionistas_ano))
  pensionistas_ano <- pensionistas_ano[rows, ]
  return(pensionistas_ano)}

anos <- (2022+length(lista.novos.pensionistas)+1):2090
lista.pensionistas1 <- vector(mode = "list", length = length(anos))
lista.mortos1 <- vector(mode = "list", length = length(anos))

pensionistas__apos_sem_fluxo <- lista.pensionistas[[length(lista.pensionistas)]]

for(i in 1:length(anos)){
  if(i == 1){lista.pensionistas1[[1]] <- pensionistas__apos_sem_fluxo}
  mortes(lista.pensionistas1[[i]])
  lista.mortos1[[i]] <- mortos_ano
  
    #ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
  if(i < length(anos)){
    lista.pensionistas1[[i+1]] <- atualizar.pensionistas.sem.fluxo(sobrev_ano)
  }
}
names(lista.pensionistas1) <- anos
names(lista.mortos1) <- anos

lista.pensionistas <- c(list(pensionistas_2021), lista.pensionistas, lista.pensionistas1)
names(lista.pensionistas) <- 2021:2090

saveRDS(lista.pensionistas, "simul_pensionistas/lista.pensionistas.rds")
