library(readr)
library(dplyr)
library(foreach)
library(data.table)

lista.novos.pensionistas <- readRDS("C:/Users/Diogo/Desktop/Ipea/Simulações/Programação CH/simul_novos_pensionistas/lista.novos_pensionistas.rds")
lista.novos.pensionistas.dinat <- readRDS("C:/Users/Diogo/Desktop/Ipea/Simulações/Programação CH/simul_novos_pensionistas_dinat/lista.novos_pensionistas_dinat.rds")

# Funções ----------------------------------------------------------------------
sintetico <- function(n,y) {set.seed(124) 
  rbinom(1, n, y)}

mortes <- function(pensionistas_ano){
  #CALCULANDO O NUMERO DE PENSIONISTAS QUE DEVEM MORRER EM ano
  pensionistas_ano_agrup <- group_by(pensionistas_ano, idade, genero)
  pensionistas_ano_agrup <- summarise(pensionistas_ano_agrup, qtde = n()) 
  
  pensionistas_ano_agrup <- left_join(pensionistas_ano_agrup, tabua, by.x = idade, by.y = genero)
  pensionistas_ano_agrup<- pensionistas_ano_agrup[complete.cases(pensionistas_ano_agrup), ]
  
  c <- pensionistas_ano_agrup$qtde
  d <- pensionistas_ano_agrup$mortal_0
  
  pensionistas_ano_agrup$mortos <- foreach(i = c, j = d) %do% {sintetico(i,j)}
  pensionistas_ano_agrup$mortos <- as.numeric(pensionistas_ano_agrup$mortos)
  mortos_est <- sum(pensionistas_ano_agrup$mortos, na.rm =TRUE)
  
  # DEFININDO QUEM, DE FATO, VAI MORRER EM ano ENTRE OS pensionistas
  e <- pensionistas_ano_agrup$genero
  f <- pensionistas_ano_agrup$idade
  h <- 1:nrow(pensionistas_ano_agrup)
  
  df_list <- list()
  filtering <- function(i,j) {filter(pensionistas_ano, genero==i & idade ==j)}
  df_list <- foreach(i = e, j = f) %do% {filtering(i,j)}
  
  creeping_death <- function(i) {if (pensionistas_ano_agrup$mortos[i] == 0) { df_list[[i]]}
    else {head(df_list[[i]], - pensionistas_ano_agrup$mortos[i])}}
  df_list <- foreach(i = h) %do% {creeping_death(i)}
  
  pensionistas_sobrev_ano <<- rbindlist(df_list)
  mortos_ano <<- setdiff(pensionistas_ano, pensionistas_sobrev_ano)}
  
# pensionistas_sobrev_ano$id <- as.double(pensionistas_sobrev_ano$id)

atualizar.pensionistas <- function(pensionistas_sobrev_ano, novos_pensionistas_ano,
                                   novos_pensionistas_dinat_ano){
  pensionistas_ano <- full_join(pensionistas_sobrev_ano, novos_pensionistas_ano)
  pensionistas_ano <- full_join(pensionistas_ano, novos_pensionistas_dinat_ano) %>% 
    mutate(idade = idade+1)
  rm(pensionistas_sobrev_ano, novos_pensionistas_ano,  novos_pensionistas_dinat_ano)
  
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
tabua_m <- read.csv2("C:/Users/Diogo/Desktop/Ipea/Simulações/tabua_nm_mul.csv")
tabua_h <- read.csv2("C:/Users/Diogo/Desktop/Ipea/Simulações/tabua_nm_hom.csv")
tabua_h$genero <- "M"
tabua_m$genero <- "F"
tabua <- rbind(tabua_h,tabua_m)
rm(tabua_h,tabua_m)
names(tabua)[1] <- "idade"
tabua <- select(tabua, genero, idade, mortal_0)

pensionistas_2021_agrup <- group_by(pensionistas_2021, idade, genero)
pensionistas_2021_agrup <- summarise(pensionistas_2021_agrup, qtde = n()) 

pensionistas_2021_agrup <- left_join(pensionistas_2021_agrup, tabua, by.x = idade, by.y = genero)
pensionistas_2021_agrup <- pensionistas_2021_agrup[complete.cases(pensionistas_2021_agrup), ]

c <- pensionistas_2021_agrup$qtde
d <- pensionistas_2021_agrup$mortal_0

pensionistas_2021_agrup$mortos <- foreach(i = c, j = d) %do% {sintetico(i,j)}
pensionistas_2021_agrup$mortos <- as.numeric(pensionistas_2021_agrup$mortos)
mortos_est <- sum(pensionistas_2021_agrup$mortos)

# DEFININDO QUEM, DE FATO, VAI MORRER EM 2021 ENTRE OS pensionistas
e <- pensionistas_2021_agrup$genero
f <- pensionistas_2021_agrup$idade
h <- 1:nrow(pensionistas_2021_agrup)

df_list <- list()
filtering <- function(i,j) {filter(pensionistas_2021, genero==i & idade ==j)}
df_list <- foreach(i = e, j = f) %do% {filtering(i,j)}

creeping_death <- function(i) {if (pensionistas_2021_agrup$mortos[i] == 0) {  df_list[[i]]} 
  else {head(df_list[[i]], -pensionistas_2021_agrup$mortos[i])}}

df_list <- foreach(i = h) %do% {creeping_death(i)}

pensionistas_sobrev_2021 <- rbindlist(df_list)
rm(df_list, pensionistas_2021_agrup, c, d, e, f, h, i, j, mortos_est)

mortos_2021 <- setdiff(pensionistas_2021, pensionistas_sobrev_2021)

#ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
novos_pensionistas_2021 <- lista.novos.pensionistas[[2]]
novos_pensionistas_dinat_2021 <- lista.novos.pensionistas.dinat[[1]]

pensionistas_2022 <- full_join(pensionistas_sobrev_2021,novos_pensionistas_2021)
pensionistas_2022 <- full_join(pensionistas_2022,novos_pensionistas_dinat_2021)

# pensionistas_2022[19] <- NULL

rm(pensionistas_sobrev_2021, novos_pensionistas_2021, novos_pensionistas_dinat_2021)
pensionistas_2022$idade <- pensionistas_2022$idade+1

# 2022 até 2060 ---------------------------------------------------------------
anos <- 2022:2060
lista.pensionistas <- vector(mode = "list", length = length(anos))
lista.mortos <- vector(mode = "list", length = length(anos))

for(i in 1:length(anos)){
  if(i == 1){lista.pensionistas[[1]] <- pensionistas_2022}
  mortes(lista.pensionistas[[i]])
  lista.mortos[[i]] <- mortos_ano
  
    #ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
  if(i < length(anos)){
    lista.pensionistas[[i+1]] <- atualizar.pensionistas(pensionistas_sobrev_ano, lista.novos.pensionistas[[i]],
                                                        lista.novos.pensionistas.dinat[[i]])}
}
names(lista.pensionistas) <- anos
names(lista.mortos) <- anos

# for(i in 1:length(anos)){
#   write.csv2(lista.pensionistas[[i]], paste0("D:/FRM_munics_500mil_e_mais/simul_pensionistas/pensionistas_munics_500mil_e_mais_", anos[i],".csv"))}
# 
# write.csv2(pensionistas_2021, "D:/FRM_munics_500mil_e_mais/simul_pensionistas/pensionistas_munics_500mil_e_mais_2021.csv")

# 2061 até 2090 ----------------------------------------------------------------
atualizar.pensionistas.sem.fluxo <- function(pensionistas_sobrev_ano){
  pensionistas_ano <- mutate(pensionistas_sobrev_ano, idade = idade+1)
  rm(pensionistas_sobrev_ano)
  
  set.seed(42)
  rows <- sample(nrow(pensionistas_ano))
  pensionistas_ano <- pensionistas_ano[rows, ]
  return(pensionistas_ano)}

anos <- 2061:2090
lista.pensionistas1 <- vector(mode = "list", length = length(anos))
lista.mortos1 <- vector(mode = "list", length = length(anos))

pensionistas_2060 <- lista.pensionistas[[39]]

for(i in 1:length(anos)){
  if(i == 1){lista.pensionistas1[[1]] <- pensionistas_2060}
  mortes(lista.pensionistas1[[i]])
  lista.mortos1[[i]] <- mortos_ano
  
    #ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
  if(i < length(anos)){
    lista.pensionistas1[[i+1]] <- atualizar.pensionistas.sem.fluxo(pensionistas_sobrev_ano)
  }
}
names(lista.pensionistas1) <- anos
names(lista.mortos1) <- anos

# for(i in 1:length(anos)){
#   write.csv2(lista.pensionistas1[[i]], paste0("D:/FRM_munics_500mil_e_mais/simul_pensionistas/pensionistas_munics_500mil_e_mais_", anos[i],".csv"))
# }
lista.pensionistas <- c(list(pensionistas_2021), lista.pensionistas, lista.pensionistas1)
names(lista.pensionistas) <- 2021:2090

saveRDS(lista.pensionistas, "C:/Users/Diogo/Desktop/Ipea/Simulações/Programação CH/simul_pensionistas/lista.pensionistas.rds")
