library(readr)
library(dplyr)
library(foreach)
library(data.table)
setwd("C:/Users/Diogo/Desktop/Ipea/Simulações")
taxa_reposicao <- 0.00

lista.novos.aposentados <- readRDS(paste0("microdados_finais/comparativo/novos_aposentados_reposicao_", taxa_reposicao, ".rds"))

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

criar.pensionistas.sinteticos <- function(instituidores, ano = 0){
  if(nrow(mortos_ano)>0) {
    # CALCULANDO O N?MERO DE PENS?ES A SEREM INSTITU?DAS EM ano 
    mortos_ano_agrup <- group_by(mortos_ano, idade, genero)
    mortos_ano_agrup <- summarise(mortos_ano_agrup, qtde = n()) 
    
    mortos_ano_agrup <- left_join(mortos_ano_agrup, pens_prob, by.x = "idade", by.y = "genero")
    mortos_ano_agrup$p_pens <- as.numeric(mortos_ano_agrup$p_pens)
    if(ano == 2020){
      mortos_ano_agrup <- filter(mortos_ano_agrup, idade >= 18)
    } else{
      mortos_ano_agrup <- mortos_ano_agrup[complete.cases(mortos_ano_agrup), ]
    }
    
    c <- mortos_ano_agrup$qtde
    d <- mortos_ano_agrup$p_pens
    
    mortos_ano_agrup$instituidores <- foreach(i = c, j = d) %do% {sintetico(i,j)}
    mortos_ano_agrup$instituidores <- as.numeric(mortos_ano_agrup$instituidores)
    instituidores_est <- sum(mortos_ano_agrup$instituidores)
    
    # DEFININDO QUEM, DE FATO, VAI INSTITUIR AS PENS?ES
    e <- mortos_ano_agrup$genero
    f <- mortos_ano_agrup$idade
    h <- 1:nrow(mortos_ano_agrup)
    
    df_list <- list()
    filtering <- function(i,j) {filter(mortos_ano, genero==i & idade ==j)}
    df_list <- foreach(i = e, j = f) %do% {filtering(i,j)}
    
    creeping_death <- function(i) {if (mortos_ano_agrup$instituidores[i] == 0) {
      df_list[[i]]} else {head(df_list[[i]], -mortos_ano_agrup$instituidores[i])}}
    
    df_list <- foreach(i = h) %do% {creeping_death(i)}
    nao_instituidores_ano <- rbindlist(df_list)
    
    instituidores_ano <<- setdiff(mortos_ano, nao_instituidores_ano)
    
    novos_pensionistas_M <- filter(instituidores, genero == "M")
  novos_pensionistas_M$genero <- "F"
  novos_pensionistas_M20 <- filter(novos_pensionistas_M, idade<30)
  novos_pensionistas_M20$idade <- novos_pensionistas_M20$idade+1
  novos_pensionistas_M30 <- filter(novos_pensionistas_M, idade>=30 & idade <40)
  novos_pensionistas_M30$idade <- novos_pensionistas_M30$idade-2
  novos_pensionistas_M40 <- filter(novos_pensionistas_M, idade>=40 & idade < 50)
  novos_pensionistas_M40$idade <- novos_pensionistas_M40$idade-4
  novos_pensionistas_M50 <- filter(novos_pensionistas_M, idade>=50 & idade <60)
  novos_pensionistas_M50$idade <- novos_pensionistas_M50$idade-6
  novos_pensionistas_M60 <- filter(novos_pensionistas_M, idade>=60) 
  novos_pensionistas_M60$idade <- novos_pensionistas_M60$idade-8
  
  novos_pensionistas_M <- rbind(novos_pensionistas_M20, novos_pensionistas_M30, novos_pensionistas_M40, 
                                novos_pensionistas_M50, novos_pensionistas_M60)
  
  novos_pensionistas_F <- filter(instituidores, genero == "F")
  novos_pensionistas_F$genero <- "M"
  novos_pensionistas_F20 <- filter(novos_pensionistas_F, idade<30)
  novos_pensionistas_F20$idade <- novos_pensionistas_F20$idade+5
  novos_pensionistas_F30 <- filter(novos_pensionistas_F, idade>=30 & idade <40)
  novos_pensionistas_F30$idade <- novos_pensionistas_F30$idade+3
  novos_pensionistas_F40 <- filter(novos_pensionistas_F, idade>=40 & idade < 50)
  novos_pensionistas_F40$idade <- novos_pensionistas_F40$idade+2
  novos_pensionistas_F50 <- filter(novos_pensionistas_F, idade>=50 & idade <60)
  novos_pensionistas_F50$idade <- novos_pensionistas_F50$idade+2
  novos_pensionistas_F60 <- filter(novos_pensionistas_F, idade>=60) 
  novos_pensionistas_F60$idade <- novos_pensionistas_F60$idade+1
  
  novos_pensionistas_F <- rbind(novos_pensionistas_F20, novos_pensionistas_F30, novos_pensionistas_F40, 
                                novos_pensionistas_F50, novos_pensionistas_F60)
  
  novos_pensionistas_ano <- rbind(novos_pensionistas_M, novos_pensionistas_F)
  novos_pensionistas_ano <- mutate(novos_pensionistas_ano, rem_med_nom = 0.6*rem_med_nom) }
  else {
    novos_pensionistas_ano <- mortos_ano
    instituidores_ano <<- mortos_ano
  }
  return(novos_pensionistas_ano)
}

atualizar.inativos <- function(sobrev_ano, novos_aposentados_ano){
  inativos_ano <- full_join(sobrev_ano, novos_aposentados_ano) %>% 
    mutate(idade = idade+1)

  set.seed(42)
  rows <- sample(nrow(inativos_ano))
  inativos_ano <- inativos_ano[rows, ]
  return(inativos_ano)
}

# 2021 -------------------------------------------------------------------------
# TRANSFORMANDO O ANO BASE DOS DADOS DE 2020 PARA 2021
inativos_2021 <- lista.novos.aposentados[[1]]
inativos_2021 <- mutate(inativos_2021, idade=idade+1)

set.seed(42)
rows <- sample(nrow(inativos_2021))
inativos_2021 <- inativos_2021[rows, ]
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

pens_prob <- read.csv2("pensionistas_prob.csv")
names(pens_prob)[1] <- "idade" 
pens_prob <- select(pens_prob, idade, idade_conj, p_pens, genero)
pens_prob$p_pens <- as.numeric(pens_prob$p_pens)

mortes(inativos_2021)
mortos_2021 <- mortos_ano
inativos_sobrev_2021 <- sobrev_ano

# CALCULANDO O NUMERO DE PENSOES A SEREM INSTITUIDAS EM 2021
mortos_2021_agrup <- group_by(mortos_2021, idade, genero)
mortos_2021_agrup <- summarise(mortos_2021_agrup, qtde = n()) 

mortos_2021_agrup <- left_join(mortos_2021_agrup, pens_prob, by.x = "idade", by.y = "genero")
mortos_2021_agrup$p_pens <- as.numeric(mortos_2021_agrup$p_pens)
mortos_2021_agrup <- filter(mortos_2021_agrup, idade>= 18)

c <- mortos_2021_agrup$qtde
d <- mortos_2021_agrup$p_pens

mortos_2021_agrup$instituidores <- foreach(i = c, j = d) %do% {sintetico(i,j)}
mortos_2021_agrup$instituidores <- as.numeric(mortos_2021_agrup$instituidores)
instituidores_est <- sum(mortos_2021_agrup$instituidores)

# DEFININDO QUEM, DE FATO, VAI INSTITUIR AS PENSOES
e <- mortos_2021_agrup$genero
f <- mortos_2021_agrup$idade
h <- 1:nrow(mortos_2021_agrup)

df_list <- list()
filtering <- function(i,j) {filter(mortos_2021, genero==i & idade ==j)}
df_list <- foreach(i = e, j = f) %do% {filtering(i,j)}

creeping_death <- function(i) {if (mortos_2021_agrup$instituidores[i] == 0) {
  df_list[[i]]} else {head(df_list[[i]], -mortos_2021_agrup$instituidores[i])}}

df_list <- foreach(i = h) %do% {creeping_death(i)}
nao_instituidores_2021 <- rbindlist(df_list)

rm(df_list, mortos_2021_agrup, c, d, e, f, h, i, j, instituidores_est)

instituidores_2021 <- setdiff(mortos_2021, nao_instituidores_2021)

#CRIANDO OS NOVOS PENSIONISTAS "SINTETICOS"
novos_pensionistas_dinat_2021 <- criar.pensionistas.sinteticos(instituidores_2021)

#ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
novos_aposentados_2021 <- lista.novos.aposentados[[2]]
inativos_2022 <- full_join(inativos_sobrev_2021, novos_aposentados_2021)
rm(inativos_sobrev_2021, instituidores_2021, nao_instituidores_2021, novos_aposentados_2021)
inativos_2022$idade <- inativos_2022$idade + 1

# 2022 at? 2060 ---------------------------------------------------------------
anos <- 2022:(2022+length(lista.novos.aposentados))
lista.inativos <- vector(mode = "list", length = length(anos))
lista.mortos <- vector(mode = "list", length = length(anos))
lista.novos_pensionistas_dinat <- vector(mode = "list", length = length(anos))
for(i in 1:length(anos)){
  if(i == 1){lista.inativos[[1]] <- inativos_2022}
  mortes(lista.inativos[[i]])
  lista.mortos[[i]] <- mortos_ano
  
  #CRIANDO OS NOVOS PENSIONISTAS "SINT?TICOS"
  lista.novos_pensionistas_dinat[[i]] <- criar.pensionistas.sinteticos(instituidores_ano)
  
  #ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
  if(i < length(anos)){
    lista.inativos[[i+1]] <- atualizar.inativos(sobrev_ano, lista.novos.aposentados[[i]])
    }
}
names(lista.mortos) <- anos

lista.inativos <- c(list(inativos_2021), lista.inativos)
names(lista.inativos) <- c(2021, anos)

lista.novos_pensionistas_dinat <- c(list(novos_pensionistas_dinat_2021), lista.novos_pensionistas_dinat)
names(lista.novos_pensionistas_dinat) <- c(2021, anos)

# 2061 at? 2080 ----------------------------------------------------------------
atualizar.inativos.sem.fluxo <- function(sobrev_ano){
  inativos_ano <- mutate(sobrev_ano, idade = idade+1)

  set.seed(42)
  rows <- sample(nrow(inativos_ano))
  inativos_ano <- inativos_ano[rows, ]
  return(inativos_ano)}

anos <- (2022+length(lista.novos.aposentados)+1):2080
lista.inativos1 <- vector(mode = "list", length = length(anos))
lista.mortos1 <- vector(mode = "list", length = length(anos))
lista.novos_pensionistas_dinat1 <- vector(mode = "list", length = length(anos))

inativos_apos_sem_fluxo <- lista.inativos[[length(lista.inativos)]]

for(i in 1:length(anos)){
  if(i == 1){lista.inativos1[[1]] <- inativos_apos_sem_fluxo}
  mortes(lista.inativos1[[i]])
  lista.mortos1[[i]] <- mortos_ano
  
  #CRIANDO OS NOVOS PENSIONISTAS "SINT?TICOS"
  lista.novos_pensionistas_dinat1[[i]] <- criar.pensionistas.sinteticos(instituidores_ano)
  
  #ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
  if(i < length(anos)){
    lista.inativos1[[i+1]] <- atualizar.inativos.sem.fluxo(sobrev_ano)
  }
}
names(lista.inativos1) <- anos
names(lista.mortos1) <- anos
names(lista.novos_pensionistas_dinat1) <- anos

lista.inativos <- c(lista.inativos, lista.inativos1)
lista.novos_pensionistas_dinat <- c(lista.novos_pensionistas_dinat, lista.novos_pensionistas_dinat1)

saveRDS(lista.inativos, "simul_inativos/lista.inativos.rds")
saveRDS(lista.novos_pensionistas_dinat, "simul_novos_pensionistas_dinat/lista.novos_pensionistas_dinat.rds")
