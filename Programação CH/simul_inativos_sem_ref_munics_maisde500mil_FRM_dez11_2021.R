library(readr)
library(dplyr)
library(foreach)
library(data.table)

lista.novos.aposentados <- readRDS("C:/Users/Diogo/Desktop/Ipea/Simulações/Programação CH/simul_novos_inativos/lista.novos_aposentados.rds")

# Fun??es ----------------------------------------------------------------------
sintetico <- function(n,y) {set.seed(124) 
  rbinom(1, n, y)}

mortes <- function(inativos_ano){
  #CALCULANDO O N?MERO DE SERVIDORES QUE DEVEM MORRER EM ano
  inativos_ano_agrup <- group_by(inativos_ano, idade, genero)
  inativos_ano_agrup <- summarise(inativos_ano_agrup, qtde = n()) 
  
  inativos_ano_agrup <- left_join(inativos_ano_agrup, tabua, by.x = idade, by.y = genero)
  inativos_ano_agrup<- inativos_ano_agrup[complete.cases(inativos_ano_agrup), ]
  
  c <- inativos_ano_agrup$qtde
  d <- inativos_ano_agrup$mortal_0
  
  inativos_ano_agrup$mortos <- foreach(i = c, j = d) %do% {sintetico(i,j)}
  inativos_ano_agrup$mortos <- as.numeric(inativos_ano_agrup$mortos)
  mortos_est <- sum(inativos_ano_agrup$mortos, na.rm =TRUE)
  
  # DEFININDO QUEM, DE FATO, VAI MORRER EM ano ENTRE OS inativos
  e <- inativos_ano_agrup$genero
  f <- inativos_ano_agrup$idade
  h <- 1:nrow(inativos_ano_agrup)
  
  df_list <- list()
  filtering <- function(i,j) {filter(inativos_ano, genero==i & idade ==j)}
  df_list <- foreach(i = e, j = f) %do% {filtering(i,j)}
  
  creeping_death <- function(i) {if (inativos_ano_agrup$mortos[i] == 0) { df_list[[i]]}
    else {head(df_list[[i]], - inativos_ano_agrup$mortos[i])}}
  df_list <- foreach(i = h) %do% {creeping_death(i)}
  
  inativos_sobrev_ano <<- rbindlist(df_list)
  mortos_ano <<- setdiff(inativos_ano, inativos_sobrev_ano)
}
  
criar.pensionistas.sinteticos <- function(mortos_ano){
  if(nrow(mortos_ano)>0) {  
  # CALCULANDO O NUMERO DE PENSOES A SEREM INSTITUIDAS EM ano 
  mortos_ano_agrup <- group_by(mortos_ano, idade, genero)
  mortos_ano_agrup <- summarise(mortos_ano_agrup, qtde = n()) 
  
  mortos_ano_agrup <- left_join(mortos_ano_agrup, pens_prob, by.x = "idade", by.y = "genero")
  mortos_ano_agrup$p_pens <- as.numeric(mortos_ano_agrup$p_pens)
  mortos_ano_agrup<- mortos_ano_agrup[complete.cases(mortos_ano_agrup), ]
  
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
  novos_pensionistas_M <- filter(instituidores_ano, genero == "M")
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
  
  novos_pensionistas_F <- filter(instituidores_ano, genero == "F")
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

  return(novos_pensionistas_ano)
  } else 
  {novos_pensionistas_ano <<- mortos_ano
  instituidores_ano <<- mortos_ano}
}

atualizar.inativos <- function(inativos_sobrev_ano, novos_aposentados_ano){
  inativos_ano <- full_join(inativos_sobrev_ano, novos_aposentados_ano) %>% 
    mutate(idade = idade+1)
  rm(inativos_sobrev_ano, novos_aposentados_ano)
  
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
tabua_m <- read.csv2("C:/Users/Diogo/Desktop/Ipea/Simulações/tabua_nm_mul.csv")
tabua_h <- read.csv2("C:/Users/Diogo/Desktop/Ipea/Simulações/tabua_nm_hom.csv")
tabua_h$genero <- "M"
tabua_m$genero <- "F"
tabua <- rbind(tabua_h,tabua_m)
rm(tabua_h,tabua_m)
names(tabua)[1] <- "idade"
tabua <- select(tabua, genero, idade, mortal_0)

mortos_2021 <- mortes(inativos_2021)
inativos_sobrev_2021 <- inativos_sobrev_ano

# definindo os pensionistas em 2021 
pens_prob <- read.csv2("C:/Users/Diogo/Desktop/Ipea/Simulações/pensionistas_prob.csv")
names(pens_prob)[1] <- "idade" 
pens_prob <- select(pens_prob, idade, idade_conj, p_pens, genero)
pens_prob$p_pens <- as.numeric(pens_prob$p_pens)

novos_pensionistas_dinat_2021 <- criar.pensionistas.sinteticos(mortos_2021)

#ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
novos_aposentados_2021 <- lista.novos.aposentados[[2]]
inativos_2022 <- full_join(inativos_sobrev_2021,novos_aposentados_2021)
rm(inativos_sobrev_2021, novos_aposentados_2021)
inativos_2022$idade <- inativos_2022$idade+1

# 2022 at? 2057 ---------------------------------------------------------------
anos <- 2022:2057
lista.inativos <- vector(mode = "list", length = length(anos))
lista.mortos <- vector(mode = "list", length = length(anos))
lista.novos_pensionistas_dinat <- vector(mode = "list", length = length(anos))
for(i in 1:length(anos)){
  if(i == 1){lista.inativos[[1]] <- inativos_2022}
  mortes(lista.inativos[[i]])
  lista.mortos[[i]] <- mortos_ano
  
  #CRIANDO OS NOVOS PENSIONISTAS "SINT?TICOS"
  lista.novos_pensionistas_dinat[[i]] <- criar.pensionistas.sinteticos(mortos_ano)
  
  #ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
  if(i < length(anos)){
    lista.inativos[[i+1]] <- atualizar.inativos(inativos_sobrev_ano, lista.novos.aposentados[[i+3]])
    
    }
}
names(lista.mortos) <- anos

lista.inativos <- c(list(inativos_2021), lista.inativos)
names(lista.inativos) <- c(2021, anos)

lista.novos_pensionistas_dinat <- c(list(novos_pensionistas_dinat_2021), lista.novos_pensionistas_dinat)
names(lista.novos_pensionistas_dinat) <- c(2021, anos)

# 2058 at? 2080 ---------------------------
atualizar.inativos.sem.fluxo <- function(inativos_sobrev_ano){
  inativos_ano <- mutate(inativos_sobrev_ano, idade = idade+1)
  rm(inativos_sobrev_ano)
  
  set.seed(42)
  rows <- sample(nrow(inativos_ano))
  inativos_ano <- inativos_ano[rows, ]
  return(inativos_ano)}

anos <- 2058:2080
lista.inativos1 <- vector(mode = "list", length = length(anos))
lista.mortos1 <- vector(mode = "list", length = length(anos))
lista.novos_pensionistas_dinat1 <- vector(mode = "list", length = length(anos))

inativos_2058 <- lista.inativos[[36]]
inativos_2058 <- mutate(inativos_2058, idade = idade+1)

for(i in 1:length(anos)){
  if(i == 1){lista.inativos1[[1]] <- inativos_2058}
  mortes(lista.inativos1[[i]])
  lista.mortos1[[i]] <- mortos_ano
  
  #CRIANDO OS NOVOS PENSIONISTAS "SINT?TICOS"
  lista.novos_pensionistas_dinat1[[i]] <- criar.pensionistas.sinteticos(instituidores_ano)
  
  #ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
  if(i < length(anos)){
    lista.inativos1[[i+1]] <- atualizar.inativos.sem.fluxo(inativos_sobrev_ano)
  }
}

names(lista.inativos1) <- anos
names(lista.mortos1) <- anos
names(lista.novos_pensionistas_dinat1) <- anos

lista.inativos <- c(lista.inativos, lista.inativos1)
lista.novos_pensionistas_dinat <- c(lista.novos_pensionistas_dinat, lista.novos_pensionistas_dinat1)

saveRDS(lista.inativos, "C:/Users/Diogo/Desktop/Ipea/Simulações/Programação CH/simul_inativos/lista.inativos.rds")
saveRDS(lista.novos_pensionistas_dinat, "C:/Users/Diogo/Desktop/Ipea/Simulações/Programação CH/simul_novos_pensionistas_dinat/lista.novos_pensionistas_dinat.rds")
