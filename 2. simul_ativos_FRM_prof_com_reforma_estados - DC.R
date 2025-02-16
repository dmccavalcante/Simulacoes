library(purrr)
library(readr)
library(dplyr)
library(foreach)
library(tidyverse)
library(data.table)
setwd("C:/Users/Diogo/Desktop/Ipea/Simulações/microdados_finais")
taxa_atualizacao_salarial <- 1.015
taxa_reposicao <- 0.75

# Funções ----------------------------------------------------------------------
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
  
  ativos_sobrev_ano <<- rbindlist(df_list)
  mortos_ano <<- setdiff(ativos_ano, ativos_sobrev_ano)
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
  
  novos_pensionistas_ano <- mutate(novos_pensionistas_ano, rem_med_nom1 = rem_med_nom/((taxa_atualizacao_salarial)^tempo_empreg))
  novos_pensionistas_ano <- mutate(novos_pensionistas_ano, rem_med_nom = (rem_med_nom+rem_med_nom1)/2)
  novos_pensionistas_ano$rem_med_nom1 <- NULL
  
  novos_pensionistas_ano <- mutate(novos_pensionistas_ano, tempo_empreg_ad = tempo_empreg - 20) 
  novos_pensionistas_ano$rem_med_nom <- ifelse(novos_pensionistas_ano$tempo_empreg_ad <= 0, 0.6*novos_pensionistas_ano$rem_med_nom, 
                                               (0.6+0.02*novos_pensionistas_ano$tempo_empreg_ad)*novos_pensionistas_ano$rem_med_nom)
  novos_pensionistas_ano$tempo_empreg_ad <- NULL
  } else {
    novos_pensionistas_ano <- mortos_ano
    instituidores_ano <<- mortos_ano
    }
  return(novos_pensionistas_ano)
}

elegiveis.voluntarios <- function(sobrev_n_compuls_ano, ano){
  # homens safra1
  # 1 - Ped?gio
  eleg_com_safra1_hha <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm < 2004  & idade >= 60 & tempo1 >= tempo3 & tempo_empreg >20 & tempo1 >= 35)
  eleg_com_safra1_hhb <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm < 2004  & idade >= 55 & tempo2 >= tempo4 & tempo_empreg >20 & tempo2 >= 30)
  eleg_com_safra1_hh_pedag <- full_join(eleg_com_safra1_hhb, eleg_com_safra1_hha)
  
  #2 - Pontos
  if(ano <= 2027){
    eleg_com_safra1_hhc <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm < 2004  & idade >= 65 & tempo1 >= 35 & tempo_empreg >20 & pontos1 >= ano - 1923)
    eleg_com_safra1_hhd <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm < 2004  & idade >= 60 & tempo2 >= 30 & tempo_empreg >20 & pontos2 >= ano - 1928)
  } else{
    eleg_com_safra1_hhc <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm < 2004  & idade >= 65 & tempo1 >= 35 & tempo_empreg >20 & pontos1 >= 105)
    eleg_com_safra1_hhd <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm < 2004  & idade >= 60 & tempo2 >= 30 & tempo_empreg >20 & pontos2 >= 100)
  }
  eleg_com_safra1_hh_pontos <- full_join(eleg_com_safra1_hhc, eleg_com_safra1_hhd)  
  
  # 3 - Pontos + Ped?gio
  eleg_com_safra1_hh <- full_join(eleg_com_safra1_hh_pontos, eleg_com_safra1_hh_pedag)
  
  # mulheres_com_ref safra1
  # 1 ? Ped?gio
  eleg_com_safra1_mma <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm < 2004  & idade>=57 & tempo1 >= tempo3 & tempo_empreg >20 & tempo1 >= 30)
  eleg_com_safra1_mmb <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm < 2004  & idade>=52 & tempo2 >= tempo4 & tempo_empreg >20 & tempo2 >= 25)
  eleg_com_safra1_mm_pedag <- full_join(eleg_com_safra1_mmb, eleg_com_safra1_mma)  
  
  if(ano <= 2029){
    eleg_com_safra1_mmc <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm < 2004  & idade >= 62 & tempo1 >= 30 & tempo_empreg >20 & pontos1 >= ano - 1933)
    eleg_com_safra1_mmd <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm < 2004  & idade >= 57 & tempo2 >= 25 & tempo_empreg >20 & pontos2 >= ano - 1938)
  } else{
    eleg_com_safra1_mmc <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm < 2004  & idade >= 62 & tempo1 >= 30 & tempo_empreg >20 & pontos1 >= 100)
    eleg_com_safra1_mmd <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm < 2004  & idade >= 57 & tempo2 >= 25 & tempo_empreg >20 & pontos2 >= 92)
  }
  eleg_com_safra1_mm_pontos <- full_join(eleg_com_safra1_mmc, eleg_com_safra1_mmd)
  
  # 3 -Pontos +ped?gio
  eleg_com_safra1_mm <- full_join(eleg_com_safra1_mm_pontos, eleg_com_safra1_mm_pedag)
  
  # eleg com safra1
  eleg_com_safra1_ano <<- full_join(eleg_com_safra1_mm, eleg_com_safra1_hh)  
  
  # eleg com reforma safra 2
  # Homens
  # 1 - Ped?gio
  eleg_com_safra2_hha <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm >= 2004  & idade >= 60 & tempo1 >= tempo3 & tempo_empreg >20 & tempo1 >= 35)
  eleg_com_safra2_hhb <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm >= 2004  & idade >= 55 & tempo2 >= tempo4 & tempo_empreg >20 & tempo2 >= 30)
  
  eleg_com_safra2_hh_pedag <- full_join(eleg_com_safra2_hhb, eleg_com_safra2_hha)
  
  #2 - Pontos
  if(ano <= 2027){
    eleg_com_safra2_hhc <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm >= 2004 & idade >= 62 & tempo1 >= 35 & tempo_empreg > 20 & pontos1 >= ano - 1923)
    eleg_com_safra2_hhd <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm >= 2004 & idade >= 57 & tempo2 >= 30 & tempo_empreg > 20 & pontos2 >= ano - 1928)
  } else{
    eleg_com_safra2_hhc <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm >= 2004 & idade >= 62 & tempo1 >= 35 & tempo_empreg >20 & pontos1 >= 105)
    eleg_com_safra2_hhd <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm >= 2004 & idade >= 57 & tempo2 >= 30 & tempo_empreg >20 & pontos2 >= 100)
  }
  if(ano %in% c(2020, 2021)){
    eleg_com_safra2_hhc <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm >= 2004 & idade >= 61 & tempo1 >= 35 & tempo_empreg > 20 & pontos1 >= ano - 1923)
    eleg_com_safra2_hhd <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm >= 2004 & idade >= 56 & tempo2 >= 30 & tempo_empreg > 20 & pontos2 >= ano - 1928)
  }
  eleg_com_safra2_hh_pontos <- full_join(eleg_com_safra2_hhc, eleg_com_safra2_hhd)
  # 3 - Regra permanente
  eleg_com_safra2_hh_perm1 <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm >= 2004 & idade >= 65 & tempo1 >= 25 & tempo_empreg >= 10)
  eleg_com_safra2_hh_perm2 <- filter(sobrev_n_compuls_ano, genero == "M", ano_adm >= 2004 & idade >= 60 & tempo2 >= 25 & tempo_empreg >= 10)
  eleg_com_safra2_hh_perm <- full_join(eleg_com_safra2_hh_perm1, eleg_com_safra2_hh_perm2)
  
  #4 - todos os eleg?veis
  eleg_com_safra2_hh <- full_join(eleg_com_safra2_hh_perm, eleg_com_safra2_hh_pedag)
  eleg_com_safra2_hh <- full_join(eleg_com_safra2_hh, eleg_com_safra2_hh_pontos)
  
  #Mulheres
  # 1 - pedagio
  eleg_com_safra2_mma <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm >= 2004  & idade>=57 & tempo1 >= tempo3 & tempo_empreg >20 & tempo1 >= 30)
  eleg_com_safra2_mmb <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm >= 2004  & idade>=52 & tempo2 >= tempo4 & tempo_empreg >20 & tempo2 >= 25)
  eleg_com_safra2_mm_pedag <- full_join(eleg_com_safra2_mmb, eleg_com_safra2_mma)
  
  #2 - Pontos
  if(ano <= 2032){
    eleg_com_safra2_mmc <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm >= 2004 & idade >= 56 & tempo1 >= 30 & tempo_empreg > 20 & pontos1 >= ano - 1933)
  } else{
    eleg_com_safra2_mmc <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm >= 2004 & idade >= 56 & tempo1 >= 30 & tempo_empreg > 20 & pontos1 >= 100)
  }
  if(ano <= 2029){
    eleg_com_safra2_mmd <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm >= 2004 & idade >= 51 & tempo2 >= 25 & tempo_empreg > 20 & pontos2 >= ano - 1938)
  } else{
    eleg_com_safra2_mmd <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm >= 2004 & idade >= 51 & tempo2 >= 25 & tempo_empreg > 20 & pontos2 >= 92)
  }
  if(ano %in% c(2022,2023)){
    eleg_com_safra2_mmc <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm >= 2004  &idade>=57 & tempo1 >= 30 & tempo_empreg >20 & pontos1 >= ano - 1933)
    eleg_com_safra2_mmd <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm >= 2004  &idade>=52 & tempo2 >= 25 & tempo_empreg >20 & pontos2 >= ano - 1938)
  }
  eleg_com_safra2_mm_pontos <- full_join(eleg_com_safra2_mmc, eleg_com_safra2_mmd)
  
  # 3 - regra permanente
  eleg_com_safra2_mm_perm1 <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm >= 2004 & idade >= 62 & tempo1 >= 25 & tempo_empreg >= 10)
  eleg_com_safra2_mm_perm2 <- filter(sobrev_n_compuls_ano, genero == "F", ano_adm >= 2004 & idade >= 57 & tempo2 >= 25 & tempo_empreg >= 10)
  eleg_com_safra2_mm_perm <- full_join(eleg_com_safra2_mm_perm1, eleg_com_safra2_mm_perm2)
  
  # 4 - todas as eleg?veis
  eleg_com_safra2_mm <- full_join(eleg_com_safra2_mm_perm, eleg_com_safra2_mm_pedag)
  eleg_com_safra2_mm <- full_join(eleg_com_safra2_mm, eleg_com_safra2_mm_pontos)
  
  # fechando a safra 2
  eleg_com_safra2_ano <<- full_join(eleg_com_safra2_mm, eleg_com_safra2_hh)
}

aposentadoria.voluntaria <- function(eleg_com_safra1_ano, eleg_com_safra2_ano, compulsorios_ano){
  eleg_com_safra1_ano$probabilidade_estimada <- predict(probit_19_safra_1, eleg_com_safra1_ano, type = "response", se.fit = F)
  set.seed(42)
  eleg_com_safra1_ano$uniforme <- runif(nrow(eleg_com_safra1_ano))
  eleg_com_safra1_ano <- eleg_com_safra1_ano %>% mutate(inativo_estimado = ifelse(uniforme <= probabilidade_estimada, 1, 0))
  
  eleg_com_safra2_ano$probabilidade_estimada <- predict(probit_19_safra_2, eleg_com_safra2_ano, type = "response", se.fit = F)
  set.seed(42)
  eleg_com_safra2_ano$uniforme <- runif(nrow(eleg_com_safra2_ano))
  eleg_com_safra2_ano <- eleg_com_safra2_ano %>% mutate(inativo_estimado = ifelse(uniforme <= probabilidade_estimada, 1, 0))
  
  apos_vol_safra1_ano <- filter(eleg_com_safra1_ano, inativo_estimado %in% 1) %>% select(-c(probabilidade_estimada, uniforme, inativo_estimado))
  apos_vol_safra2_ano <- filter(eleg_com_safra2_ano, inativo_estimado %in% 1) %>% select(-c(probabilidade_estimada, uniforme, inativo_estimado))
  
  apos_vol_ano <- rbind(apos_vol_safra1_ano, apos_vol_safra2_ano)
  
  novos_aposentados_ano <<- rbind(apos_vol_ano, compulsorios_ano)
}

atualizar.ativos <- function(sobrev_n_compuls_ano, novos_aposentados_ano){
  ativos_ano <- anti_join(sobrev_n_compuls_ano, novos_aposentados_ano, by = "id") %>% 
    mutate(idade = idade+1, 
           tempo_empreg = tempo_empreg+1,
           rem_med_nom = rem_med_nom*taxa_atualizacao_salarial, 
           tempo1 = 1+tempo1, tempo2 = 1+tempo2, 
           tempo3 = 1+tempo3, tempo4 = 1+tempo4,
           pontos1 = 2+pontos1, pontos2 = 2+pontos2)
  
  set.seed(42)
  rows <- sample(nrow(ativos_ano))
  ativos_ano <- ativos_ano[rows, ]
  return(ativos_ano)
}

base.reposicao <- function(base_ano, alfa_reposicao, ano_admissao){
  set.seed(42)
  aux.reposicao <- base_ano[sample(nrow(base_ano), round(alfa_reposicao*nrow(base_ano))), ]
  aux.reposicao <- aux.reposicao %>% 
    mutate(ano_adm = ano_admissao, 
           idade = idade_adm,
           rem_med_nom = rem_med_nom/(taxa_atualizacao_salarial^tempo_empreg),
           tempo1 = 0, tempo2 = 0, pontos1 = 0, pontos2 = 0, 
           pedag1 = 0, pedag2 = 0, tempo3 = 0, tempo4 = 0,
           tempo_empreg = 0, inativo = 0)
  return(aux.reposicao)
}

# 2019 -------------------------------------------------------------------------
## SUBINDO OS DADOS DA TROPA, AJUSTANDO OS DADOS E ETC.
ativos_2019 <- readRDS("estados_com_reforma_2019.rds")

# TRANSFORMANDO O ANO BASE DOS DADOS DE 2019 PARA 2020
ativos_2020 <- ativos_2019 %>% 
  mutate(rem_med_nom = taxa_atualizacao_salarial*rem_med_nom,
         tempo_empreg = tempo_empreg+1, idade = 1+idade,
         tempo1 = 1+tempo1, tempo2 = 1+tempo2,
         tempo3 = 1+tempo3, tempo4 = 1+tempo4,
         pontos1 = 2+pontos1, pontos2 = 2+pontos2)

set.seed(42)
rows <- sample(nrow(ativos_2020))
ativos_2020 <- ativos_2020[rows, ]
rm(rows)

# CALCULANDO O N?MERO DE SERVIDORES QUE DEVEM MORRER EM 2020
tabua_m <- read.csv2("C:/Users/Diogo/Desktop/Ipea/Simulações/tabua_nm_mul.csv")
tabua_h <- read.csv2("C:/Users/Diogo/Desktop/Ipea/Simulações/tabua_nm_hom.csv")
tabua_h$genero <- "M"
tabua_m$genero <- "F"
tabua <- rbind(tabua_h,tabua_m)
rm(tabua_h,tabua_m)
names(tabua)[1] <- "idade"
tabua <- select(tabua, genero, idade, mortal_0)

pens_prob <- read.csv2("C:/Users/Diogo/Desktop/Ipea/Simulações/pensionistas_prob.csv")
names(pens_prob)[1] <- "idade" 
pens_prob <- select(pens_prob, idade, idade_conj, p_pens, genero)
pens_prob$p_pens <- as.numeric(pens_prob$p_pens)

# 2020 em diante ---------------------------------------------------------------
anos <- 2020:2055
lista.ativos <- vector(mode = "list", length = length(anos))
lista.mortos <- vector(mode = "list", length = length(anos))
lista.novos_pensionistas <- vector(mode = "list", length = length(anos))
lista.novos_aposentados <- vector(mode = "list", length = length(anos))
lista.compulsorios <- vector(mode = "list", length = length(anos))
lista.eleg_com_safra1 <- vector(mode = "list", length = length(anos))
lista.eleg_com_safra2 <- vector(mode = "list", length = length(anos))
for(i in 1:length(anos)){
  if(i == 1){lista.ativos[[1]] <- ativos_2020}
  mortes(lista.ativos[[i]])
  lista.mortos[[i]] <- mortos_ano
  
  # CRIANDO OS NOVOS PENSIONISTAS "SINT?TICOS"
  lista.novos_pensionistas[[i]] <- criar.pensionistas.sinteticos(instituidores_ano, anos[i])
  
  # DEFININDO OS SERVIDORES QUE SER?O APOSENTADOS COMPULSORIAMENTE
  lista.compulsorios[[i]] <- filter(ativos_sobrev_ano, idade >= 75)
  sobrev_n_compuls <- filter(ativos_sobrev_ano, idade < 75)
  
  # CALCULANDO O N?MERO DE NOVAS APOSENTADORIAS VOLUNT?RIAS EM 2021 
  elegiveis.voluntarios(sobrev_n_compuls, anos[i])
  lista.eleg_com_safra1[[i]] <- eleg_com_safra1_ano
  lista.eleg_com_safra2[[i]] <- eleg_com_safra2_ano
  
  # modelo
  if(i == 1){
    probit_19_safra_1 <- glm(inativo ~ idade + genero + rem_med_nom + tempo_expirado, family = binomial(link = "logit"), data = lista.eleg_com_safra1[[i]])
    probit_19_safra_2 <- glm(inativo ~ idade + genero + rem_med_nom + tempo_expirado, family = binomial(link = "logit"), data = lista.eleg_com_safra2[[i]])
  }
  
  # DEFININDO QUEM, DE FATO, VAI SE APOSENTAR VOLUNTARIAMENTE EM 2021
  aposentadoria.voluntaria(lista.eleg_com_safra1[[i]], lista.eleg_com_safra2[[i]], lista.compulsorios[[i]])
  lista.novos_aposentados[[i]] <- novos_aposentados_ano
  
  # ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
  if(i < length(anos)){
    lista.ativos[[i+1]] <- atualizar.ativos(sobrev_n_compuls, lista.novos_aposentados[[i]])
    reposicao <- base.reposicao(rbind(lista.novos_aposentados[[i]], lista.mortos[[i]]), taxa_reposicao, anos[i])
    lista.ativos[[i+1]] <- rbindlist(list(lista.ativos[[i+1]], reposicao))
  }
}
names(lista.ativos) <- anos
names(lista.mortos) <- anos
names(lista.novos_pensionistas) <- anos
names(lista.novos_aposentados) <- anos
names(lista.compulsorios) <- anos
names(lista.eleg_com_safra1) <- anos
names(lista.eleg_com_safra2) <- anos

# salvar dados -----------------------------------------------------------------
saveRDS(lista.ativos, paste0("comparativo/ativos_reposicao_", taxa_reposicao, ".rds"))
saveRDS(lista.novos_aposentados, paste0("comparativo/novos_aposentados_reposicao_", taxa_reposicao, ".rds"))
saveRDS(lista.novos_pensionistas, paste0("comparativo/novos_pensionistas_reposicao_", taxa_reposicao, ".rds"))
saveRDS(lista.novos_aposentados, paste0("comparativo/novos_aposentados_reposicao_", taxa_reposicao, ".rds"))
