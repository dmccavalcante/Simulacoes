library(tidyverse)
library(readr)
library(dplyr)
library(foreach)
library(data.table)
# Fun??es ----------------------------------------------------------------------
sintetico <- function(n,y) {set.seed(124) 
  rbinom(1, n, y)}

mortes <- function(ativos_ano){
  #CALCULANDO O N?MERO DE SERVIDORES QUE DEVEM MORRER EM ano
  ativos_ano_agrup <- group_by(ativos_ano, idade, genero)
  ativos_ano_agrup <- summarise(ativos_ano_agrup, qtde = n()) 
  
  ativos_ano_agrup <- left_join(ativos_ano_agrup, tabua, by.x = idade, by.y = genero)
  ativos_ano_agrup<- ativos_ano_agrup[complete.cases(ativos_ano_agrup), ]
  
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
  mortos_ano <<- setdiff(ativos_ano, ativos_sobrev_ano)}
  
criar.pensionistas.sinteticos <- function(mortos_anos){
  
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
  
  novos_pensionistas_ano$rem_med_nom <- ifelse(novos_pensionistas_ano$rem_med_nom<5845, 
                                               novos_pensionistas_ano$rem_med_nom, 5845+
                                                 0.7*(novos_pensionistas_ano$rem_med_nom-5845))
  return(novos_pensionistas_ano)
  } else 
  {novos_pensionistas_ano <<- mortos_ano
  instituidores_ano <<- mortos_ano}
}

elegiveis.voluntarios <- function(sobrev_n_compuls_ano){
  # A - safra 1 
  # A1- Homens 
  # A.1.1 - regra permanente
  
  hh_safra1_regperm1 <- filter(sobrev_n_compuls_ano, ano_adm <= 1998 & genero =="M"& idade >= 60 & tempo1 >= 35 & tempo_empreg >= 10)
  hh_safra1_regperm2 <- filter(sobrev_n_compuls_ano, ano_adm <= 1998 &genero =="M"& idade >= 55 & tempo2 >= 30 & tempo_empreg >= 10)
  
  hh_safra1_regperm <- full_join(hh_safra1_regperm1, hh_safra1_regperm2)
  rm(hh_safra1_regperm1, hh_safra1_regperm2)
  
  # 1.2 - EC 47
  
  hh_safra1_ec47a <- filter(sobrev_n_compuls_ano, ano_adm <= 1998 & genero =="M"& tempo1 >= 35 & tempo_empreg >= 25 & idade >= idade_aj1)
  hh_safra1_ec47b <- filter(sobrev_n_compuls_ano, ano_adm <= 1998 &genero =="M"& tempo2 >= 30 & tempo_empreg >= 25 & idade >= idade_aj2)
  
  hh_safra1_ec47 <- full_join(hh_safra1_ec47a, hh_safra1_ec47b)
  rm(hh_safra1_ec47a, hh_safra1_ec47b)
  
  # Eleg?veis totais
  
  hh_safra1_eleg <- full_join(hh_safra1_ec47, hh_safra1_regperm)
  rm(hh_safra1_ec47, hh_safra1_regperm)
  
  # A2 - Mulheres
  
  # A2.1 - regra permanente
  
  mm_safra1_regperm1 <- filter(sobrev_n_compuls_ano, ano_adm <= 1998 & genero =="F"& idade >= 55 & tempo1 >= 30 & tempo_empreg >= 10)
  mm_safra1_regperm2 <- filter(sobrev_n_compuls_ano, ano_adm <= 1998 &genero =="F"& idade >= 50 & tempo2 >= 25 & tempo_empreg >= 10)
  
  mm_safra1_regperm <- full_join(mm_safra1_regperm1, mm_safra1_regperm2)
  rm(mm_safra1_regperm1, mm_safra1_regperm2)
  
  # A2.2 - EC 47
  
  mm_safra1_ec47a <- filter(sobrev_n_compuls_ano, ano_adm <= 1998 & genero =="F"& tempo1 >= 30 & tempo_empreg >= 25 & idade >= idade_aj1)
  mm_safra1_ec47b <- filter(sobrev_n_compuls_ano, ano_adm <= 1998 &genero =="F"& tempo2 >= 25 & tempo_empreg >= 25 & idade >= idade_aj2)
  
  mm_safra1_ec47 <- full_join(mm_safra1_ec47a, mm_safra1_ec47b)
  rm(mm_safra1_ec47a, mm_safra1_ec47b)
  
  # A2.3 - Eleg?veis totais
  
  mm_safra1_eleg <- full_join(mm_safra1_ec47, mm_safra1_regperm)
  rm(mm_safra1_ec47, mm_safra1_regperm)
  
  safra1_eleg <- full_join(mm_safra1_eleg, hh_safra1_eleg)
  rm(hh_safra1_eleg, mm_safra1_eleg)
  
  # B - Safra 1.5 
  
  # B1- Homens 
  
  # B1.1 - EC 41
  
  hh_safra15_ec411 <- filter(sobrev_n_compuls_ano, ano_adm > 1998 & ano_adm < 2004 & genero =="M"& idade >= 60 & tempo1 >= 35 & tempo_empreg >= 20)
  hh_safra15_ec412 <- filter(sobrev_n_compuls_ano, ano_adm > 1998 & ano_adm < 2004 &genero =="M"& idade >= 55 & tempo2 >= 30 & tempo_empreg >= 20)
  
  hh_safra15_ec41 <- full_join(hh_safra15_ec411, hh_safra15_ec412)
  rm(hh_safra15_ec411, hh_safra15_ec412)
  
  # B2- Mulheres 
  
  # B2.1 - EC 41
  
  mm_safra15_ec411 <- filter(sobrev_n_compuls_ano, ano_adm > 1998 & ano_adm < 2004 & genero =="F"& idade >= 55 & tempo1 >= 30 & tempo_empreg >= 20)
  mm_safra15_ec412 <- filter(sobrev_n_compuls_ano, ano_adm > 1998 & ano_adm < 2004 &genero =="F"& idade >= 50 & tempo2 >= 25 & tempo_empreg >= 20)
  
  mm_safra15_ec41 <- full_join(mm_safra15_ec411, mm_safra15_ec412)
  rm(mm_safra15_ec411, mm_safra15_ec412)
  
  # B2.3 - S?ntese safra 1.5
  
  safra15_eleg_ano <- rbind(hh_safra15_ec41, mm_safra15_ec41)
  
  rm(hh_safra15_ec41, mm_safra15_ec41)
  
  # C - Safra 2 
  
  # C1- Homens - regra permanente
  
  hh_safra2_regperm1 <- filter(sobrev_n_compuls_ano, ano_adm > 2003 & genero =="M"& idade >= 60 & tempo1 >= 35 & tempo_empreg >= 10 )
  hh_safra2_regperm2 <- filter(sobrev_n_compuls_ano, ano_adm > 2003 &genero =="M"& idade >= 55 & tempo2 >= 30 & tempo_empreg >= 10 )
  
  hh_safra2_regperm <- full_join(hh_safra2_regperm1, hh_safra2_regperm2)
  rm(hh_safra2_regperm1, hh_safra2_regperm2)
  
  # C2- Mulheres 
  
  mm_safra2_regperm1 <- filter(sobrev_n_compuls_ano, ano_adm > 2003 & genero =="F"& idade >= 55 & tempo1 >= 30 & tempo_empreg >= 10 )
  mm_safra2_regperm2 <- filter(sobrev_n_compuls_ano, ano_adm > 2003 &genero =="F"& idade >= 50 & tempo2 >= 25 & tempo_empreg >= 10 )
  
  mm_safra2_regperm <- full_join(mm_safra2_regperm1, mm_safra2_regperm2)
  rm(mm_safra2_regperm1, mm_safra2_regperm2)
  
  #C3 - S?ntese safra2
  
  eleg_ano_safra2 <<- full_join(hh_safra2_regperm, mm_safra2_regperm)
  set.seed(42)
  rows <- sample(nrow(eleg_ano_safra2))
  eleg_ano_safra2 <- eleg_ano_safra2[rows, ]
  
  rm(hh_safra2_regperm, mm_safra2_regperm)
  
  ## CALCULANDO O N?MERO DE NOVAS APOSENTADORIAS VOLUNT?RIAS EM 2020 DAS SAFRAS 1 E 1.5
  
  eleg_ano_safras115 <<- rbind(safra1_eleg, safra15_eleg_ano)
  set.seed(42)
  rows <- sample(nrow(eleg_ano_safras115))
  eleg_ano_safras115 <- eleg_ano_safras115[rows, ]
  
  rm(safra1_eleg, safra15_eleg_ano)
  
  apos_est_safras115 <<- sintetico(nrow(eleg_ano_safras115), 0.4)
  apos_est_safra2 <<- sintetico(nrow(eleg_ano_safra2), 0.3)
  
  }

aposentadoria.voluntaria_safra1 <- function(eleg_ano_safras115){
 
  apos_vol_safra1_ano <<- head(eleg_ano_safras115, apos_est_safras115)} 

aposentadoria.voluntaria_safra2 <- function(eleg_ano_safra2) {

  apos_vol_safra2_ano <- head(eleg_ano_safra2, apos_est_safra2)

  apos_vol_safra2_ano <- mutate(apos_vol_safra2_ano, rem_med_nom1 = rem_med_nom/(1.015^tempo_empreg))
  apos_vol_safra2_ano <- mutate(apos_vol_safra2_ano, rem_med_nom = (rem_med_nom+rem_med_nom1)/2)
  apos_vol_safra2_ano$rem_med_nom1 <- NULL 
  apos_vol_safra2_ano <<- apos_vol_safra2_ano} 

novas.aposentadorias.finais <- function(apos_vol_safra1_ano, apos_vol_safra2_ano, compulsorio_ano) { 
  novos_aposentados_ano <<- rbind(apos_vol_safra1_ano, apos_vol_safra2_ano,compulsorio_ano)
} 

atualizar.ativos <- function(sobrev_n_compuls_ano, novos_aposentados_ano){
  ativos_ano <- anti_join(sobrev_n_compuls_ano, novos_aposentados_ano, by = "id") %>% 
    mutate(idade = idade+1,tempo_empreg = tempo_empreg+1,
           rem_med_nom = rem_med_nom*1.015, 
           tempo1 = 1+tempo1, tempo2 = 1+tempo2, 
           pontos1 = 1+pontos1, pontos2 = 1+pontos2,
           idade_aj1 = idade_aj1-1, idade_aj2 = idade_aj2-1)
  
  set.seed(42)
  rows <- sample(nrow(ativos_ano))
  ativos_ano <- ativos_ano[rows, ]
  return(ativos_ano)
}

# 2020 -------------------------------------------------------------------------
## SUBINDO OS DADOS DA TROPA, AJUSTANDO OS DADOS E ETC.
munics_maisde500mil <- read.csv2("C:/Users/Diogo/Desktop/Ipea/Simulações/Programação CH/microdados_munics_maisde500mil.csv")
munics_maisde500mil[1:2]<- NULL
names(munics_maisde500mil)[1] <- "id"
munics_maisde500mil$id <- 1:nrow(munics_maisde500mil)

names(munics_maisde500mil)[5] <- "idade"
munics_maisde500mil <- mutate(munics_maisde500mil, tempo_empreg = round(tempo_empreg/12, digits=0),
                              idade_adm = idade-tempo_empreg)

munics_maisde500mil$idade_adm <- ifelse(munics_maisde500mil$idade_adm < 18, 18, munics_maisde500mil$idade_adm)
munics_maisde500mil$idade_adm <- ifelse(munics_maisde500mil$idade_adm > 74, 74, munics_maisde500mil$idade_adm)

munics_maisde500mil <- mutate(munics_maisde500mil, taverb_prof = round((idade_adm-18)/6), digits = 0)
munics_maisde500mil <- mutate(munics_maisde500mil, taverb = round((idade_adm-18)/2.3), digits = 0)

munics_maisde500mil$genero <- ifelse(munics_maisde500mil$genero == 1, "M", "F")
munics_maisde500mil <- mutate(munics_maisde500mil, ano_adm = 2019 - tempo_empreg) 

munics_maisde500mil <- mutate(munics_maisde500mil, tempo1 = taverb+tempo_empreg, tempo2 = taverb_prof+tempo_empreg)

munics_maisde500mil_hh <- filter(munics_maisde500mil, genero == "M")

munics_maisde500mil_hh <- mutate(munics_maisde500mil_hh,  pontos1 = tempo1-35, pontos2 = tempo2-30, idade_aj1 = idade-pontos1, 
                                idade_aj2 = idade-pontos2)

munics_maisde500mil_mm <- filter(munics_maisde500mil, genero == "F")
munics_maisde500mil_mm <- mutate(munics_maisde500mil_mm,  pontos1 = tempo1 -30, pontos2 = tempo2-25, idade_aj1 = idade-pontos1, 
                                idade_aj2 = idade-pontos2)

munics_maisde500mil <- full_join(munics_maisde500mil_mm, munics_maisde500mil_hh)
rm(munics_maisde500mil_hh, munics_maisde500mil_mm)


munics_maisde500mil[2:4] <- NULL
munics_maisde500mil[10:20] <- NULL
munics_maisde500mil[9:11] <- NULL
munics_maisde500mil[4:5] <- NULL

# TRANSFORMANDO O ANO BASE DOS DADOS DE 2019 PARA 2020
ativos_2020 <- mutate(munics_maisde500mil, tempo_empreg = tempo_empreg+1)
ativos_2020$rem_med_nom <- 1.023*ativos_2020$rem_med_nom 
ativos_2020$idade <- 1+ativos_2020$idade
ativos_2020$tempo1 <- 1+ativos_2020$tempo1
ativos_2020$tempo2 <- 1+ativos_2020$tempo2
ativos_2020$pontos1 <- 1+ativos_2020$pontos1
ativos_2020$pontos2 <- 1+ativos_2020$pontos2
ativos_2020$idade_aj1 <- ativos_2020$idade_aj1-1
ativos_2020$idade_aj2 <- ativos_2020$idade_aj2-1

ativos_2019 <- munics_maisde500mil

set.seed(42)
rows <- sample(nrow(ativos_2020))
ativos_2020 <- ativos_2020[rows, ]
rm(rows, munics_maisde500mil)

# CALCULANDO O N?MERO DE SERVIDORES QUE DEVEM MORRER EM 2020
tabua_m <- read.csv2("C:/Users/Diogo/Desktop/Ipea/Simulações/tabua_nm_mul.csv")
tabua_h <- read.csv2("C:/Users/Diogo/Desktop/Ipea/Simulações/tabua_nm_hom.csv")
tabua_h$genero <- "M"
tabua_m$genero <- "F"
tabua <- rbind(tabua_h,tabua_m)
rm(tabua_h,tabua_m)
names(tabua)[1] <- "idade"
tabua <- select(tabua, genero, idade, mortal_0)

ativos_2020_agrup <- group_by(ativos_2020, idade, genero)
ativos_2020_agrup <- summarise(ativos_2020_agrup, qtde = n()) 

ativos_2020_agrup <- left_join(ativos_2020_agrup, tabua, by.x = idade, by.y = genero)
ativos_2020_agrup <- ativos_2020_agrup[complete.cases(ativos_2020_agrup), ]

c <- ativos_2020_agrup$qtde
d <- ativos_2020_agrup$mortal_0

ativos_2020_agrup$mortos <- foreach(i = c, j = d) %do% {sintetico(i,j)}
ativos_2020_agrup$mortos <- as.numeric(ativos_2020_agrup$mortos)
mortos_est <- sum(ativos_2020_agrup$mortos)

# DEFININDO QUEM, DE FATO, VAI MORRER EM 2020 ENTRE OS ATIVOS
e <- ativos_2020_agrup$genero
f <- ativos_2020_agrup$idade
h <- 1:nrow(ativos_2020_agrup)

df_list <- list()
filtering <- function(i,j) {filter(ativos_2020, genero==i & idade ==j)}
df_list <- foreach(i = e, j = f) %do% {filtering(i,j)}

creeping_death <- function(i) {if (ativos_2020_agrup$mortos[i] == 0) {  df_list[[i]]} 
  else {head(df_list[[i]], -ativos_2020_agrup$mortos[i])}}

df_list <- foreach(i = h) %do% {creeping_death(i)}

ativos_sobrev_2020 <- rbindlist(df_list)
rm(df_list, ativos_2020_agrup, c, d, e, f, h, i, j, mortos_est)

mortos_2020 <- setdiff(ativos_2020, ativos_sobrev_2020)

mortos_ano <- mortos_2020

pens_prob <- read.csv2("C:/Users/Diogo/Desktop/Ipea/Simulações/pensionistas_prob.csv")
names(pens_prob)[1] <- "idade" 
pens_prob <- select(pens_prob, idade, idade_conj, p_pens, genero)
pens_prob$p_pens <- as.numeric(pens_prob$p_pens)

# CALCULANDO O N?MERO DE PENS?ES A SEREM INSTITU?DAS EM 2020 E CRIANDO OS PENSIONISTAS
#SINTETICOS
novos_pensionistas_2020 <- criar.pensionistas.sinteticos(mortos_2020)

# DEFININDO OS SERVIDORES QUE SER?O APOSENTADOS COMPULSORIAMENTE
compulsorios_2020 <- filter(ativos_sobrev_2020, idade >= 75)
sobrev_n_compuls_2020 <- filter(ativos_sobrev_2020, idade < 75)

compulsorios_2020_safra1 <- filter(compulsorios_2020, ano_adm <1999)
compulsorios_2020_safra1 <- mutate(compulsorios_2020_safra1, rem_med_nom = ifelse(tempo_empreg/35<1,
                                                             tempo_empreg*rem_med_nom/35, rem_med_nom)) 

compulsorios_2020_safra2 <- filter(compulsorios_2020, ano_adm >=1999)
compulsorios_2020_safra2 <- mutate(compulsorios_2020_safra2, rem_med_nom1 = rem_med_nom/1.023^tempo_empreg,
                                     rem_med_nom = ifelse(tempo_empreg/35<1,tempo_empreg*(rem_med_nom+rem_med_nom1)/70, 
                                                                                  (rem_med_nom+rem_med_nom1)/2)) 

compulsorios_2020_safra2$rem_med_nom1 <- NULL

compulsorios_2020 <- rbind(compulsorios_2020_safra1, compulsorios_2020_safra2)
rm(compulsorios_2020_safra1, compulsorios_2020_safra2)

rm(ativos_sobrev_2020)

# Definindo os servidores eleg?veis para aposentadoria volunt?ria e os que, de fato, v?o
# se aposentar, al?m dos respectivos rendimentos
elegiveis.voluntarios(sobrev_n_compuls_2020)
aposentadoria.voluntaria_safra1(eleg_ano_safras115)
aposentadoria.voluntaria_safra2(eleg_ano_safra2)

# JUntando aposentados volunt?rios e compuls?rios
novos_aposentados_2020 <- novas.aposentadorias.finais(apos_vol_safra1_ano, apos_vol_safra2_ano, compulsorios_2020)

#ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
ativos_2021 <- atualizar.ativos(sobrev_n_compuls_2020, novos_aposentados_2020)

set.seed(42)
rows <- sample(nrow(ativos_2021))
ativos_2021 <- ativos_2021[rows, ]
rm(rows)

# 2021 em diante ---------------------------------------------------------------
anos <- 2021:2057
lista.ativos <- vector(mode = "list", length = length(anos))
lista.mortos <- vector(mode = "list", length = length(anos))
lista.novos_pensionistas <- vector(mode = "list", length = length(anos))
lista.novos_aposentados <- vector(mode = "list", length = length(anos))
lista.compulsorios <- vector(mode = "list", length = length(anos))
lista.eleg.safra2 <- vector(mode = "list", length = length(anos))
lista.eleg.safras115 <- vector(mode = "list", length = length(anos))

for(i in 1:length(anos)){
  if(i == 1){lista.ativos[[1]] <- ativos_2021}
  mortes(lista.ativos[[i]])
  lista.mortos[[i]] <- mortos_ano
  
  #CRIANDO, SE NECESS?RIO, OS NOVOS PENSIONISTAS "SINT?TICOS"
  lista.novos_pensionistas[[i]] <- criar.pensionistas.sinteticos(mortos_ano)
  
  # DEFININDO OS SERVIDORES QUE SERAO APOSENTADOS COMPULSORIAMENTE
  compulsorios_ano <- filter(ativos_sobrev_ano, idade >= 75)
   
  compulsorios_ano_safra1 <- filter(compulsorios_ano, ano_adm <1999)
  compulsorios_ano_safra1 <- mutate(compulsorios_ano_safra1, rem_med_nom = ifelse(tempo_empreg/35<1,
                                                                                    tempo_empreg*rem_med_nom/35, rem_med_nom)) 
  
  compulsorios_ano_safra2 <- filter(compulsorios_ano, ano_adm >=1999)
  compulsorios_ano_safra2 <- mutate(compulsorios_ano_safra2, rem_med_nom1 = rem_med_nom/1.023^tempo_empreg,
                                    rem_med_nom = ifelse(tempo_empreg/35<1,tempo_empreg*(rem_med_nom+rem_med_nom1)/70, 
                                                                                    (rem_med_nom+rem_med_nom1)/2)) 
  compulsorios_ano_safra2$rem_med_nom1 <- NULL
  compulsorios_ano <- rbind(compulsorios_ano_safra1, compulsorios_ano_safra2)
  rm(compulsorios_ano_safra1, compulsorios_ano_safra2)
  
  lista.compulsorios[[i]] <- compulsorios_ano
 
  sobrev_n_compuls <- filter(ativos_sobrev_ano, idade < 75)
  
  ## CALCULANDO O N?MERO DE NOVAS APOSENTADORIAS VOLUNT?RIAS EM 2021 
  elegiveis.voluntarios(sobrev_n_compuls)
  
  lista.eleg.safras115[[i]] <- eleg_ano_safras115
  lista.eleg.safra2[[i]] <- eleg_ano_safra2

    # DEFININDO QUEM, DE FATO, VAI SE APOSENTAR VOLUNTARIAMENTE EM 2021
  aposentadoria.voluntaria_safra1(eleg_ano_safras115)
  aposentadoria.voluntaria_safra2(eleg_ano_safra2)
  novas.aposentadorias.finais(apos_vol_safra1_ano, apos_vol_safra2_ano, compulsorios_ano)
  lista.novos_aposentados[[i]] <- novos_aposentados_ano
  
  #ATUALIZANDO OS DADOS DE QUEM FICOU NA ATIVA
  if(i < length(anos)){
    lista.ativos[[i+1]] <- atualizar.ativos(sobrev_n_compuls, lista.novos_aposentados[[i]])
  }
}
names(lista.mortos) <- anos
names(lista.compulsorios) <- anos
names(lista.eleg.safra2) <- anos
names(lista.eleg.safras115) <- anos

lista.ativos <- c(list(ativos_2020), lista.ativos)
names(lista.ativos) <- c(2020, anos)
saveRDS(lista.ativos, "C:/Users/Diogo/Desktop/Ipea/Simulações/Programação CH/simul_ativos/lista.ativos.rds")

lista.novos_aposentados <- c(list(novos_aposentados_2020), lista.novos_aposentados)
names(lista.novos_aposentados) <- c(2020, anos)
saveRDS(lista.novos_aposentados, "C:/Users/Diogo/Desktop/Ipea/Simulações/Programação CH/simul_novos_inativos/lista.novos_aposentados.rds")

lista.novos_pensionistas <- c(list(novos_pensionistas_2020), lista.novos_pensionistas)
names(lista.novos_pensionistas) <- c(2020, anos)
saveRDS(lista.novos_pensionistas, "C:/Users/Diogo/Desktop/Ipea/Simulações/Programação CH/simul_novos_pensionistas/lista.novos_pensionistas.rds")
