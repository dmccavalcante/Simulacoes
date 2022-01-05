library(tidyverse)
library(dplyr)
library(readr)

#Estados
estados_microdados <-
  readRDS("//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Microdados 2014-2018/Microdados Painel/painel_19_temporario.rds") %>% 
  mutate(inativo = ifelse(saida %in% "inativo", 1, 0)) %>% 
  filter(categoria %in% "professor")

estados_microdados$ano_adm <- as.integer(estados_microdados$ano_adm)
estados_microdados$tempo_empreg1 <- 2019 - estados_microdados$ano_adm

estados_microdados <- 
  select(estados_microdados, 
         c("uf", "genero", "idade", "ano_adm", "rem_med_nominal", "tempoAnteriorTotal", 
           "tempoAnteriorProfessor", "tempo_empreg1", "inativo"))

names(estados_microdados)[5] <- "rem_med_nom"
names(estados_microdados)[6] <- "taverb"
names(estados_microdados)[7] <- "taverb_prof"
names(estados_microdados)[8] <- "tempo_empreg"

estados_microdados <- mutate(estados_microdados, idade_adm = idade-tempo_empreg)
estados_microdados$nivelgov <- "Estados"

estados_microdados$idade_adm <- ifelse(estados_microdados$idade_adm < 18, 18, estados_microdados$idade_adm)
estados_microdados$idade_adm <- round(estados_microdados$idade_adm, digits = 0)

estados_com_reforma <- filter(estados_microdados, uf %in% c("AC", "AL", "BA", "CE", "ES", "GO", "MT", "MS", "MG",
                                                            "PA", "PI", "PB", "PR", "RN", "RS", "SE", "SP"))
estados_sem_reforma <- filter(estados_microdados, uf %in% c("AM", "AP", "DF", "MA", "PE", "RJ", "RR", "RO", 
                                                            "SC", "TO"))
estados_com_reforma[1] <- NULL
estados_sem_reforma[1] <- NULL
rm(estados_microdados)

write.csv2(estados_com_reforma, "//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Simulações/microdados_finais/estados_com_reforma.csv")
write.csv2(estados_sem_reforma, "//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Simulações/microdados_finais/estados_sem_reforma.csv")

# 2014:2018 --------------------------------------------------------------------
anos <- 2014:2018
for(j in 1:5){
  estados_microdados <-
    readRDS(str_c("//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Microdados 2014-2018/Microdados Painel/painel_", str_sub(anos[j], 3, 4), ".rds")) %>% 
    mutate(inativo = ifelse(saida %in% "inativo", 1, 0)) %>% 
    filter(categoria %in% "professor")
  
  estados_microdados$ano_adm <- as.integer(estados_microdados$ano_adm)
  estados_microdados$tempo_empreg1 <- anos[j] - estados_microdados$ano_adm
  
  estados_microdados <- 
    select(estados_microdados, 
           c("uf", "genero", "idade", "ano_adm", "rem_med_nominal", "tempoAnteriorTotal", 
             "tempoAnteriorProfessor", "tempo_empreg1", "inativo"))
  
  names(estados_microdados)[5] <- "rem_med_nom"
  names(estados_microdados)[6] <- "taverb"
  names(estados_microdados)[7] <- "taverb_prof"
  names(estados_microdados)[8] <- "tempo_empreg"
  
  estados_microdados <- mutate(estados_microdados, idade_adm = idade-tempo_empreg)
  estados_microdados$nivelgov <- "Estados"
  
  estados_microdados$idade_adm <- ifelse(estados_microdados$idade_adm < 18, 18, estados_microdados$idade_adm)
  estados_microdados$idade_adm <- round(estados_microdados$idade_adm, digits = 0)
  
  estados_com_reforma <- filter(estados_microdados, uf %in% c("AC", "AL", "BA", "CE", "ES", "GO", "MT", "MS", "MG",
                                                              "PA", "PI", "PB", "PR", "RN", "RS", "SE", "SP"))
  estados_sem_reforma <- filter(estados_microdados, uf %in% c("AM", "AP", "DF", "MA", "PE", "RJ", "RR", "RO", 
                                                              "SC", "TO"))

  estados_com_reforma <- ajustar.dados(estados_com_reforma)
  
  anos.aux <- anos[j]:1990
  aux.lista <- vector(mode = "list", length = length(anos.aux))
  aux.elegiveis <- vector(mode = "list", length = length(anos.aux))
  for(i in 1:length(anos.aux)){
    if(i == 1) aux.lista[[i]] <- estados_com_reforma
    elegiveis.voluntarios(aux.lista[[i]], anos.aux[i])
    aux.elegiveis[[i]] <- rbind(eleg_com_safra1_ano, eleg_com_safra2_ano) %>% 
      select(id) %>% 
      mutate(ano = 1)
    
    names(aux.elegiveis[[i]])[2] <- str_c("ano_", anos.aux[i])
    
    if(i < length(anos.aux)){
      aux.lista[[i+1]] <- aux.lista[[i]] %>% 
        mutate(rem_med_nom = taxa_atualizacao_salarial/rem_med_nom,
               tempo_empreg = tempo_empreg - 1, idade = idade - 1,
               tempo1 = tempo1 - 1, tempo2 = tempo2 - 1,
               tempo3 = tempo3 - 1, tempo4 = tempo4 - 1,
               pontos1 = pontos1 - 2, pontos2 = pontos2 - 2)
      
    }
  }
  rm(aux.lista)
  
  elegiveis <- aux.elegiveis %>% 
    reduce(left_join, by = "id") %>% 
    mutate(tempo_expirado = rowSums(.[, str_c("ano_", anos.aux)], na.rm = T)) %>% 
    select(id, tempo_expirado)
  
  estados_com_reforma <- left_join(estados_com_reforma, elegiveis, by = "id") %>% 
    mutate(tempo_expirado = ifelse(is.na(tempo_expirado), 0, tempo_expirado))
  
  saveRDS(estados_com_reforma, str_c("//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Simulações/microdados_finais/estados_com_reforma_", str_sub(anos[j], 3, 4), ".rds"))
  saveRDS(estados_sem_reforma, str_c("//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Simulações/microdados_finais/estados_sem_reforma_", str_sub(anos[j], 3, 4), ".rds"))
}
