library(tidyverse)
options(scipen=999)
# Funções ----------------------------------------------------------------------
filtro.elegiveis <- function(base, ano.da.base, 
                             estado = c("AC","AM","RR","PA","AP","TO","MA",
                                        "PI","CE","RN","PB","PE","AL","SE","BA",
                                        "MG","ES","RJ","SP","PR","SC","RS","MS",
                                        "MT","GO","DF","RO")){
  base <- base %>% 
    mutate(tempo_servico = ano.da.base - as.numeric(ano_adm) + 1 + tempoAnteriorTotal,
           rem_med_nominal = ifelse(rem_med_nominal %in% 0, NA, rem_med_nominal),
           inativo = ifelse(saida %in% "inativo", 1, 0),
           valor = 1,
           categoria = ifelse(categoria %in% "outros executivo", "000outros executivo", categoria),
           generoM = ifelse(genero %in% "M", 1, 0)) %>%
    filter(idade >= 60 & tempo_servico >= 25 & !categoria %in% "NV30" & !uf %in% "MG") %>% 
    filter(uf %in% estado) %>% 
    select(uf, inativo, idade, genero, categoria, rem_med_nominal, tempo_servico) %>% 
    mutate(`agente penitenciario` = ifelse(categoria %in% "agente penitenciario", 1, 0),
           `DP` = ifelse(categoria %in% "DP", 1, 0),
           `judiciario` = ifelse(categoria %in% "judiciario", 1, 0),
           `legislativo` = ifelse(categoria %in% "legislativo", 1, 0),
           `militar` = ifelse(categoria %in% "militar", 1, 0),
           `MP` = ifelse(categoria %in% "MP", 1, 0),
           `outros executivo` = ifelse(categoria %in% "outros executivo", 1, 0),
           `policial civil` = ifelse(categoria %in% "policial civil", 1, 0),
           `professor` = ifelse(categoria %in% "professor", 1, 0),
           `TC` = ifelse(categoria %in% "TC", 1, 0))
  return(base)
}

modelo.previsao <- function(base, funcao_ligacao = "logit"){
  modelo <- glm(inativo ~ idade + genero + categoria + rem_med_nominal, family = binomial(link = funcao_ligacao), data = base)
  base$probabilidade_estimada <- predict(modelo, base, type = "response", se.fit = F)
  base$uniforme <- runif(nrow(base))
  base <- base %>% mutate(inativo_estimado = ifelse(uniforme <= probabilidade_estimada, 1, 0), 
                          previsao_correta = ifelse(inativo_estimado == inativo, 1, 0))
  return(base)
}

# Leitura e filtros nas bases --------------------------------------------------
r14_backup <- readRDS("//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Microdados 2014-2018/Microdados Painel/painel_14.rds")
r15_backup <- readRDS("//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Microdados 2014-2018/Microdados Painel/painel_15.rds")
r16_backup <- readRDS("//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Microdados 2014-2018/Microdados Painel/painel_16.rds")
r17_backup <- readRDS("//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Microdados 2014-2018/Microdados Painel/painel_17.rds")
r18_backup <- readRDS("//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Microdados 2014-2018/Microdados Painel/painel_18.rds")
r19_backup <- readRDS("//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Microdados 2014-2018/Microdados Painel/painel_19_temporario.rds")

r14 <- filtro.elegiveis(r14_backup, 2014)
r15 <- filtro.elegiveis(r15_backup, 2015)
r16 <- filtro.elegiveis(r16_backup, 2016)
r17 <- filtro.elegiveis(r17_backup, 2017)
r18 <- filtro.elegiveis(r18_backup, 2018)
r19 <- filtro.elegiveis(r19_backup, 2019)

# Descritivas ------------------------------------------------------------------
# descritivas_elegiveis <-
#   r15 %>% 
#   group_by(uf) %>% 
#   summarise(Quantidade_elegiveis = n(),
#             rem_media_elegiveis = mean(rem_med_nominal, na.rm = T),
#             rem_mediana_elegiveis = median(rem_med_nominal, na.rm = T),
#             idade_media_elegiveis = mean(idade, na.rm = T),
#             idade_mediana_elegiveis = median(idade, na.rm = T),
#             perc_fem = sum(genero %in% "F")/n())
# chisq.test(table(r15$inativo, r15$genero))
# table(r15$inativo, r15$categoria)

# Regressão logística ----------------------------------------------------------
set.seed(123)
r14 <- subset(r14, categoria %in% "professor")
r15 <- subset(r15, categoria %in% "professor")
r16 <- subset(r16, categoria %in% "professor")
r17 <- subset(r17, categoria %in% "professor")
r18 <- subset(r18, categoria %in% "professor")
r19 <- subset(r19, categoria %in% "professor")

logit_14 <- glm(inativo ~ idade + genero + rem_med_nominal, family = binomial(link = "probit"), data = r14)
logit_15 <- glm(inativo ~ idade + genero + rem_med_nominal, family = binomial(link = "probit"), data = r15)
logit_16 <- glm(inativo ~ idade + genero + rem_med_nominal, family = binomial(link = "probit"), data = r16)
logit_17 <- glm(inativo ~ idade + genero + rem_med_nominal, family = binomial(link = "probit"), data = r17)
logit_18 <- glm(inativo ~ idade + genero + rem_med_nominal, family = binomial(link = "probit"), data = r18)
logit_19 <- glm(inativo ~ idade + genero + rem_med_nominal, family = binomial(link = "probit"), data = r19)

summary(logit_14)
summary(logit_15)
summary(logit_16)
summary(logit_17)
summary(logit_18)
summary(logit_19)

data.frame(
  mod_14 = logit_14$coefficients,
  mod_15 = logit_15$coefficients,
  mod_16 = logit_16$coefficients,
  mod_17 = logit_17$coefficients,
  mod_18 = logit_18$coefficients,
  mod_19 = logit_19$coefficients
)

r14$probabilidade_estimada <- predict(logit_18, r14, type = "response", se.fit = F)
r15$probabilidade_estimada <- predict(logit_18, r15, type = "response", se.fit = F)
r16$probabilidade_estimada <- predict(logit_18, r16, type = "response", se.fit = F)
r17$probabilidade_estimada <- predict(logit_18, r17, type = "response", se.fit = F)
r18$probabilidade_estimada <- predict(logit_18, r18, type = "response", se.fit = F)
r19$probabilidade_estimada <- predict(logit_18, r19, type = "response", se.fit = F)

r14$uniforme <- runif(nrow(r14))
r15$uniforme <- runif(nrow(r15))
r16$uniforme <- runif(nrow(r16))
r17$uniforme <- runif(nrow(r17))
r18$uniforme <- runif(nrow(r18))
r19$uniforme <- runif(nrow(r19))

r14 <- r14 %>% mutate(inativo_estimado = ifelse(uniforme <= probabilidade_estimada, 1, 0), previsao_correta = ifelse(inativo_estimado == inativo, 1, 0))
r15 <- r15 %>% mutate(inativo_estimado = ifelse(uniforme <= probabilidade_estimada, 1, 0), previsao_correta = ifelse(inativo_estimado == inativo, 1, 0))
r16 <- r16 %>% mutate(inativo_estimado = ifelse(uniforme <= probabilidade_estimada, 1, 0), previsao_correta = ifelse(inativo_estimado == inativo, 1, 0))
r17 <- r17 %>% mutate(inativo_estimado = ifelse(uniforme <= probabilidade_estimada, 1, 0), previsao_correta = ifelse(inativo_estimado == inativo, 1, 0))
r18 <- r18 %>% mutate(inativo_estimado = ifelse(uniforme <= probabilidade_estimada, 1, 0), previsao_correta = ifelse(inativo_estimado == inativo, 1, 0))
r19 <- r19 %>% mutate(inativo_estimado = ifelse(uniforme <= probabilidade_estimada, 1, 0), previsao_correta = ifelse(inativo_estimado == inativo, 1, 0))

table(r14$inativo_estimado)-
table(r14$inativo)
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
table(r15$inativo_estimado)-
table(r15$inativo)
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
table(r16$inativo_estimado)-
table(r16$inativo)
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
table(r17$inativo_estimado)-
table(r17$inativo)
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
table(r18$inativo_estimado)-
table(r18$inativo)
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
table(r19$inativo_estimado)-
table(r19$inativo)

# Validando o modelo de 2019 com os demais anos --------------------------------
setwd("//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Simulações/microdados_finais")
taxa_atualizacao_salarial <- 1.015
taxa_reposicao <- 0.25

ativos <- readRDS("estados_com_reforma_14.rds")
ativos <- ajustar.dados(ativos)

compulsorios <- filter(ativos, idade >= 75)
sobrev_n_compuls <- filter(ativos, idade < 75)

elegiveis.voluntarios(sobrev_n_compuls, 2014)
eleg_com_safra1 <- eleg_com_safra1_ano
eleg_com_safra2 <- eleg_com_safra2_ano

eleg_com_safra1$probabilidade_estimada <- predict(probit_19_safra_1, eleg_com_safra1, type = "response", se.fit = F)
set.seed(42)
eleg_com_safra1$uniforme <- runif(nrow(eleg_com_safra1))
eleg_com_safra1 <- eleg_com_safra1 %>% mutate(inativo_estimado = ifelse(uniforme <= probabilidade_estimada, 1, 0))

eleg_com_safra2$probabilidade_estimada <- predict(probit_19_safra_2, eleg_com_safra2, type = "response", se.fit = F)
set.seed(42)
eleg_com_safra2$uniforme <- runif(nrow(eleg_com_safra2))
eleg_com_safra2 <- eleg_com_safra2 %>% mutate(inativo_estimado = ifelse(uniforme <= probabilidade_estimada, 1, 0))

apos_vol_safra1 <- filter(eleg_com_safra1, inativo_estimado %in% 1) %>% select(-c(probabilidade_estimada, uniforme, inativo_estimado))
apos_vol_safra2 <- filter(eleg_com_safra2, inativo_estimado %in% 1) %>% select(-c(probabilidade_estimada, uniforme, inativo_estimado))

apos_vol <- rbind(apos_vol_safra1, apos_vol_safra2)
novos_aposentados <- rbind(apos_vol, compulsorios)
sum(ativos$inativo)
nrow(novos_aposentados)
# 2014
# valor real: 17249
# valor estimado: 22113
# 2015
# valor real: 20156
# valor estimado: 24243
# 2016
# valor real: 18825
# valor estimado: 24883
# 2017
# valor real: 19966
# valor estimado: 26028
# 2018
# valor real: 21245
# valor estimado: 26606

# Regressão probit -------------------------------------------------------------
# probit_14 <- glm(inativo ~ idade + genero + categoria + rem_med_nominal, family = binomial(link = "probit"), data = r14)
# probit_15 <- glm(inativo ~ idade + genero + categoria + rem_med_nominal, family = binomial(link = "probit"), data = r15)
# probit_16 <- glm(inativo ~ idade + genero + categoria + rem_med_nominal, family = binomial(link = "probit"), data = r16)
# probit_17 <- glm(inativo ~ idade + genero + categoria + rem_med_nominal, family = binomial(link = "probit"), data = r17)
# probit_18 <- glm(inativo ~ idade + genero + categoria + rem_med_nominal, family = binomial(link = "probit"), data = r18)
# 
# data.frame(
#   mod_14 = probit_14$coefficients,
#   mod_15 = probit_15$coefficients,
#   mod_16 = probit_16$coefficients,
#   mod_17 = probit_17$coefficients,
#   mod_18 = probit_18$coefficients
# )

r14 <- modelo.previsao(r14, "probit")
r14 <- modelo.previsao(r14, "probit")
r14 <- modelo.previsao(r14, "probit")
r14 <- modelo.previsao(r14, "probit")
r14 <- modelo.previsao(r14, "probit")
r14 <- modelo.previsao(r14, "probit")

predict(probit_14, r14, type = "response", se.fit = F)

# Análise de sobrevivência -----------------------------------------------------
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

a <- survfit(Surv(tempo_servico, inativo) ~ idade + genero + categoria + rem_med_nominal, data = r18)
a <- coxph(Surv(tempo_servico, inativo) ~ idade + genero + categoria + rem_med_nominal, data = r19)
a <- aareg(Surv(tempo_servico, inativo) ~ idade + genero + categoria + rem_med_nominal, data = r19)
autoplot(a)
summary(a)
tt <- a$time
s <- a$surv

reg <- survreg(Surv(r19$tempo_servico, r19$inativo) ~ idade + genero + categoria + rem_med_nominal, dist = "exp")
summary(reg)

a1 <- nlm(f = LLCD_exp, p = c(mean(r19$tempo_servico)),        ti = r19$tempo_servico, delta = r19$inativo, hessian = T, iterlim = 1000)
a2 <- nlm(f = LLCD_wei, p = c(mean(r19$tempo_servico),1),      ti = r19$tempo_servico, delta = r19$inativo, hessian = T)
a3 <- nlm(f = LLCD_llogis, p = c(mean(r19$tempo_servico),1),   ti = r19$tempo_servico, delta = r19$inativo, hessian = T)
a4 <- nlm(f = LLCD_burr12, p = c(mean(r19$tempo_servico),1,1), ti = r19$tempo_servico, delta = r19$inativo, hessian = T, iterlim = 300)
a5 <- nlm(f = LLCD_lnorm, p = c(5,5),                          ti = r19$tempo_servico, delta = r19$inativo, hessian = T)

a1
a2
a3
a4
a5

a <- survfit(Surv(r19$tempo_servico, r19$inativo)~1)
x <- 0:41
plot(a, conf.int = F)
lines(x, sexpo(x, alpha = a1$estimate[1]), col = "red")
lines(x, swei(x, alpha = a2$estimate[1], gama = a2$estimate[2]), col = "blue")
lines(x, sllogis(x, alpha = a3$estimate[1], gama = a3$estimate[2]), col = "green")
lines(x, sburr12(x, alpha = a4$estimate[1], gama = a4$estimate[2], k = a4$estimate[3]), col = "purple")
lines(x, 1 - plnorm(x, a5$estimate[1], a5$estimate[2]), col = "black")

sllogis(x, alpha = a3$estimate[1], gama = a3$estimate[2])
plot(dllogis(x, alpha = a3$estimate[1], gama = a3$estimate[2])/sllogis(x, alpha = a3$estimate[1], gama = a3$estimate[2]))


# ------------------------------------------------------------------------------
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

vet <- mutate(veteran, AG = ifelse((age < 60), "LT60", "OV60"),
              AG = factor(AG),
              trt = factor(trt,labels=c("standard","test")),
              prior = factor(prior,labels=c("N0","Yes")))

km_fit <- survfit(Surv(time, status) ~ 1, data=veteran)

km_AG_fit <- survfit(Surv(time, status) ~ AG, data=vet)

cox <- coxph(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior , data = vet)
cox_fit <- survfit(cox)

aa_fit <-aareg(Surv(time, status) ~ trt + celltype +
                 karno + diagtime + age + prior , 
               data = vet)

r_fit <- ranger(Surv(time, status) ~ trt + celltype + 
                  karno + diagtime + age + prior,
                data = vet,
                mtry = 4,
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)

# Average the survival models
death_times <- r_fit$unique.death.times 
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)

# Plot the survival models for each patient
plot(r_fit$unique.death.times,r_fit$survival[1,], 
     type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Patient Survival Curves")

#
cols <- colors()
for (n in sample(c(2:dim(vet)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))




kmi <- rep("KM",length(km_fit$time))
km_df <- data.frame(km_fit$time,km_fit$surv,kmi)
names(km_df) <- c("Time","Surv","Model")

coxi <- rep("Cox",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,coxi)
names(cox_df) <- c("Time","Surv","Model")

rfi <- rep("RF",length(r_fit$unique.death.times))
rf_df <- data.frame(r_fit$unique.death.times,avg_prob,rfi)
names(rf_df) <- c("Time","Surv","Model")

plot_df <- rbind(km_df,cox_df,rf_df)

p <- ggplot(plot_df, aes(x = Time, y = Surv, color = Model))
p + geom_line()
