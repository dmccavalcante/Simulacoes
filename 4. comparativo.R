library(data.table)
library(openxlsx)
library(ggplot2)
library(ggpubr)
library(scales)

options(scipen=999)
# "ativos", "novos_aposentados", "novos_pensionistas"
tipo <- "novos_pensionistas"
arquivos <- list.files("//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Simulações/microdados_finais/comparativo", full.names = T, pattern = tipo)

lista.aux <- vector(mode = "list", length = length(arquivos))
for(i in 1:length(arquivos)){
  lista.aux[[i]] <- readRDS(arquivos[i])
  lista.aux[[i]] <- data.frame(Ano = as.numeric(names(lista.aux[[i]])),
                               Quantidade = sapply(lista.aux[[i]], function(x) nrow(x)),
                               Gastos = sapply(lista.aux[[i]], function(x) sum(x$rem_med_nom*12, na.rm = T))/(10^9),
                               Taxa_reposicao = substr(arquivos[i], 104 + nchar(tipo), 107 + nchar(tipo)))
}

dados <- rbindlist(lista.aux)
quantidade <- 
  ggplot(dados, aes(x = Ano, y = Quantidade, group = Taxa_reposicao)) + 
  geom_line(aes(color = Taxa_reposicao, linetype = Taxa_reposicao)) +
  scale_y_continuous(labels = label_comma(big.mark = "\\.")) + 
  theme_bw(base_size = 13) +
  theme(legend.position = "none") 

gastos <- 
ggplot(dados, aes(x = Ano, y = Gastos, group = Taxa_reposicao)) + 
  geom_line(aes(color = Taxa_reposicao, linetype = Taxa_reposicao)) +
  ylab("Gastos (R$ Bilhões)") +
  scale_y_continuous(labels = label_comma(big.mark = "\\.")) + 
  theme_bw(base_size = 13)

ggarrange(quantidade, gastos,
          ncol = 2, nrow = 1,
          widths = c(.85, 1))

aux.quantidade <- dados %>% select(-Gastos) %>% spread(., Taxa_reposicao, Quantidade)
aux.gastos <- dados %>% select(-Quantidade) %>% spread(., Taxa_reposicao, Gastos)

dados_consolidado <- left_join(aux.quantidade, aux.gastos, by = "Ano")
write.xlsx(dados_consolidado, paste0("//sbsb2/DIMAC/Novo DIRETORIO NEMAC/Bolsistas/Diogo/Simulações/microdados_finais/comparativo/resultados/", tipo, ".xlsx"))
