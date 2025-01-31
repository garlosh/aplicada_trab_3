library(readxl)
library(purrr)  
source("utils.R")
dados <- read_excel("./dados/dados5.xlsx") %>% as.data.frame()
dados <- dados[, -c(1, 2)]


dados <- filtrar_colunas_na(dados, .90)
dados <- filtrar_variaveis_numericas(dados)
target_variable <- "venda_valor"
cramer_cor <- calc_cramer_corr(dados, target_variable)
pearson_cor <- calc_pearson_corr(dados, target_variable)

#falta tirar essas variaveis
which(abs(cramer_cor[lower.tri(cramer_cor)]) > 0.7)
which(abs(pearson_cor[lower.tri(pearson_cor)]) > 0.7)
