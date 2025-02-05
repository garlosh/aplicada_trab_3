library(readxl)
library(purrr)  
library(xgboost)
library(caret)
library(randomForest)
library(performanceEstimation)
source("utils.R")
set.seed(2025)
dados <- read_excel("./dados/dados5.xlsx") %>% as.data.frame()
nome_colunas <- list(
  "base" = c(),
  "identificador" = c(),
  "preenchimento" = c(),
  "variabilidade" = c(),
  "correlacao" = c()
)
dados[, c("porao_num_banheiros_lavabos", "acima_solo_num_cozinhas")] <- sapply(dados[, c("porao_num_banheiros_lavabos", "acima_solo_num_cozinhas")], as.factor)
nome_colunas$base = colnames(dados)
dados <- dados[, -c(1, 2)]
target_variable <- "venda_valor"
nome_colunas$identificador = colnames(dados)
#Remoção de NA's e variaveis com baixa variabilidade

dados <- filtrar_colunas_na(dados, .90)
nome_colunas$preenchimento = colnames(dados)
dados <- filtrar_variaveis_numericas(dados)
nome_colunas$variabilidade = colnames(dados)

# Substituição NA por ausente para categóricas e inputação de valores para continuas
categorical_cols <- sapply(dados, function (x) {is.character(x) || is.factor(x)})
numerical_cols <- sapply(dados[, setdiff(colnames(dados), c("venda_valor"))], is.numeric)

dados[, numerical_cols] <- knnImp(dados[, numerical_cols])
dados <- substituir_na_por_ausentes(dados)

#Agrupamento de categorias não representativas
niveis_categoricas_antes <- lapply(dados[, categorical_cols], unique)
dados <- agrupa_categorias(dados, 0.03)
niveis_categoricas_depois <- lapply(dados[, categorical_cols], unique)
niveis_agrupados <- vector("list", length = length(niveis_categoricas_depois))
for (i in seq_len(length(niveis_agrupados))) {
  niveis_agrupados[[i]] <- setdiff(niveis_categoricas_antes[[i]], niveis_categoricas_depois[[i]])
}
names(niveis_agrupados) <- names(niveis_categoricas_antes)
dados[, categorical_cols] <- lapply(dados[, categorical_cols], as.factor)


#Calculo das correlações e limpeza das muito correlacionadas
cramer_cor <- calc_cramer_corr(dados, target_variable)
pearson_cor <- calc_pearson_corr(dados, target_variable)

index_cramer <- which(abs(cramer_cor) > 0.7, arr.ind = TRUE)
index_pearson <- which(abs(pearson_cor) > 0.7,  arr.ind = TRUE)

colnames(cramer_cor)[index_cramer[, 2]]
colnames(pearson_cor)[index_pearson[, 2]] 

#Removidas por interpretabilidade
dados <- dados[, -which(colnames(dados) %in% c("moradia_tipo", "moradia_estilo", "exterior_cobertura_2",
                                               "venda_condicao", "porao_area_total", "acima_solo_area", "garagem_area"))]
nome_colunas$correlacao = colnames(dados)

colunas_removidas_etapas <- vector("list", length = length(nome_colunas)-1)
for (i in 2:length(nome_colunas)) {
  colunas_removidas_etapas[[i-1]] <- setdiff(nome_colunas[[i-1]], nome_colunas[[i]])
}
names(colunas_removidas_etapas) <- names(nome_colunas)[-1]

write.table(dados, "./dados/base_processada.csv", sep = ";", quote = FALSE, row.names = FALSE)

