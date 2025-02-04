library(readxl)
library(purrr)  
library(xgboost)
library(caret)
library(Metrics)
library(randomForest)
library(fastDummies)
source("utils.R")
set.seed(2025)
dados <- read_excel("./dados/dados5.xlsx") %>% as.data.frame()
dados <- dados[, -c(1, 2)]


dados <- filtrar_colunas_na(dados, .90)
dados <- filtrar_variaveis_numericas(dados)
target_variable <- "venda_valor"
cramer_cor <- calc_cramer_corr(dados, target_variable)
pearson_cor <- calc_pearson_corr(dados, target_variable)

#falta tirar essas variaveis
index_cramer <- which(abs(cramer_cor) > 0.7, arr.ind = TRUE)
index_pearson <- which(abs(pearson_cor) > 0.7,  arr.ind = TRUE)

colnames(cramer_cor)[index_cramer[, 2]]
colnames(pearson_cor)[index_pearson[, 2]] 

# Substituição NA por ausente
dados[is.na(dados)] <- "Ausentes"

#Removidas por interpretabilidade
dados <- dados[, -which(colnames(dados) %in% c("moradia_tipo", "moradia_estilo", "exterior_cobertura_2", "porao_area_total", "acima_solo_area", "garagem_capacidade_carros"))]

#categorical_cols <- sapply(dados, is.factor)

dados <- agrupa_categorias(dados, 0.03)
categorical_cols <- sapply(dados, is.character)
dados[, categorical_cols] <- lapply(dados[, categorical_cols], as.factor)
# Divisão dos dados: Treino (%), Teste (%), Validação (%)
trainIndex <- createDataPartition(dados[[target_variable]], p = 0.8, list = FALSE)
train_data <- dados[trainIndex, ]
test_data <- dados[-trainIndex, ]

# Ajustar Modelos

glm_model <- glm(venda_valor ~ 1, data = train_data, family = Gamma(link = 'log'))

teste <- step(glm_model, scope = formula(glm(venda_valor ~ ., data = train_data, family = Gamma(link = 'log'))),  direction = "both")
predict(teste, newdata = test_data[, -which(colnames(test_data) %in% "venda_valor")], type = "response") - dados$venda_valor

pareto.chart(table(dados$vizinhanca)/length(dados$vizinhanca))

