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

#Removidas por interpretabilidade
dados <- dados[, -which(colnames(dados) %in% c("moradia_tipo", "moradia_estilo", "exterior_cobertura_2", "porao_area_total", "acima_solo_area", "garagem_capacidade_carros"))]
categorical_cols <- sapply(dados, is.character)
dados[, categorical_cols] <- lapply(dados[, categorical_cols], as.factor)
categorical_cols <- sapply(dados, is.factor)

predictors <- setdiff(names(dados), target_variable)

# Criar variáveis dummies para colunas categóricas
dados <- dummy_cols(dados, select_columns = names(categorical_cols[categorical_cols == TRUE]), remove_selected_columns = TRUE)


# Atualizar lista de preditores após criação de dummies
predictors <- setdiff(names(dados), target_variable)

# Divisão dos dados: Treino (60%), Teste (20%), Validação (20%)

trainIndex <- createDataPartition(dados[[target_variable]], p = 0.6, list = FALSE)
train_data <- dados[trainIndex, ]
temp_data <- dados[-trainIndex, ]

testIndex <- createDataPartition(temp_data[[target_variable]], p = 0.5, list = FALSE)
test_data <- temp_data[testIndex, ]
valid_data <- temp_data[-testIndex, ]

# Ajustar Modelos
glm_model <- glm(venda_valor ~ ., data = train_data, family = Gamma)
step(glm_model, direction = "both")

#X_train <- as.matrix(train_data[, predictors])
#X_valid <- as.matrix(valid_data[, predictors])


