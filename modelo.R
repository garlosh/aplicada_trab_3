library(fitdistrplus)
library(rpart)
library(rpart.plot)
set.seed(2025)
dados <- read.table("./dados/base_processada.csv", sep = ";", header = TRUE, dec = ".")
dados <- dados[, -which(colnames(dados) %in% c("venda_ano", "venda_mes"))]

#Escolha dist
dados[dados[, "exterior_qualidade"] == "m\\u00e9dia", "exterior_qualidade"] <- "média"
descdist(dados$venda_valor, discrete = FALSE)
plot(fitdist(dados$venda_valor, "gamma"))
plot(fitdist(dados$venda_valor, "lnorm"))
comparacao_modelos <- list("glm" = c(), "arvore" = c())
#divisao treino e teste
trainIndex <- sample(1:nrow(dados), round(nrow(dados)*0.8))
dados_treino <- dados[trainIndex, ]
dados_teste <- dados[-trainIndex, -which(colnames(dados) %in% c("venda_valor"))]

#modelo GLM
glm_fit <- glm(venda_valor~1, data = dados_treino, family = gaussian(link = "log"))
step_glm <- step(glm_fit, scope = formula(glm(venda_valor~., data = dados, family = gaussian(link = "log"))), direction = "both", k = log(nrow(dados_treino)))
comparacao_modelos$glm$mse = RMSE(predict(step_glm, newdata = dados_teste, type = "response"), dados$venda_valor[-trainIndex])
length(unlist(strsplit(as.character(formula(step_glm)), "\\+")))

#Arvore
#arvore_fit <- rpart(venda_valor~., data = dados_treino)
hyper_grid <- expand.grid(
  minsplit = seq(5, 105, 10),
  maxdepth = seq(4, 12, 1)
)

get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}


#Buscando a melhor arvore
models <- list()
for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = venda_valor ~ .,
    data    = dados_treino,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)

optimal_tree <- rpart(
  formula = venda_valor~ .,
  data    = dados_treino,
  method  = "anova",
  control = list(minsplit = 15, maxdepth = 5, cp = 0.01)
)
rpart.plot(optimal_tree, type = 1, fallen.leaves = FALSE)

RMSE(predict(optimal_tree, newdata = dados_treino), dados$venda_valor[-trainIndex])
