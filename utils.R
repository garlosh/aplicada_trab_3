library(dplyr)       
library(purrr)       
library(Hmisc)       
library(vcd)        
library(rcompanion)

is_categorical <- function(x){
  is.character(x) || is.factor(x)
}

calc_pearson_corr <- function(data, target_var) {
  numeric_vars <- data %>% select(where(is.numeric), -all_of(target_var))
  pearson_matrix <- cor(numeric_vars, use = 'pairwise.complete.obs')
  pearson_matrix[lower.tri(pearson_matrix, diag = TRUE)] <- NA
  return(pearson_matrix)
}

calc_cramer_corr <- function(data, target_var) {

  factor_vars <- data %>%
    select(where(is_categorical), -all_of(target_var)) %>%
    as.data.frame()

  factor_vars[] <- lapply(factor_vars, factor)
  factor_vars <- factor_vars %>%
    dplyr::select(where(~ nlevels(.x) > 1))
  
  var_names <- colnames(factor_vars)
  n_vars <- length(var_names)
  cramer_res <- matrix(
    data = NA,
    nrow = n_vars,
    ncol = n_vars,
    dimnames = list(var_names, var_names)
  )
  

  for (i in seq_len(n_vars)) {
    for (j in seq_len(n_vars)) {
       if (i < j) {
        if (nlevels(factor_vars[[i]]) > 1 && nlevels(factor_vars[[j]]) > 1) {
          tryCatch({cramer_res[i, j] <- cramerV(factor_vars[[i]], factor_vars[[j]])}, error = function(e){cramer_res[i, j] <- NA})
          
        } else {
          cramer_res[i, j] <- NA
        }

      }
    }
  }
  
  return(cramer_res)
}


calc_r2_vector <- function(data, target_var) {
  
  if (!is.numeric(data[[target_var]])) {
    stop("Exemplo apenas para target numérico.")
  }
  
  r2_list <- sapply(names(data), function(v) {
    if (v == target_var) {
      return(NA_real_)
    } else {
      
      if (is.numeric(data[[v]])) {
        return(cor(data[[v]], data[[target_var]], use = "complete.obs")^2)
      } else {
        
        mod <- lm(as.formula(paste(target_var, "~", v)), data = data)
        return(summary(mod)$r.squared)
      }
    }
  })
  
  names(r2_list) <- names(data)
  return(r2_list)
}


filter_variables <- function(data, 
                             cramer,     
                             pearson,    
                             target_var, 
                             threshold = 0.7) {
  
  
  r2_vec <- calc_r2_vector(data, target_var)
  
  
  teste <- function(mat, dt, r2, threshold) {
    for (i in seq_len(nrow(mat))) {
      for (j in seq(i + 1, ncol(mat))) {
        
        
        if (!is.na(mat[i, j]) && mat[i, j] >= threshold) {
          var_i <- rownames(mat)[i]
          var_j <- colnames(mat)[j]
          
          
          r2_i <- r2[var_i]
          r2_j <- r2[var_j]
          
          
          if (is.na(r2_i) || is.na(r2_j)) {
            var_to_remove <- ifelse(is.na(r2_i), var_i, var_j)
          } else {
            var_to_remove <- ifelse(r2_i < r2_j, var_i, var_j)
          }
          
          
          dt <- dt[, !(names(dt) %in% var_to_remove), drop = FALSE]
          r2[var_to_remove] <- NA_real_
          mat <- mat[
            !(rownames(mat) %in% var_to_remove),
            !(colnames(mat) %in% var_to_remove),
            drop = FALSE
          ]
          
          
          return(teste(mat, dt, r2, threshold))
        }
      }
    }
    
    
    return(list(mat = mat, dt = dt, r2 = r2))
  }
  
  
  factor_vars  <- names(data)[sapply(data, is.factor)]
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  
  
  common_factors <- intersect(factor_vars, rownames(cramer))
  cramer_subset  <- cramer[common_factors, common_factors, drop = FALSE]
  dt_factors     <- data[, common_factors, drop = FALSE]
  
  common_numeric <- intersect(numeric_vars, rownames(pearson))
  pearson_subset <- pearson[common_numeric, common_numeric, drop = FALSE]
  dt_numeric     <- data[, common_numeric, drop = FALSE]
  
  
  result_factor  <- teste(cramer_subset, dt_factors,  r2_vec, threshold)
  
  dt_factors <- result_factor$dt
  
  
  result_numeric <- teste(pearson_subset, dt_numeric, r2_vec, threshold)
  dt_numeric <- result_numeric$dt
  
  
  factor_ok  <- names(dt_factors)
  numeric_ok <- names(dt_numeric)
  
  final_vars <- unique(c(factor_ok, numeric_ok, target_var))
  final_vars <- intersect(final_vars, names(data))  
  
  final_data <- data[, final_vars, drop = FALSE]
  return(final_data)
}




filtrar_colunas_na <- function(df, r) {
  
  perc_na <- colMeans(is.na(df)) * 100
  
  
  df_filtrado <- df[, perc_na <= r]
  
  return(df_filtrado)
}

filtrar_variaveis_numericas <- function(df) {
  
  colunas_numericas <- sapply(df, is.numeric)
  
  
  variaveis_validas <- sapply(df[, colunas_numericas, drop = FALSE], function(col) {
    quantis <- quantile(col, probs = c(0.1, 0.9), na.rm = TRUE)
    return(quantis[1] != quantis[2])
  })
  
  
  df_filtrado <- cbind(df[!colunas_numericas], df[, colunas_numericas][, variaveis_validas, drop = FALSE])
  
  return(df_filtrado)
}

avaliar_modelo <- function(modelo, dados, tipo, response) {
  X_val <- as.matrix(dados[, predictors])
  y_val <- dados[[response]]
  
  preds <- switch(tipo,
                  "GLM" = predict(modelo, dados, type = "response"),
                  "XGB" = predict(modelo, X_val))
  
  rmse_value <- rmse(y_val, preds)
  #r2_value <- cor(y_val, preds)^2
  
  return(RMSE = rmse_value)
}

agrupa_categorias <- function(df, r) {
  for (col in names(df)) {
    if (is.character(df[[col]]) || is.factor(df[[col]])) {
      freq <- table(df[[col]]) / nrow(df)
      categorias_a_agrupar <- names(freq[freq < r])
      if(length(categorias_a_agrupar) > 1){
        df[[col]] <- ifelse(df[[col]] %in% categorias_a_agrupar, "Outros", df[[col]])
      }
      
    }
  }
  return(df)
}

substituir_na_por_ausentes <- function(df) {
  
  colunas_categoricas <- sapply(df, function(col) is.factor(col) || is.character(col))
  

  df[colunas_categoricas] <- lapply(df[colunas_categoricas], function(col) {
    col[is.na(col)] <- "Ausentes"
    return(col)
  })
  
  return(df)
}

