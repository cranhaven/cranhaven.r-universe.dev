# Obtener moda
moda <- function(x) {
  names(which.max(table(x)))
}

# Obtiene todos los indices de los grupos.
calc_cross_groups <-  function(x) {
  pred <- c()
  test <- data.frame()
  for (g in names(x)) {
    pred <- c(pred, x[[g]]$pred$prediction)
    var.pred <- x[[g]]$pred$var.pred
    test <- rbind(test, x[[g]]$test)
  }
  
  pred <- list(prediction = pred, var.pred = var.pred)
  res  <- traineR::general.indexes(test, pred)
  
  return(res)
}

# Obtiene todos los indices promedio.
calc_cross_index <-  function(x) {
  res <- 0
  n   <- 0
  for (vc in names(x)) {
    indices <- data.frame(calc_cross_groups(x[[vc]]))
    res <- res + indices
    n <- n + 1
  }
  
  return(res / n)
}

# Obtiene la tabla para los grupos.
table_group <-  function(x, grupos) {
  res <- data.frame()
  
  for (g in names(x)) {
    gi  <- as.numeric(gsub("G", "", g))
    ids <- grupos[[gi]]
    
    nuevo <- data.frame(
      ids  = grupos[[gi]],
      pred = x[[g]]$pred$prediction
    )
    
    res <- rbind(res, nuevo)
  }
  
  res <- res[order(res$ids), ]
  
  return(res$pred)
}

# Obtiene la probabilidad promedio.
cross_group <-  function(x, grupos) {
  pred <- 0
  n    <- 0
  for (vc in names(x)) {
    vci  <- as.numeric(gsub("VC", "", vc))
    pred <- pred + table_group(x[[vc]], grupos[[vci]])
    n    <- n + 1
  }
  
  pred <- pred / n
  return(pred)
}

# Obtiene todos los errores promedio.
calc_cross_error <-  function(x, ind = "RMSE") {
  res <- data.frame()
  for (k in names(x)) {
    for (vc in names(x[[k]])) {
      errores    <- calc_cross_groups(x[[k]][[vc]])
      res[k, vc] <- errores[[ind]] 
    }
  }
  
  return(res)
}

# Obtiene todos los errores promedio train-test.
calc_cross_error_tt <-  function(x, ind = "RMSE") {
  res <- data.frame()
  for (vc in 1:length(x)) {
    k          <- x[[vc]]$nombre
    error      <- x[[vc]]$indices[[ind]]
    res[k, vc] <- error
  }
  
  return(res)
}
