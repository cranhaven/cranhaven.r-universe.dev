#' general.indices
#'
#' @description calculates indices to measure accuracy of a model.
#'
#' @param real the real values in traning-testing.
#' @param prediccion the prediction values in traning-testing.
#'
#' @return a list with the Correlation, Relative Error, Mean Absolute Error and Root Mean Square Error.
#' @export
#'
#' @examples
#' real <- rnorm(45)
#' prediction <- rnorm(45)
#' model <- "KNN"
#' general.indices(real, prediction)
#' 
general.indices <- function(real, prediccion) {
  RMSE <- sqrt(sum((real - prediccion) ^ 2) / length(prediccion))
  MAE  <- sum(abs(real - prediccion)) / length(prediccion)
  RE   <- sum(abs(real - prediccion)) / sum(abs(real)) * 100
  desvStand <- sd(prediccion)
  COR  <- ifelse(near(desvStand,0), 0, as.numeric(cor(real, prediccion)))
  COR  <- ifelse(is.na(COR), 0 , COR)
  indices <- list(Raiz.Error.Cuadratico = RMSE,
                  Error.Absoluto = MAE,
                  Error.Relativo = RE,
                  Correlacion = COR)
  return(indices)
}

rmse = function(real, prediccion) {
  return(sqrt(mean((real - prediccion) ^ 2)))
}

tabla.indicesPrecision <- function(indices, decimals = NULL, idioma){
  df <- as.data.frame(indices)
  if(!is.null(decimals)){
    df <- round(df, digits = decimals)
  }
  #Esto es necesario debido a problema con la cantidad de decimales
  #con la función renderTable
  df[,] <- sapply(df[,], as.character)
  
  colnames(df) <- c(tr("RMSE", idioma), tr("MAE", idioma), tr("ER", idioma), tr("correlacion", idioma))
  return(df)
}

#' summary_indices
#'
#' @description summarizes a variable by returning the minimum, first quartile, third quartile and maximum value.
#'
#' @param data a numeric vector. 
#'
#' @export
#'
#' @examples
#' summary_indices(iris$Sepal.Length)
#' 
summary_indices <- function(data){
  list("Min" = min(data),
       "1Q"  = quantile(data, prob=c(0.25)),
       "3Q"  = quantile(data, prob=c(0.75)),
       "Max" = max(data))
}

tabla.varpred.summary <- function(summary.var, decimals = NULL, idioma = "es"){
  df <- as.data.frame(summary.var)
  
  if(!is.null(decimals)){
    df <- round(df, digits = decimals)
  }
  #Esto es necesario debido a problema con la cantidad de decimales
  #con la función renderTable
  df[,] <- sapply(df[,], as.character)
  
  colnames(df) <- c(tr("minimo",idioma),tr("q1",idioma),
                     tr("q3",idioma),tr("maximo",idioma))
  return(df)
}