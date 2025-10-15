#' @title Missing Value Imputation
#'
#' @description
#'  This function performs missing value imputation in the input data using various methods.
#'  The available imputation methods are:
#'   
#'  - "mean": Imputes missing values with the mean of the variable.
#'  - "median": Imputes missing values with the median of the variable.
#'  - "mode": Imputes missing values with the mode of the variable (for categorical data).
#'  - "locf": Imputes missing values using the Last Observation Carried Forward method.
#'  - "knn": Imputes missing values using the k-Nearest Neighbors algorithm (specify k).
#'   
#' 
#' @param data Input data.
#' @param method Method of handling missing values: "mean," "median," "mode," "locf," or "knn."
#' @param k Value of the number of neighbors to be checked (only for knn method). Default is NULL.
#'
#' @return a data frame with imputed missing values 
#' @export
#' @importFrom caret preProcess
#' @import RANN
#' @examples 
#' data(airquality)
#' impute_missing(airquality, method='mean')
#'
impute_missing <- function(data, method = "mean", k = NULL) {
  imputed_data <- data
  
  if (is.null(k)) {
    imputed_data <- lapply(imputed_data, function(col) {
      if (method == "mean") {
        col[is.na(col)] <- mean(col, na.rm = TRUE)
      } else if (method == "median") {
        col[is.na(col)] <- median(col, na.rm = TRUE)
      } else if (method == "mode") {
        col[is.na(col)] <- impute_mode(col)
      } else if (method == "locf") {
        col <- impute_locf(col)
      }
      return(col)
    })
  } else if (method == "knn" && !is.null(k)) {
    imputed_data <- impute_knn(imputed_data, k)
  } else {
    stop("Invalid imputation method. Supported methods are: mean, median, mode, locf, knn")
  }
  
  return(as.data.frame(imputed_data))
}


impute_locf <- function(x) {
  imputed_values <- x
  lo <- NA
  
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      if (!is.na(lo)) {
        imputed_values[i] <- lo
      }
    } else {
      lo <- x[i]
    }
  }
  return(imputed_values)
}

impute_mode <- function(x) {
  tbl <- table(x)
  modes <- tbl[tbl == max(tbl)]
  mode_values <- as.numeric(names(modes))
  if (length(mode_values) != sum(is.na(x))) {
    mode_values <- rep(mode_values, length.out = sum(is.na(x)))
}
  return(mode_values)
}

impute_knn <- function(data, k) {

  imputed_values <- caret::preProcess(data, method = 'knnImpute', k = k)
  imputed_data <- predict(imputed_values, data)
  
  procNames <- data.frame(col = names(imputed_values$mean), mean = imputed_values$mean, sd = imputed_values$std)
  for (i in procNames$col) {
    imputed_data[i] <- imputed_data[i] * imputed_values$std[i] + imputed_values$mean[i]
  }
  
  return(imputed_data)
}


