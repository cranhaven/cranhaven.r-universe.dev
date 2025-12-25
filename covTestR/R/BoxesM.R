#' @export
#' 
#' @importFrom rlang list2
#' 
#' @rdname homogeneityStatistics
BoxesM <- function(x, ...){
  ls <- list2(...)
  matrix_ls <- x
  statistic <- BoxesMStat(matrix_ls)
  
  xmin <- names(matrix_ls[1])
  xmax <- names(matrix_ls[length(matrix_ls)])
  xother <- names(matrix_ls[-c(1, length(matrix_ls))])
  
  data.name <- Reduce(paste0, past(xmin = xmin, xother, xmax = xmax))
  
  
  
  names(statistic) <- "Chi-Squared"
  
  p <- nrow(matrix_ls[[1]])
  parameter <- (length(matrix_ls) - 1) * p * (p + 1) / 2 
  names(parameter) <- "df"
  
  null.value <- 0
  names(null.value) <- "difference in covariance matrices"
  
  p.value <- 1 - pchisq(statistic, parameter)
  
  obj <- list(statistic = statistic,
              parameter = parameter,
              p.value = p.value,
              estimate = NULL,
              null.value = null.value,
              alternative = "two.sided",
              method = "Boxes' M Homogeneity of Covariance Matrices Test",
              data.name = data.name)
  class(obj) <- "htest"
  obj
}

