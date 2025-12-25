#' @importFrom rlang list2
#' @importFrom stats pnorm
#'
#' @export
#'
#' @references Ahmad, R. (2017). Location-invariant test of homogeneity 
#' of large-dimensional covariance matrices. Journal of Statistical 
#' Theory and Practice, 11(4):731-745. 
#' \href{http://doi.org/10.1080/15598608.2017.1308895}{10.1080/15598608.2017.1308895}
#' @rdname homogeneityStatistics
Ahmad2017 <- function(x, ...){
  
  ls <- list2(...)
  
  samples <- order(sapply(x, function(dat){
    nrow(dat)
  }), decreasing = TRUE)
  
  matrix_ls <- x[samples]
  
  statistic <- Ahmad2017Stat(matrix_ls)

  xmin <- names(matrix_ls[1])
  xmax <- names(matrix_ls[length(matrix_ls)])
  xother <- names(matrix_ls[-c(1, length(matrix_ls))])

  data.name <- Reduce(paste0, past(xmin = xmin, xother, xmax = xmax))

  names(statistic) <- "Standard Normal"

  parameter <- c(0, 1)
  names(parameter) <- c("Mean", "Variance")

  null.value <- 0
  names(null.value) <- "difference in covariance matrices"

  p.value <- 1 - pnorm(abs(statistic))

  obj <- list(statistic = statistic,
              parameter = parameter,
              p.value = p.value,
              estimate = NULL,
              null.value = null.value,
              alternative = "two.sided",
              method = "Ahmad 2017 Homogeneity of Covariance Matrices Test",
              data.name = data.name)
  class(obj) <- "htest"
  obj
}
