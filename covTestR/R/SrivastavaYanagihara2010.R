#' @export
#' @importFrom rlang list2
#' @importFrom stats pchisq
#'
#' @references Srivastava, M. and Yanagihara, H. (2010). Testing the 
#' equality of several covariance matrices with fewer observation that 
#' the dimension. Journal of Multivariate Analysis, 101(6):1319-1329. 
#' \href{http://doi.org/10.1016/j.jmva.2009.12.010}{10.1016/j.jmva.2009.12.010}
#' @rdname homogeneityStatistics
SrivastavaYanagihara2010 <- function(x, ...){

  ls <- list2(...)
  matrix_ls <- x

  statistic <- SrivastavaYanagihara2010Stat(matrix_ls)

  xmin <- names(matrix_ls[1])
  xmax <- names(matrix_ls[length(matrix_ls)])
  xother <- names(matrix_ls[-c(1, length(matrix_ls))])

  data.name <- Reduce(paste0, past(xmin = xmin, xother, xmax = xmax))

  names(statistic) <- "Chi-Squared"

  parameter <- length(matrix_ls) - 1
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
              method = "Srivastava and Yanagihara 2010 Homogeneity of Covariance Matrices Test",
              data.name = data.name)
  class(obj) <- "htest"
  obj
}
