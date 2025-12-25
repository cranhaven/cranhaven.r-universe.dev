#' @importFrom rlang list2
#' @importFrom stats pchisq
#'
#' @export
#'
#' @references Chaipitak, S. and Chongcharoen, S. (2013). A test for 
#' testing the equality of two covariance matrices for high-dimensional 
#' data. Journal of Applied Sciences, 13(2):270-277. 
#' \href{http://doi.org/10.3923/jas.2013.270.277}{10.3923/jas.2013.270.277}
#' @rdname homogeneityStatistics
Chaipitak2013 <- function(x, ...){

  ls <- list2(...)
  matrix_ls <- x

  statistic <- Chaipitak2013Stat(matrix_ls)

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
              method = "Chaipitak and Chongchareon 2013 Homogeneity of Covariance Matrices Test",
              data.name = data.name)
  class(obj) <- "htest"
  obj
}
