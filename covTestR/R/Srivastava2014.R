#' @export
#' @importFrom rlang list2
#' @importFrom stats pchisq
#'
#' @references Srivastava, M., Yanagihara, H., and Kubokawa T. (2014). 
#' Tests for covariance matrices in high dimension with less sample size. 
#' Journal of Multivariate Analysis, 130:289-309. 
#' \href{http://doi.org/10.1016/j.jmva.2014.06.003}{10.1016/j.jmva.2014.06.003}
#' @rdname homogeneityStatistics
Srivastava2014 <- function(x, ...){

  ls <- list2(...)
  matrix_ls <- x
  statistic <- Srivastava2014Stat(matrix_ls)

  xmin <- names(matrix_ls[1])
  xmax <- names(matrix_ls[length(matrix_ls)])
  xother <- names(matrix_ls[-c(1, length(matrix_ls))])

  data.name <- Reduce(paste0, past(xmin = xmin, xother, xmax = xmax))

  names(statistic) <- "Chi-Squared"

  parameter <- 1
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
              method = "Srivastava et al. 2014 Homogeneity of Covariance Matrices Test",
              data.name = data.name)
  class(obj) <- "htest"
  obj
}
