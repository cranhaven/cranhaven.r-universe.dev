#' @export
#' @importFrom rlang list2
#' @importFrom stats pchisq
#'
#' @references Schott, J. (2007). A test for the equality of covariance 
#' matrices when the dimension is large relative to the sample sizes. 
#' Computational Statistics & Data Analysis, 51(12):6535-6542. 
#' \href{http://doi.org/10.1016/j.csda.2007.03.004}{10.1016/j.csda.2007.03.004}
#' @rdname homogeneityStatistics
Schott2007 <- function(x, ...) {

  ls <- list2(...)
  matrix_ls <- x

  statistic <- Schott2007Stat(matrix_ls)

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
              method = "Schott 2007 Homogeneity of Covariance Matrices Test",
              data.name = data.name)
  class(obj) <- "htest"
  obj
}

