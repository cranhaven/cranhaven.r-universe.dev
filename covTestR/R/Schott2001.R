#' @export
#' @importFrom rlang list2
#' @importFrom stats pchisq
#' 
#' @references Schott, J (2001). Some Tests for the Equality of Covariance
#' Matrices. Journal of Statistical Planniing and Inference. 94(1), 25-36. 
#' \href{http://doi.org/10.1016/S0378-3758(00)00209-3}{10.1016/S0378-3758(00)00209-3}
#' @rdname homogeneityStatistics
Schott2001 <- function(x, ...) {

  ls <- list2(...)
  matrix_ls <- x

  statistic <- Schott2001Stat(matrix_ls)

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
              method = "Schott 2001 Homogeneity of Covariance Matrices Test",
              data.name = data.name)
  class(obj) <- "htest"
  obj
}
