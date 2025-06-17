#' This function provides the p-value of an adaptation of the Portmanteau statistic to test if there is some dependence in the rows of the residuals matrix given as an argument of the function.
#'
#' @param  residuals the residuals matrix obtained by fitting a linear model to each column of the response matrix as if they were independent
#' @return the p-value of a whitening test. If the p-value is small (generally smaller than 0.05)
#' it means that the hypothesis that each row of the  residuals matrix is a white noise, is rejected.
#' @examples
#' data(copals_camera)
#' Y <- scale(Y[, 1:100])
#' X <- model.matrix(~ group + 0)
#' residuals <- lm(as.matrix(Y) ~ X - 1)$residuals
#' whitening_test(residuals)
#' @export
whitening_test <-
  function(residuals) {
    n <- dim(residuals)[1]
    q <- dim(residuals)[2]
    ## Portmanteau test on each row of the residuals
    recup_stat <- function(k) {
      acf_vect <- acf(as.vector(residuals[k, ]), type = "correlation", plot = FALSE, lag.max = floor(sqrt(q)))$acf
      return(q * sum(acf_vect[2:length(acf_vect)]^2))
    }
    Stat <- sapply(1:n, recup_stat)
    pvalue <- pchisq(sum(Stat), df = (floor(sqrt(q)) * n), lower.tail = FALSE)

    return(pvalue)
  }
