#' @title Scree plot for singular values.
#'
#' @description Scree plot for singular values.
#'
#' @param data the data matrix. Entries are either binary or categorical.
#' Missing entries should be \code{NA}.
#' @param link the link function. Possible choices are "logit" and "probit".
#' @param epsilon the truncation parameter. Default value is 1e-4.
#' @param K_max The maximum number of factors contained in data. Default value is 10.
#'
#' @export
#'
#' @import mirtjml stats graphics
#'
#' @examples
#' require(mirtjml)
#'
#' # load a simulated dataset
#' attach(data_sim)
#'
#' data <- data_sim$response
#' screeplot_svd(data, K_max = 10)
#'
#' @references Zhang, H., Chen, Y., & Li, X. (2020). A note on exploratory item factor
#'  analysis by singular value decomposition. Psychometrika, 1-15, \doi{10.1007/s11336-020-09704-7}.
screeplot_svd <- function(data, link = "logit", epsilon = 1e-4, K_max = 10) {

  if(missing(data)) stop("argument \"data\" is missing, with no default")
  N <- nrow(data)
  J <- ncol(data)
  if (N < J) stop("N < J")

  data.na <- is.na(data)
  num.na <- sum(data.na)
  W <- 1 - as.numeric(data.na)
  p <- sum(W) / (N * J)

  uni <- unique(c(data))
  uni <- uni[which(is.na(uni) == FALSE)]
  uni <- sort(uni)
  num.cat <- length(uni)
  if (num.cat <= 1) stop("only 1 category")

  if (K_max >= J) stop("K_max >= J")

  if ((link != "logit") & (link != "probit")) stop("unknown link function")

  M.ave <- matrix(0, N, J)
  for (t in 1 : (num.cat - 1)) {

    Z <- data
    Z[which(data.na == TRUE)] <- 0
    Z[which(data >= uni[t+1])] <- 1
    Z[which(data < uni[t+1])] <- 0

    tmp1 <- svd(Z)
    s1 <- tmp1$d
    U1 <- tmp1$u
    V1 <- tmp1$v
    threshold <- 1.01 * sqrt(N * (p + 3 * p * (1 - p)))
    m <- max(which(s1 >= threshold), K_max + 1)
    X <- (1 / p) * U1[, (1 : m)] %*% diag(s1[1 : m], m, m) %*% t(V1[, (1 : m)])

    X[X > 1 - epsilon] <- 1 - epsilon
    X[X < epsilon] <- epsilon

    M <- matrix(0, N, J)
    if (link == "logit") {
      M <- log(X / (1 - X))
    } else M <- qnorm(X)

    d <- colMeans(M)
    M <- M - rep(1, N) %*% t(d)
    M.ave <- (M.ave * (t - 1) + M) / t

  }

  sigma <- svd(M.ave)$d / sqrt(N * J)
  plot(sigma[1 : 25], type = "l", main = "Scree plot", xlab = "Number of factors", ylab = "Standarized singular values")
  points(sigma[1 : 25], pch = 15)

}
