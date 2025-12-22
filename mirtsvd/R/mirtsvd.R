#' @title Item Factor Analysis by Singular Value Decomposition
#'
#' @description Item Factor Analysis by Singular Value Decomposition
#'
#' @param data the data matrix. Entries are either binary or categorical.
#' Missing entries should be \code{NA}.
#' @param K the number of factors.
#' @param link the link function. Possible choices are "logit" and "probit".
#' @param epsilon the truncation parameter. Default value is 1e-4.
#' @param rotation_fn rotation applied to the estimated loading matrix.
#' See \code{\link[GPArotation]{rotations}}.
#' If \code{NULL}, no rotation would be applied.
#' @param ... optional arguments passed to rotation_fn.
#'
#' @return The function returns a list with the following components:
#' \describe{
#'   \item{loadings}{The estimated loading matrix.}
#'   \item{rotation}{The rotation method.}
#'   \item{type}{The data type.}
#'   \item{number}{The number of categories in data.}
#' }
#'
#' @export
#'
#' @import GPArotation mirtjml stats
#'
#' @examples
#' require(mirtjml)
#' require(GPArotation)
#'
#' # load a simulated dataset
#' attach(data_sim)
#'
#' data <- data_sim$response
#' K <- data_sim$K
#' res <- mirtsvd(data, K, rotation_fn = Varimax)
#'
#' @references Zhang, H., Chen, Y., & Li, X. (2020). A note on exploratory item factor
#'  analysis by singular value decomposition. Psychometrika, 1-15, \doi{10.1007/s11336-020-09704-7}.
mirtsvd <- function(data, K, link = "logit", epsilon = 1e-4,
                          rotation_fn = NULL, ...) {

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

  if (missing(K)) stop("argument \"K\" is missing, with no default")
  if (K >= J) stop("K >= J")

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
    m <- max(which(s1 >= threshold), K + 1)
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

  tmp2 <- svd(M.ave)
  s2 <- tmp2$d
  U2 <- tmp2$u
  V2 <- tmp2$v
  A <- (1 / sqrt(N)) * V2[, 1 : K] %*% diag(s2[1 : K], K, K)

  if (is.null(rotation_fn)) {
    rotation = NULL
  } else {
    tmp3 <- rotation_fn(A, ...)
    A <- tmp3$loadings
    rotation <- tmp3$method
  }
  if (num.cat == 2) {
    type <- 'Binary'
  } else {
    type <- 'Ordinal'
  }
  res <- list("loadings" = A,
              "rotation" = rotation,
              "type" = type,
              "number" = num.cat)
  return(res)

}
