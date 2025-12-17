#' @importFrom Matrix Matrix
#' @importFrom Rmosek mosek


cmaRs.mosek_optimization <- function(i, numBF, sqrtz,
                                     Tm, l, alpha, z, L, t,
                                     BasisFunctions, n, D, y)
                                     # this function estimates the parameters
                                     # of the cmars
                                     # model for different sqrtz values
                                     # i: counter
                                     # numBF: total number of basis functions
                                     # sqrtz: the upper bound of the
                                     # second constraint
                                     # Tm:estimated parameters of
                                     # the current model
                                     # l: number of BFs in the current model
                                     # alpha: vector of number of parameters
                                     # in for all sqrtz values
                                     # z: vector of RSS values for
                                     # all sqrtz values
                                     # t: vector of norm L Theta values for
                                     # all sqrtz values
                                     # BasisFunctions: design matrix
                                     # n: sample size of the data
                                     # D: preparation for optimization
# y:response variable
{
  # setup conic quadratic programming problem
  cqo1 <- list(sense = "min")
  # objective function
  cqo1$c <- rbind(1, matrix(0, nrow <- 2 * numBF + n + 2, ncol <- 1))
  cqo1$A <- Matrix::Matrix(D, sparse = TRUE)
  blc <- c(t(y), matrix(0, nrow <- 1, ncol <- numBF))
  buc <- c(t(y), matrix(0, nrow <- 1, ncol <- numBF))
  cqo1$bc <- rbind(blc, buc)
  ppp <- c(0)
  for (j in 2:(2 * numBF + n + 2))
  {
    ppp <- c(ppp, -Inf)
  }
  qqq <- c(Inf)
  for (j in 2:(2 * numBF + n + 2))
  {
    qqq <- c(qqq, Inf)
  }
  blx <- c(ppp, sqrtz[i])
  bux <- c(qqq, sqrtz[i])
  cqo1$bx <- rbind(blx, bux)
  numcones <- 2
  cqo1$cones <- matrix(list(), nrow <- 2, ncol <- numcones)
  rownames(cqo1$cones) <- c("type", "sub")
  concones1 <- c(1, (numBF + 3):(numBF + n + 2))
  cqo1$cones[, 1] <- list("QUAD", concones1)
  concones2 <- c(
    (2 * numBF + n + 3),
    (numBF + n + 3):(2 * numBF + n + 2)
  )

  cqo1$cones[, 2] <- list("QUAD", concones2)
  r <- Rmosek::mosek(cqo1)
  ans <- r$sol$itr$xx
  Theta <- ans[2:(numBF + 2)]
  Tm <- rbind(Tm, Theta)
  l <- length(which((abs(Theta) > 0.001))) - 1
  alpha <- c(alpha, l)
  m <- BasisFunctions %*% Theta # predicted response
  z <- c(z, sqrt(t((y - m)) %*% (y - m)))
  zz <- t(z)
  a <- L %*% Theta
  t <- c(t, sqrt(t(a) %*% a))
  tt <- t(t)

  return(list(
    Theta = Theta, m = m, t = t,
    z = z, BasisFunctions = BasisFunctions
  )) # output
} # end of function mosek_optimization
