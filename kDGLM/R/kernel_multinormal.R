#### Multi normal ####

#' convert_multi_NG_Normal
#'
#' Calculate the parameters of the Normal-Gamma that best approximates the given Multivariate Normal distribution. The distribution obtained for each outcome is marginal.
#' The approximation is the best in the sense that it minimizes the KL divergence from the Normal to the Normal-Gamma.
#' In this approach, we suppose that the first entry of the multivariate normal represents the mean of the observed data and the second represent the log variance.
#'
#' @param ft numeric: A vector representing the means from the normal distribution.
#' @param Qt matrix: A matrix representing the covariance matrix of the normal distribution.
#' @param parms list: A list of extra known parameters of the distribution. Not used in this kernel.
#'
#' @return The parameters of the conjugated distribution of the linear predictor.
#' @keywords internal
#'
#' @family auxiliary functions for a Normal outcome
convert_multi_NG_Normal <- function(ft, Qt, parms) {
  k <- length(ft)
  r <- -3 / 2 + sqrt(9 / 4 + 2 * k)
  mu.index <- parms$mu.index
  var.index <- parms$var.index

  ft <- matrix(ft, k, 1)
  ft.mean <- ft[mu.index]
  ft.var <- ft[var.index]

  Qt.diag <- diag(Qt)
  Qt.mean <- Qt.diag[mu.index]
  Qt.var <- Qt.diag[var.index]

  if (r == 1) {
    Qt.cov <- Qt[1, 2]
  } else {
    Qt.cov <- diag(Qt[mu.index, var.index])
  }

  mu0 <- ft.mean + Qt.cov
  c0 <- exp(-ft.var - Qt.var / 2) / (Qt.mean + 1e-40)
  helper <- -3 + 3 * sqrt(1 + 2 * Qt.var / 3)
  # helper=Qt[2,2]
  alpha <- 1 / helper
  beta <- alpha * exp(-ft.var - Qt.var / 2)

  vec.par <- c(rbind(mu0, c0, alpha, beta))

  return(vec.par)
}

convert_multi_Normal_NG <- function(conj.param, parms = list()) {
  r <- dim(conj.param)[2] / 4
  k <- r + r * (r + 1) / 2

  mu0 <- conj.param[, seq.int(1, r * 4 - 1, 4)]
  c0 <- conj.param[, seq.int(1, r * 4 - 2, 4) + 1]
  alpha0 <- conj.param[, seq.int(1, r * 4 - 3, 4) + 2]
  beta0 <- conj.param[, seq.int(1, r * 4, 4) + 3]

  f1 <- mu0
  f2 <- digamma(alpha0) - log(beta0 + 1e-40)
  q1 <- beta0 / (c0 * alpha0)
  q2 <- trigamma(alpha0)
  q12 <- 0
  if (r == 1) {
    ft <- c(f1, f2)
    Qt <- matrix(c(q1, q12, q12, q2), byrow = F, ncol = 2)
  } else {
    ft <- c(f1, diag(f2)[upper.tri(diag(f2))])
    Qt <- diag(k)
    diag(Qt) <- c(q1, diag(q2)[upper.tri(diag(q2))])
  }
  return(list("ft" = ft, "Qt" = Qt))
}

#
#' update_multi_NG_correl
#'
#' @importFrom Rfast lower_tri upper_tri lower_tri.assign upper_tri.assign
#'
#' @return The parameters of the posterior distribution.
#' @keywords internal
update_multi_NG_correl <- function(conj.param, ft, Qt, y, parms) {
  # parms=outcome$parms

  # y=c(1,NA,8,NA,4)
  # r=length(y)
  # k=r + r * (r + 1) / 2
  # ft=rep(0,k) |> matrix(k,1)
  # Qt=diag(k)

  # parms=list(mu.index=1:5,
  #            var.index=1:5+5,
  #            var.index=(2*5+1):(5 + 5 * (5 + 1) / 2),
  #            alt.method=TRUE
  #            )

  #
  # for(index in 1:5){
  #   ft=ft.up
  #   Qt=Qt.up
  # y=outcome$data[index,]

  r <- length(y)

  flags <- is.na(y)
  y.index <- c((1:r)[!flags], (1:r)[flags])
  y <- y[y.index]
  r.valid <- sum(!flags)

  Var <- matrix(NA, r, r)
  Var.index <- lower.tri(Var)
  Var[Var.index] <- 1:sum(Var.index)
  Var[t(Var.index)] <- t(Var)[t(Var.index)]
  Var <- Var[y.index, y.index]

  index.order <- c(
    (1:r)[!flags], (1:r)[flags],
    (1:r)[!flags] + r, (1:r)[flags] + r,
    Var[Var.index] + 2 * r
  )
  index.reorder <- order(index.order)
  # print(index.order)

  ft <- ft[index.order, ]
  Qt <- Qt[index.order, index.order]

  mu.index <- parms$mu.index
  var.index <- parms$var.index
  cor.index <- parms$cor.index
  upper.index <- parms$upper.index
  lower.index <- parms$lower.index
  alt.method <- parms$alt.method
  r.index <- mu.index

  r <- length(y)
  k <- r + r * (r + 1) / 2
  vec.r <- 1:(r**2)

  mu.index <- seq_len(r)
  var.index <- mu.index + r
  cor.index <- (2 * r + 1):k

  # lower.index <- matrix(seq_len(r**2), r, r)[Var]
  # upper.index <- t(matrix(seq_len(r**2), r, r))[Var]


  ft.up <- ft
  Qt.up <- Qt

  A <- matrix(0, k, 2)
  A[1, 1] <- A[r + 1, 2] <- 1
  ft.now <- ft.up[c(1, r + 1)]
  Qt.now <- Qt.up[c(1, r + 1), c(1, r + 1)]

  if (parms$alt.method) {
    # post <- update_NG_alt(param, ft.now, Qt.now, y[1])
    # post <- update_NG_gauss(param, ft.now, Qt.now, y[1])
  } else {
    param <- convert_NG_Normal(ft.now, Qt.now)
    up.param <- update_NG(param, ft.now, Qt.now, y[1])
    post <- convert_Normal_NG(up.param)
  }

  ft.post <- post$ft
  Qt.post <- post$Qt
  # print('\n#################### first #######################')
  # print(Qt.now)
  # print(Qt.post)

  At <- Qt.up[, c(1, r + 1)] %*% ginv(Qt.now)
  At.t <- t(At)
  ft.up <- ft.up + At %*% (ft.post - ft.now)
  Qt.up <- Qt.up + At %*% (Qt.post - Qt.now) %*% At.t

  if (r.valid > 1) {
    for (i in 2:(r.valid)) {
      {      x <- c(ft.up)
        rho <- matrix(0, r, r)
        rho[upper.index] <- rho[lower.index] <- tanh(x[cor.index])
        sd <- diag(exp(-x[var.index] / 2))
        diag(rho) <- 1
        Sigma <- sd %*% rho %*% sd
        # diag(rho)=diag(sd)
        # Sigma=rho%*%t(rho)
        # Sigma=crossprod(transpose(rho))
        # print(eigen(Sigma))
        # print(Sigma)

        i.seq <- seq_len(i - 1)

        Sigma.rho <- Sigma[i, i.seq]
        Sigma.part <- Sigma[i.seq, i.seq]

        if (all(Sigma.part == 0)) {
          ft.now <- x[c(i, i + r)]
          Qt.now <- Qt.up[c(i, i + r), c(i, i + r)]
        } else {
          S <- ginv(Sigma.part)
          e <- (y[i.seq] - x[i.seq])
          Sigma.S <- c(Sigma.rho %*% S)
          mu_bar <- x[i] + Sigma.S %*% e
          S_bar <- Sigma[i, i] - Sigma.S %*% Sigma.rho

          A <- matrix(0, k, 2)
          A[1:i, 1] <- c(-Sigma.S, 1)

          dx <- array(0, c(r, r, r * (r + 1) / 2))
          for (j in 1:i) {
            dx[j, j, j] <- -0.5 * sd[j, j]
          }
          ref.rho <- c(rho)[upper.index]
          dx[vec.r[upper.index] + c(0:(k - 2 * r - 1)) * (r**2) + (r**3)] <-
            dx[vec.r[lower.index] + c(0:(k - 2 * r - 1)) * (r**2) + (r**3)] <-
            (1 + ref.rho) * (1 - ref.rho)

          dSigma <- array(NA, c(r, r, r * (r + 1) / 2))
          aux.1 <- rho %*% sd

          dSigma[, , r.index] <- array_mult_left(dx[, , r.index], aux.1)

          dSigma[, , r.index] <- dSigma[, , r.index] + array_transp(dSigma[, , r.index])
          dSigma[, , -r.index] <- dx[, , -r.index, drop = FALSE] |>
            array_mult_left(sd) |>
            array_mult_right(sd)

          dSigma.part <- dSigma[i.seq, i.seq, , drop = FALSE]
          dSigma.rho <- dSigma[i, i.seq, ] |> matrix(i - 1, r * (r + 1) / 2)

          dSigma.p1 <- -array_mult_right(dSigma.part, S)
          dSigma.p1 <- array_mult_left(dSigma.p1, S)
          dSigma.p1 <- array_collapse_left(dSigma.p1, e)
          dSigma.p1 <- c(Sigma.rho %*% dSigma.p1)

          dSigma.p2 <- c(c(S %*% e) %*% dSigma.rho)
          A[-r.index, 1] <- dSigma.p1 + dSigma.p2

          dSigma.p1 <- -array_collapse_right(dSigma.part, Sigma.S)
          dSigma.p1 <- c(Sigma.S %*% dSigma.p1)

          helper.p2 <- c(S %*% Sigma.rho)
          dSigma.p2 <- 2 * c(helper.p2 %*% dSigma.rho)
          A[-r.index, 2] <- -(dSigma[i, i, ] - dSigma.p1 - dSigma.p2) / c(S_bar)

          ft.now <- c(mu_bar, -log(S_bar))
          Qt.now <- t(A) %*% Qt.up %*% A
        }
      }
      ###################################
      # {f=function(x){
      #
      #   mu <- x
      #   rho <- matrix(0, r, r)
      #   rho <- lower_tri.assign(rho, x[cor.index], diag = FALSE)
      #   rho <- upper_tri.assign(rho, x[cor.index], diag = FALSE)
      #   sd <- diag(exp(-x[var.index] / 2))
      #   rho <- tanh(rho)
      #   diag(rho)=1
      #   Sigma <- sd %*% rho %*% sd
      #
      #   S=ginv(Sigma[1:(i-1),1:(i-1)])
      #   mu_bar=mu[i]+Sigma[i,1:(i-1)]%*%S%*%(y[1:(i-1)]-mu[1:(i-1)])
      #   S_bar=Sigma[i,i]-Sigma[i,1:(i-1)]%*%S%*%Sigma[1:(i-1),i]
      #   # Sigma.S=solve(Sigma[1:(i-1),1:(i-1)],Sigma[1:(i-1),i])
      #   # mu_bar=mu[i]+Sigma.S%*%(y[1:(i-1)]-mu[1:(i-1)])
      #   # S_bar=Sigma[i,i]-Sigma.S%*%Sigma[1:(i-1),i]
      #   return(c(mu_bar,-log(S_bar)))
      # }
      #
      # A_test=t(calculus::derivative(f,ft.up))
      # # ft.now=f(ft.up)
      # # Qt.now=t(A)%*%Qt.up%*%A
      # print('a')
      # print(A)
      # print(A_test)
      # print(max(abs(A-A_test)))
      # }
      ####################################

      if (parms$alt.method) {
        # post <- update_NG_alt(param, ft.now, Qt.now, y[i])
        # post <- update_NG_gauss(param, ft.now, Qt.now, y[i])
      } else {
        param <- convert_NG_Normal(ft.now, Qt.now)
        up.param <- update_NG(param, ft.now, Qt.now, y[i])
        post <- convert_Normal_NG(up.param)
      }

      ft.post <- post$ft
      Qt.post <- post$Qt
      # print('\n#################### second #######################')
      # print(Qt.now)
      # print(Qt.post)
      # print(Qt.up)

      # print(A)
      # print(Qt.now)
      At <- Qt.up %*% A %*% ginv(Qt.now)
      At.t <- t(At)

      ft.up <- ft.up + At %*% (ft.post - ft.now)
      Qt.up <- Qt.up + At %*% (Qt.post - Qt.now) %*% At.t
    }
    # ft_star[,index]=ft.up
    # Qt_star[,,index]=Qt.up
  }
  # }


  ft.up <- ft.up[index.reorder, ]
  Qt.up <- Qt.up[index.reorder, index.reorder]

  # print(ft.up)

  return(list("ft" = ft.up, "Qt" = Qt.up))
}

##' update_multi_NG_chol
# #'
# #' @importFrom Rfast lower_tri upper_tri lower_tri.assign upper_tri.assign
# update_multi_NG_chol <- function(conj.param, ft, Qt, y, parms) {
#   mu.index <- parms$mu.index
#   var.index <- parms$var.index
#   cor.index <- parms$cor.index
#   upper.index <- parms$upper.index
#   lower.index <- parms$lower.index
#   alt.method <- parms$alt.method
#   r.index <- mu.index
#
#   r <- length(y)
#   k <- r + r * (r + 1) / 2
#   vec.r <- 1:(r**2)
#
#   ft.up <- ft
#   Qt.up <- Qt
#
#   for (i in 1:r) {
#     {
#       f <- function(x) {
#         mu <- x
#         rho <- matrix(0, r, r)
#         rho[upper.index] <- rho[lower.index] <- x[cor.index]
#         # rho[upper.index] <- rho[lower.index] <- 2*exp(x[cor.index])/(1+sum(exp(x[cor.index])))-1
#         sd <- diag(exp(-x[var.index] / 2))
#         rho <- tanh(rho)
#         diag(rho) <- 1
#         Sigma <- sd %*% rho %*% sd
#         # diag(rho)=diag(sd)
#         # Sigma=rho%*%t(rho)
#         # Sigma=crossprod(transpose(rho))
#         # print(eigen(Sigma))
#
#         if (i == 1) {
#           mu_bar <- mu[i]
#           S_bar <- Sigma[i, i]
#         } else {
#           S <- ginv(Sigma[1:(i - 1), 1:(i - 1)])
#           mu_bar <- mu[i] + Sigma[i, 1:(i - 1)] %*% S %*% (y[1:(i - 1)] - mu[1:(i - 1)])
#           S_bar <- Sigma[i, i] - Sigma[i, 1:(i - 1)] %*% S %*% Sigma[1:(i - 1), i]
#         }
#         # print(S_bar)
#         return(c(mu_bar, -log(S_bar)))
#         # return(c(Sigma))
#       }
#
#       A_test <- t(derivative(f, var = ft.up))
#       A <- A_test
#       ft.now <- f(ft.up)
#       Qt.now <- t(A) %*% Qt.up %*% A
#       # print(ft.now)
#       # print(max(abs(A - A_test)))
#     }
#     ####################################
#
#     if (parms$alt.method) {
#       post <- update_NG_alt(param, ft.now, Qt.now, y[i])
#     } else {
#       param <- convert_NG_Normal(ft.now, Qt.now)
#       up.param <- update_NG(param, ft.now, Qt.now, y[i])
#       post <- convert_Normal_NG(up.param)
#     }
#
#     ft.post <- post$ft
#     Qt.post <- post$Qt
#     # print('\n#################### second #######################')
#     # print(Qt.now)
#     # print(Qt.post)
#     # print(Qt.up)
#
#     At <- Qt.up %*% A %*% ginv(Qt.now)
#     At.t <- t(At)
#
#     ft.up <- ft.up + At %*% (ft.post - ft.now)
#     Qt.up <- Qt.up + At %*% (Qt.post - Qt.now) %*% At.t
#   }
#   # ft_star[,index]=ft.up
#   # Qt_star[,,index]=Qt.up
#   # }
#
#
#
#   return(list("ft" = ft.up, "Qt" = Qt.up))
# }

#' multi_normal_gamma_pred
#'
#' @param conj.param list or data.frame: The parameters of the conjugated distribution (Normal-Gamma) of the linear predictor.
#' @param outcome numeric or matrix (optional): The observed values at the current time.
#' @param parms list (optional): A list of extra parameters for the model. Not used in this function.
#' @param pred.cred numeric: the desired credibility for the credibility interval.
#'
#' @return A list containing the following values:
#' \itemize{
#'    \item pred numeric/matrix: the mean of the predictive distribution of a next observation. Same type and shape as the parameter in model.
#'    \item var.pred numeric/matrix: the variance of the predictive distribution of a next observation. Same type and shape as the parameter in model.
#'    \item icl.pred numeric/matrix: the percentile of 100*((1-pred.cred)/2)% of the predictive distribution of a next observation. Same type and shape as the parameter in model.
#'    \item icu.pred numeric/matrix: the percentile of 100*(1-(1-pred.cred)/2)% of the predictive distribution of a next observation. Same type and shape as the parameter in model.
#'    \item log.like numeric: the The log likelihood for the outcome given the conjugated parameters.
#' }
#'
#' @importFrom stats qt dt
#' @importFrom Rfast data.frame.to_matrix
#' @keywords internal
#'
#' @family auxiliary functions for a Normal outcome
multi_normal_gamma_pred <- function(conj.param, outcome = NULL, parms = list(), pred.cred = 0.95) {
  pred.flag <- !is.na(pred.cred)
  like.flag <- !is.null(outcome)
  if (!like.flag && !pred.flag) {
    return(list())
  }

  t <- if.null(dim(conj.param)[1], 1)
  r <- if.null(dim(conj.param)[2], length(conj.param)) / 4
  k <- r + r * (r + 1) / 2

  # conj.param <- data.frame.to_matrix(conj.param)
  conj.param <- matrix(unlist(conj.param, use.names = FALSE), t, 4 * r)
  if (dim(conj.param)[2] == 1) {
    conj.param <- conj.param |> t()
  }

  mu0 <- conj.param[, seq.int(1, r * 4 - 1, 4), drop = FALSE]
  c0 <- conj.param[, seq.int(2, r * 4 - 1, 4), drop = FALSE]
  alpha0 <- conj.param[, seq.int(3, r * 4 - 1, 4), drop = FALSE]
  beta0 <- conj.param[, seq.int(4, r * 4, 4), drop = FALSE]

  nu <- 2 * alpha0
  sigma2 <- (beta0 / alpha0) * (1 + 1 / c0)
  s <- sqrt(sigma2)

  if (pred.flag) {
    pred <- mu0 |> t()
    if (r == 1) {
      var.pred <- array(sigma2, c(1, 1, t))
    } else {
      var.pred <- array(0, c(r, r, t))
      for (i in seq_len(r)) {
        var.pred[i, i, ] <- sigma2[, i]
      }
    }

    icl.pred <- (qt((1 - pred.cred) / 2, nu) * s + mu0) |> t()
    icu.pred <- (qt(1 - (1 - pred.cred) / 2, nu) * s + mu0) |> t()
  } else {
    pred <- NULL
    var.pred <- NULL
    icl.pred <- NULL
    icu.pred <- NULL
  }
  if (like.flag) {
    outcome <- matrix(outcome, t, r)
    # warning("The log-likelihood is not being calculated properly. Each outcome is computed independently.")
    log.like <- colSums(dt((outcome - mu0) / s, nu, log = TRUE) - log(s))
  } else {
    log.like <- NULL
  }

  list(
    "pred"     = pred,
    "var.pred" = var.pred,
    "icl.pred" = icl.pred,
    "icu.pred" = icu.pred,
    "log.like" = log.like
  )
}
