##
#' Homogeneity Detection Incorporating Prior Constraint Information by ADMM
#'
#' simultaneous homogeneity detection and variable selection incorporating prior constraint by ADMM
#' algorithm. The problem turn to solving quadratic programming problems of the form
#' \emph{\min(-d^T b + 1/2 b^T D b)} with the constraints \emph{A^T b >= b_0}. The penalty is the pairwise
#' fusion with p(p-1)/2 number of penalties.
#'
#'
#'
#'
#' @param Y n-by-1 response matrix.
#' @param X n-by-p design matrix.
#' @param lambda2 penalty tuning parameter for thresholding function.
#' @param A.eq  equality constraint matrix.
#' @param A.ge  inequality constraint matrix.
#' @param A.lbs low-bounds matrix on variables, see examples.
#' @param A.ubs upper-bounds matrix on variables, see examples.
#' @param b.eq  equality constraint vector.
#' @param b.ge  inequality constraint vector.
#' @param b.lbs low-bounds on variables, see details.
#' @param b.ubs upper-bounds on variables, see details.
#' @param penalty The penalty to be applied to the model. Either "lasso" (the default), "SCAD",
#' or "MCP".
#' @param admmScale1  first ADMM scale parameter, 1/nrow(X) is default.
#' @param admmScale2  second ADMM scale parameter, 1 is default.
#' @param admmAbsTol  absolute tolerance for ADMM, 1e-04 is default.
#' @param admmRelTol  relative tolerance for ADMM, 1e-04 is default.
#' @param nADMM  maximum number of iterations for ADMM, 2000 is default.
#' @param admmVaryScale  dynamically chance the ADMM scale parameter, FALSE is default
#'
#' @return \item{betahat}{solution vector.}
#' @return \item{stats.ADMM_inters}{number of iterations.}
#'
#' @import ncvreg glmnet quadprog Matrix
#' @export HSDiC_ADMM
#'
#' @seealso \code{\link{solve.QP}}
#'
#' @references {'Pairwise Fusion Approach Incorporating Prior Constraint Information' by Yaguang Li}
#'



##
HSDiC_ADMM <- function(X, Y, A.eq, A.ge, A.lbs, A.ubs, b.eq, b.ge, b.lbs, b.ubs,
                       penalty = c("MCP", "SCAD", "adlasso", "lasso"), lambda2,
                       admmScale1= 1/nrow(X), admmScale2 = 1, admmAbsTol = 1e-04,
                       admmRelTol = 1e-04, nADMM = 2000, admmVaryScale = FALSE) {

  # require(ncvreg)
  # require(glmnet)
  # require(quadprog)
  # require(Matrix)
  penalty <- match.arg(penalty)

  n <- nrow(X)
  p <- ncol(X)


  # # ADMM loop
  # admmScale1 <- 1/n  # 'admmScale' - ADMM scale parameter, 1/n is default
  # nADMM <- 2000  # maximum number of iterations for ADMM
  # admmAbsTol <- 1e-04  # 'admmAbsTol' - absolute tolerance for ADMM
  # admmRelTol <- 1e-04  # 'admmRelTol' - relative tolerance for ADMM
  iter.zero <- 1e-04


  # initialize
  Q0 <- lm(Y ~ X - 1)
  b0 <- Q0$coef
  betahat <- b0[1:ncol(X)]

  ##
  L <- NULL
  for (j in 1:(p - 1)) {
    L[[j]] <- matrix(0, nrow = p - j, ncol = p)
    L[[j]][, j] <- 1
    for (k in (j + 1):p) {
      L[[j]][k - j, k] <- -1
    }
  }
  L <- do.call(rbind, L)

  z1 <- betahat
  z2 <- L %*% betahat
  u1 <- z1 - betahat
  u2 <- z2 - L %*% betahat


  # ADMM loop
  XX <- rbind(X, diag(p), L)

  for (iADMM in 1:nADMM) {

    # update beta - L1/ SCAD/ MCP
    YY <- rbind(Y, as.matrix(z1 - u1), z2 - u2)

    if (penalty == "adlasso") {

      reg.fit <- glmnet(x = XX, y = drop(YY), family = "gaussian",
                        alpha = 1, penalty.factor = 1/abs(b0), intercept = FALSE)
      coef.beta <- reg.fit$beta
      dev <- deviance(reg.fit)
      reg.df <- reg.fit$df
      obj <- dev + log(nrow(YY)) * reg.df
      lambda.ind <- which.min(obj)
      lambda <- reg.fit$lambda[lambda.ind]
      betahat <- coef.beta[, lambda.ind]

    } else {


      reg.fit <- ncvreg(X = XX, y = drop(YY), family = "gaussian",
                        penalty = penalty)
      coef.beta <- reg.fit$beta  # extract coefficients at all values of lambda, including the intercept
      reg.df <- apply(abs(coef.beta[-1, , drop = FALSE]) > 1e-10,
                      2, sum)
      BIC <- reg.fit$loss + log(nrow(YY)) * reg.df
      lambda.ind <- which.min(BIC)
      betahat <- coef.beta[-1, lambda.ind]

    }



    # update z - projection to constraint set
    v1 <- betahat + u1
    v2 <- L %*% betahat + u2
    zOld1 <- z1
    zOld2 <- z2
    # project to polyhedron using quadratic programming
    z1 <- solve.QP(Dmat = diag(admmScale1, p), dvec = admmScale1 *
                     v1, Amat = t(rbind(A.eq, A.ge, A.lbs, A.ubs)),
                   bvec = c(b.eq, b.ge, b.lbs, b.ubs), meq = 1)$solution
    z1[abs(z1) < iter.zero] <- 0


    if (penalty == "adlasso") {

      w <- outer(b0, b0, "-")
      w <- abs(w[lower.tri(w)])^2
      z2 <- thresh_est(z = v2, lambda = lambda2/w, tau = admmScale2,
                       penalty = "lasso")

    } else {

      z2 <- thresh_est(z = v2, lambda = lambda2, a = 3, tau = admmScale2,
                       penalty = penalty)

    }


    # update scaled dual variables u
    dualResNorm <- norm((z1 - zOld1) * admmScale1 + t(L) %*% (z2 -
                                                                zOld2) * admmScale2, type = "2")

    primalRes1 <- betahat - z1
    primalResNorm1 <- norm(primalRes1, type = "2")
    u1 <- u1 + primalRes1

    primalRes2 <- L %*% betahat - z2
    primalResNorm2 <- norm(primalRes2, type = "2")
    u2 <- u2 + primalRes2


    # stopping criterion
    if (max(primalResNorm1, primalResNorm2) <= sqrt(p) * admmAbsTol +
        admmRelTol * max(norm(betahat, "2"), norm(L %*% betahat, "2"),
                         norm(z1, "2"), norm(z2, "2")) && (dualResNorm <= sqrt(n) *
                                                           admmAbsTol + admmRelTol * (norm(u1, "2") + norm(t(L) %*% u2,
                                                                                                           "2")))) {

      cat("Convergence !", "\n")
      break
    }

    # beta = betahat; update ADMM scale parameter if requested
    if (admmVaryScale) {
      if (max(primalResNorm1, primalResNorm2)/dualResNorm > 10) {
        admmScale1 <- admmScale1/2
        admmScale2 <- admmScale2/2
        u1 <- u1/2
        u2 <- u2/2
      } else if (max(primalResNorm1, primalResNorm2)/dualResNorm <
                 0.1) {
        admmScale1 <- admmScale1 * 2
        admmScale2 <- admmScale2 * 2
        u1 <- 2 * u1
        u2 <- 2 * u2
      }
    }

  }

  return(list(beta = betahat, stats.ADMM_inters = iADMM))
}

