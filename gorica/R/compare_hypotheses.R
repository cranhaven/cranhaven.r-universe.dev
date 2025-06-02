compare_hypotheses <-
  function(object, ..., iterations = 100000){
    UseMethod("compare_hypotheses")
  }

compare_hypotheses.ormle <-
  function(object, ..., iterations = 100000){
    if (iterations < 1) stop("No of iterations < 1")
    if (inherits(object, "ormle")) objlist <- list(object, ...) else objlist <- object
    isorlm <- sapply(objlist, function(x) inherits(x, "ormle"))
    orlmlist <- objlist[isorlm]
    Call <- match.call()
    Call$iterations <- NULL
    if (inherits(object, "ormle")) names(orlmlist) <- as.character(Call[-1L])[isorlm]
    gorica_penalties <- lapply(orlmlist, function(x) gorica_penalty(x, iterations = iterations))
    loglik <- sapply(orlmlist, function(x) x$logLik)
    penalty <- sapply(gorica_penalties, `[[`, "penalty")
    gor <- -2*loglik + 2*penalty
    list(comparisons = data.frame(loglik = loglik,
               penalty = penalty,
               gorica = gor),
         gorica_penalties = gorica_penalties
    )
  }

compare_hypotheses.list <- function(object, ..., iterations = 100000){
  if (all(sapply(object, class) == "ormle")) out <- compare_hypotheses.ormle(object, iterations = iterations)
  return(out)
}

#' @importFrom MASS ginv
#' @importFrom mvtnorm rmvnorm
#' @importFrom quadprog solve.QP
gorica_penalty <-
  function(object, iterations = 100000){
    K <- length(object$est)
    if(any(object$constr != 0) | object$nec != 0){

      Z <- rmvnorm(n = iterations, mean = rep(0, K), sigma = object$covmtrx)
      ginvcovm <- ginv(object$covmtrx)
      Dmat2 = 2*ginvcovm

      dvec2 <- 2*(Z %*% t(ginvcovm))
      t_const <- t(object$constr)

      nact <- apply(dvec2, 1, function(z){
        solveQP2 = solve.QP(Dmat = Dmat2,
                            dvec = z,
                            Amat = t_const,
                            bvec = object$rhs - object$rhs,
                            meq = object$nec,
                            factorized = FALSE)
        if (solveQP2$iact[1] == 0){
          0
        } else {
          length(solveQP2$iact)
        }
      })
      wt_bar <- sapply(1:K, function(x) sum(x == (K - nact)))/iterations
      list(penalty = sum(1:K*wt_bar), wt_bar = wt_bar)
    } else {
      list(penalty = K, wt_bar = c(rep(0, K-1), 1))
    }

  }

compute_weights <- function(gorica){
  if(length(gorica) < 2) return(1)
  delta <- gorica - min(gorica)
  exp(-delta/2) / sum(exp(-delta/2))
}
