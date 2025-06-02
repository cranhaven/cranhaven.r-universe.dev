#Example 1: Poisson Regression Modeling

########################################################################
#Maximum Likelihood Estimation
library(MASS)
library(quadprog)
library(mvtnorm)

tmp_ormle <-
  function(est,covmtrx,constr,rhs,nec){
    K=length(est)
    covmtrx = as.matrix(covmtrx)
    Dmat = 2*ginv(covmtrx)
    dvec = 2*(est%*% ginv(covmtrx))
    solveQP = solve.QP(Dmat, dvec = dvec, t(constr), rhs, meq = nec, factorized = FALSE)
    tildeQ = solveQP$solution
    restrictedest=solveQP$solution
    names(restrictedest)=names(est)
    loglik =as.numeric( ( -K/2*log(2*pi) )-( 0.5*log(det(covmtrx) ) )-( 0.5* t(est- tildeQ)%*%ginv(covmtrx)%*% (est-tildeQ)) )

    out <- list(est=est, covmtrx=covmtrx, constr=constr, rhs=rhs, nec=nec, logLik=loglik,restrictedest=restrictedest)
    class(out) <- "ormle"
    return(out)

  }

tmp_gorica_penalty <-
  function(object, iterations=1000, mc.cores=1){

    if (!(inherits(object, "ormle") )) stop("object needs to be of class ormle")
    if (all(object$constr == 0) & object$nec == 0){
      penalty <- length(object$est)
    } else {
      if (iterations < 1) stop("No of iterations < 1")

      est<-object$est
      K<-length(est)
      covmtrx <- object$covmtrx
      constr<-object$constr
      rhs=object$rhs
      nec=object$nec

      Z <- rmvnorm(n=iterations, mean=rep(0, K), sigma=covmtrx)
      Dmat2=2*ginv(covmtrx)

      nact <- apply(Z, 1, function(z){

        dvec2=2*(z%*%ginv(covmtrx))
        solveQP2= solve.QP(Dmat2,dvec2,t(constr),rhs,meq =nec,factorized = FALSE)
        if (solveQP2$iact[1] == 0) return(0) else return(length(solveQP2$iact))
      })

      dimsol <- K - nact
      LP <- sapply(1:K, function(x) sum(x == (dimsol)))/iterations
      penalty <- sum((1:K)*LP[])

    }

    return(penalty)

  }



tmp_gorica <-
  function(object, ..., iterations=1000){
    if (!inherits(object, "ormle") & !inherits(object, "list")) stop("object needs to be of class ormle or a list of ormle objects")
    if (iterations < 1) stop("No of iterations < 1")
    if (inherits(object, "ormle")) objlist <- list(object, ...) else objlist <- object
    isorlm <- sapply(objlist, function(x) inherits(x, "ormle"))
    orlmlist <- objlist[isorlm]
    Call <- match.call()
    Call$iterations <- NULL
    if (inherits(object, "ormle")) names(orlmlist) <- as.character(Call[-1L])[isorlm]
    loglik <- -2*sapply(orlmlist, function(x) x$logLik)
    penalty <- 2*sapply(orlmlist, function(x) tmp_gorica_penalty(x, iterations=iterations))
    gorica <- loglik + penalty
    delta <- gorica - min(gorica)
    gorica_weights <- exp(-delta/2) / sum(exp(-delta/2))
    data.frame(misfit=loglik,complexity=penalty,gorica=gorica,gorica_weights=round(gorica_weights,4))
  }



#Specification of the data set
data("academic_awards")
academic_awards$zmath <- scale(academic_awards$math)
#Model fitting
model <- glm(num_awards ~ prog + zmath + prog * zmath, family = "poisson",
             data = academic_awards)


strest <- model$coefficients[c(4,5,6)]
strcovmtrx <- vcov(model)[c(4,5,6), c(4,5,6)]

#Obtaining order-restricted estimates using ormle

#Specification of the hypotheses under evaluation

# Hypothesis 1
constr <- matrix(c(1, 0, 0,
                   1, 1, 0,
                   1, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
rhs <- rep(0, 3)
nec <- 3
H1 <- gorica:::ormle(est = strest, covmtrx = strcovmtrx, constr = constr, nec = nec, rhs = rhs)

H1_char <- "zmath = 0 & zmath + progGeneral:zmath = 0 & zmath + progVocational:zmath =0"

# Hypothesis 2
constr <- matrix(c(0, 1, -1,
                   0, 0, 1), nrow = 2, ncol = 3, byrow = TRUE)
rhs <- rep(0, 2)
nec <- 0
H2 <- gorica:::ormle(est = strest, covmtrx = strcovmtrx, constr = constr, nec = nec, rhs = rhs)
H2_char <- "progGeneral:zmath > progVocational:zmath & progVocational:zmath > 0"


# Hypothesis 3
constr <- matrix(c(0, 0, -1), nrow = 1, ncol = 3, byrow = TRUE)
rhs <- rep(0, 1)
nec <- 0
H3 <- gorica:::ormle(est = strest, covmtrx = strcovmtrx, constr = constr, nec = nec, rhs = rhs)
H3_char <- "progVocational:zmath < 0"

# The unconstrained hypothesis
constr <- matrix(c(rep(0, 3)), nrow = 1, ncol = 3, byrow = TRUE)
rhs <- rep(0, 1)
nec <- 0
Hu <- gorica:::ormle(est = strest, covmtrx = strcovmtrx, constr = constr, nec = nec, rhs = rhs)

set.seed(111)
# Source code in gorica is reached, which is saved in the file "Gorica.txt"


#Performing gorica to obtain the values of misfit, complexity, GORICA, and GORICA weights
man_gorica <- gorica:::compare_hypotheses.ormle(H1, H2, H3, Hu, iterations = 1000)

original_gorica <- tmp_gorica(H1, H2, H3, Hu, iterations = 1000)
pasted_hyp <- paste(H1_char, H2_char, H3_char, sep = ";")
res_gorica <- gorica(model, "zmath = 0 & zmath + progGeneral:zmath = 0 & zmath + progVocational:zmath =0;progGeneral:zmath > progVocational:zmath & progVocational:zmath > 0;progVocational:zmath < 0", iterations = 1000)

test_that("Original and manual gorica same", {
  expect_equivalent(original_gorica$gorica, man_gorica$comparisons$gorica, tolerance = .13)
})

test_that("Original and package gorica same", {
  expect_equivalent(original_gorica$gorica, res_gorica$fit$gorica, tolerance = .04)
})

test_that("manual and package gorica same", {
  expect_equivalent(man_gorica$comparisons$gorica, res_gorica$fit$gorica, tolerance = .091)
})



# CHeck penalty function --------------------------------------------------

penalty_internal <- sapply(list(H1, H2, H3, Hu), function(x){gorica:::gorica_penalty(x, iterations = 1000)$penalty})

penalty_original <- sapply(list(H1, H2, H3, Hu), function(x){tmp_gorica_penalty(x, iterations = 1000)})

penalty_package <- res_gorica$fit$penalty

test_that("Original and internal penalty same", {
  expect_equivalent(penalty_original, penalty_internal, tolerance = .05)
})

test_that("Original and package penalty same", {
  expect_equivalent(penalty_original, penalty_package, tolerance = .05)
})

test_that("Internal and package penalty same", {
  expect_equivalent(penalty_internal, penalty_package, tolerance = .01)
})
