#Example 1: Poisson Regression Modeling

########################################################################
#Maximum Likelihood Estimation
library(MASS)
library(quadprog)
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
H1_original_ormle <- tmp_ormle(est = strest, covmtrx = strcovmtrx, constr = constr, nec = nec, rhs = rhs)

test_that("ormle est same", {
  expect_equivalent(H1_original_ormle$est, H1$est)
})

test_that("ormle covmtrx same", {
  expect_equivalent(H1_original_ormle$covmtrx, H1$covmtrx)
})

test_that("ormle constr same", {
  expect_equivalent(H1_original_ormle$constr, H1$constr)
})

test_that("ormle rhs same", {
  expect_equivalent(H1_original_ormle$rhs, H1$rhs)
})

test_that("ormle nec same", {
  expect_equivalent(H1_original_ormle$nec, H1$nec)
})

test_that("ormle loglik same", {
  expect_equivalent(H1_original_ormle$loglik, H1$loglik)
})

test_that("ormle restrictedest same", {
  expect_equivalent(H1_original_ormle$restrictedest, H1$restrictedest)
})
