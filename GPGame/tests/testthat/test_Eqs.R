context('Equilibrium computation')
library(GPareto)
library(copula)
library(DiceDesign)

test_that("KS equilibrium computation is correct", {
  
  ## 2D
  xgrid <- seq(0,1,length.out = 101)
  Xgrid <- as.matrix(expand.grid(xgrid, xgrid))
  Z <- P2(Xgrid)
  
  PF <- nonDom(Z)
  Shadow <- apply(PF, 2, min)
  Nadir <- apply(PF, 2, max)
  KSeq <- GPGame:::getKS(Z = Z, Nadir = Nadir, Shadow = Shadow)
  KSeq2 <- GPGame:::getKS_cpp(Z, Nadir, Shadow)
  expect_equal(KSeq$id, KSeq2)
  
  ratios <- (matrix(Nadir, nrow = nrow(PF), ncol = ncol(PF), byrow = TRUE) - PF) %*% diag(1/(Nadir - Shadow))
  KS_ref <- PF[which.max(apply(ratios, 1, min)),, drop = FALSE]
  
  expect_equal(KSeq$KS, KS_ref)
  
  # plot(Z)
  # plotParetoEmp(PF, col ='red')
  # points(KSeq$KS, col = "green", pch = 20)
  
  ## 5D (Pareto front of a ball)
  set.seed(1)
  nvar <- 5
  n <- 2e6
  Z <- matrix(rnorm(n * nvar), n)
  Z <- t(apply(Z, 1, function(x) return(-abs(x)/sqrt(sum(x^2)))))
  Shadow <- rep(-1, nvar)
  Nadir <- rep(0, nvar)
  KS_ref <- matrix(rep(- 1/sqrt(nvar), nvar), nrow = 1)
  KSeq <- GPGame:::getKS(Z = Z, Nadir = Nadir, Shadow = Shadow)
  KSeq2 <- GPGame:::getKS_cpp(Z = Z, Nadir = Nadir, Shadow = Shadow)
  expect_equal(KSeq$id, KSeq2)
  
  expect_equal(KSeq$KS, KS_ref, tol = 1e-2)
})

test_that("CKS equilibrium computation is correct", {
  
  ## 2D
  nvar <- 2
  n <- 1e4
  Xgrid <- matrix(runif(n * nvar), n)
  Z <- P2(Xgrid)
  
  Shadow <- rep(0, nvar)
  Nadir <- rep(1, nvar)
  
  library(copula)
  U <- pobs(Z)
  KS_U <- GPGame:::getKS(U, Nadir = Nadir, Shadow = Shadow)
  PF <- nonDom(U)
  
  CKSeq <- GPGame:::getCKS(Z = Z, Nadir = Nadir, Shadow = Shadow)
  
  expect_equal(Z[KS_U$id,,drop = FALSE], CKSeq$CKS)
  
  # plot(U)
  # plotParetoEmp(PF, col ='blue')
  # points(U[CKSeq$id,,drop = F], col = "green", pch = 20)
  # points(KS_U$KS, col = "red", pch = 1)
  
  ## 3D (Pareto front generated from a sphere)
  set.seed(3)
  nvar <- 3
  n <- 1e3
  U <- matrix(rnorm(n * nvar), n)
  U <- t(apply(U, 1, function(x) return(if(sum(x^2) < 1) return(-abs(x)) else return(-abs(x)/sqrt(sum(x^2)))))) + 1
  PF_u <- nonDom(U)
  
  
  Shadow <- rep(0, nvar)
  Nadir <- rep(1, nvar)
  
  Z <- U
  Z[,1] <- qbeta(U[,1], 2.3 , 0.5)
  Z[,2] <- qbeta(U[,2], 2   , 1.5)
  Z[,3] <- qbeta(U[,3], 0.75, 1.2)
  
  PF <- nonDom(Z)
  
  CKS_ref <- GPGame:::getKS(Z = U, Nadir = Nadir, Shadow = Shadow)
  CKSeq <- GPGame:::getCKS(Z = Z, Nadir = Nadir, Shadow = Shadow)
  
  expect_equal(CKSeq$id, CKS_ref$id)
  
  # plotParetoEmp(PF_u)
  # points3d(U)
  # points3d(U[CKSeq$id,, drop = FALSE])
  # 
  # plot3d(Z)
  # plotParetoEmp(PF)
  
  # With Zred
  U2 <- matrix(rnorm(n * nvar), n)
  U2 <- t(apply(U2, 1, function(x) return(if(sum(x^2) < 1) return(-abs(x)) else return(-abs(x)/sqrt(sum(x^2)))))) + 1
  Z2 <- U2
  Z2[,1] <- qbeta(U2[,1], 2.3 , 0.5)
  Z2[,2] <- qbeta(U2[,2], 2   , 1.5)
  Z2[,3] <- qbeta(U2[,3], 0.75, 1.2)
  
  CKS_U2 <- GPGame:::getCKS(Z2, Nadir = Nadir, Shadow = Shadow)
  CKS_U2_kweights <- GPGame:::getCKS(Z, Nadir = Nadir, Shadow = Shadow, Zred = Z2)
  expect_equal(CKS_U2$CKS, CKS_U2_kweights$CKS)
  
})

test_that("getEquilibrium works as intended", {
  set.seed(1)
  nvar <- 3
  n <- 60
  
  design <- lhsDesign(n = n, dimension = nvar, seed = 42)$design
  response <- DTLZ2(design)
  
  # nref <- 1e5
  # Xref <- matrix(runif(nref * nvar), nref)
  # Yref <- DTLZ2(Xref)
  # PFref <- nonDom(Yref, return.idx = TRUE)
  
  KS_ref <- matrix(rep(1/sqrt(nvar), nvar), nrow = 1)
  CKS_ref_cop <- matrix(c(0.533, 0.533, 0.533), nrow = 1) #approx KS in copula space
  CKS_ref <- matrix(c(0.423, 0.42, 0.80), nrow = 1) #approx
  
  # plotParetoEmp(response)
  # points3d(response)
  
  models <- list(km(~1, design = design, response = response[,1]),
                 km(~1, design = design, response = response[,2]),
                 km(~1, design = design, response = response[,3]))
  nsamp <- 1e4
  Xsamp <- matrix(runif(nsamp * nvar), nsamp)
  
  # first on the mean predictions
  preds <- t(predict_kms(models, newdata = Xsamp, type = "UK")$mean)
  
  KSp <- getEquilibrium(Z = preds, nobj = 3, equilibrium = "KSE", return.design = TRUE)
  CKSp <- getEquilibrium(Z = preds, nobj = 3, equilibrium = "CKSE", return.design = TRUE)
  
  expect_equal(KSp$NEPoff, KS_ref, tol = 5e-2)
  expect_equal(CKSp$NEPoff, CKS_ref, tol = 1e-2)
  
  # with kweights
  kweights <- list()
  
  for(u in 1:3){
    kn <- covMat1Mat2(models[[u]]@covariance, Xsamp, design)
    Knn <- try(chol2inv(chol(covMat1Mat2(models[[u]]@covariance, design, design) + diag(1e-6, nrow(design)))))
    kweights <- c(kweights, list(kn %*% Knn))
  }
  
  CKSp2 <- getEquilibrium(Z = response, nobj = 3, equilibrium = "CKSE", return.design = TRUE, kweights = kweights)
  CKS_emp_ref <- response[which.min(rowSums(sweep(response, 2, CKS_ref, "-")^2)),, drop = FALSE]
  expect_equal(CKSp2$NEPoff, CKS_emp_ref)
  
  # points3d(KSp$NEPoff, col = "red", size = 5)
  # points3d(CKSp$NEPoff, col = "green", size = 5)
  # points3d(CKSp2$NEPoff, col = "yellow", size = 5)
  # points3d(CKS_emp_ref, col = "pink", size = 5)
  # points3d(CKS_ref, col = "orange", size = 5)
  
  # then for simulated equilibria
  nsim <- 7
  Simus <- NULL
  
  for(u in 1:3){
    Simus <- rbind(Simus, simulate(models[[u]], newdata = Xsamp[1:1000,], nsim = nsim, cond = TRUE, checkNames = FALSE))
  }
  Simus <- t(Simus)
  
  KS_sim <- getEquilibrium(Simus, nobj = 3, equilibrium = "KSE", return.design = TRUE)
  CKS_sim <- getEquilibrium(Simus, nobj = 3, equilibrium = "CKSE", return.design = TRUE)
  
  # Check KS and CKS one set of simulations at a time
  for(u in 1:nsim){
    expect_equal(KS_sim$NEPoff[u,, drop = FALSE], getEquilibrium(Simus[,seq(u, nsim*3, by = nsim)], nobj = 3, equilibrium = "KSE", return.design = FALSE))
    expect_equal(CKS_sim$NEPoff[u,, drop = FALSE], getEquilibrium(Simus[,seq(u, nsim*3, by = nsim)], nobj = 3, equilibrium = "CKSE", return.design = FALSE))
  }
  
  
  # with kweights
  kweights2 <- list()
  
  for(u in 1:3){
    kn <- covMat1Mat2(models[[u]]@covariance, Xsamp, Xsamp[1:1000,])
    Knn <- try(chol2inv(chol(covMat1Mat2(models[[u]]@covariance, Xsamp[1:1000,], Xsamp[1:1000,]) + diag(1e-6, 1000))))
    kweights2 <- c(kweights2, list(kn %*% Knn))
  }
  CKS_sim2 <- getEquilibrium(Simus, nobj = 3, equilibrium = "CKSE", return.design = TRUE, kweights = kweights2)
  
  expect_equal(KS_sim$NEPoff, matrix(KS_ref, nrow = nsim, ncol = nvar, byrow = TRUE), tol = 5e-2)
  expect_equal(CKS_sim$NEPoff, matrix(CKS_ref, nrow = nsim, ncol = nvar, byrow = TRUE), tol = 5e-2)
  expect_equal(CKS_sim2$NEPoff, matrix(CKS_ref, nrow = nsim, ncol = nvar, byrow = TRUE), tol = 5e-2)
  
})

# test_that("Test cpp routines", {
#   nobj <- 9
#   np1 <- 2000
#   np2 <- 200
#   Zrand <- matrix(rnorm(nobj*np1), np1)
#   Zred <- matrix(rnorm(nobj*np2), np2)
#   
#   ranks_ref <- GPGame:::rel_ranks(Zrand = Zrand, Zred = Zred)
#   ranks_cpp <- GPGame:::rel_ranks_cpp(Zrand = Zrand, Zred = Zred)
#   
#   expect_equal(ranks_ref, ranks_cpp)
#   # microbenchmark(GPGame:::rel_ranks(Zrand = Zrand, Zred = Zred),
#   #                GPGame:::rel_ranks_cpp(Zrand = Zrand, Zred = Zred))
# })


