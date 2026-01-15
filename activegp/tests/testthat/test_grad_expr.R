library(activegp)
library(hetGP)
context("grad_expr")

covtypes <- c("Gaussian", "Matern3_2", "Matern5_2")

### There are effectively three cases for Wij calculation:
### computing i == j element, derivative wrt i
### computing i == j element, derivative wrt k != i
### computing i != j element, derivative wrt i or j
### computing i != j element, derivative wrt k != i or j
### These cases need to be verified for differentiation vs args 1 and 2.

for (ct in 1:length(covtypes)) {
  test_that(paste(covtypes[ct], "Kernel Univariate Derivatives Work wrt first arg"), {
    
    a <- 0.7# First point
    b <- 0.3# Second point
    l <- 1# Lengthscale
    h <- 1e-8# Step size for finite difference approx
    thresh <- 1e-3#Allowable gap between finite differencing and analytic soltn.
    
    ##### w_ii
    expect_true(abs((activegp:::w_ii_lebesgue(a+h,b,l,ct) - activegp:::w_ii_lebesgue(a,b,l,ct)) / h - 
                      activegp:::grad_w_ii_lebesguea(a,b,l,ct)) < thresh)
    
    ##### w_ij
    # For a > b
    expect_true(abs((activegp:::w_ij_lebesgue(a+h,b,l,ct) - activegp:::w_ij_lebesgue(a,b,l,ct)) / h - 
                      activegp:::grad_w_ij_lebesguea(a,b,l,ct)) < thresh)
    # For a <= b
    temp <- a
    a <- b
    b <- temp
    expect_true(abs((activegp:::w_ij_lebesgue(a+h,b,l,ct) - activegp:::w_ij_lebesgue(a,b,l,ct)) / h - 
                      activegp:::grad_w_ij_lebesguea(a,b,l,ct)) < thresh)
    temp <- a
    a <- b
    b <- temp
    
    ##### Ikk
    # For a > b
    expect_true(abs((activegp:::Ikk_lebesgue(a+h,b,l,ct) - activegp:::Ikk_lebesgue(a,b,l,ct)) / h - 
                      activegp:::grad_Ikk_lebesguea(a,b,l,ct)) < thresh)
    # For a <= b
    temp <- a
    a <- b
    b <- temp
    expect_true(abs((activegp:::Ikk_lebesgue(a+h,b,l,ct) - activegp:::Ikk_lebesgue(a,b,l,ct)) / h - 
                      activegp:::grad_Ikk_lebesguea(a,b,l,ct)) < thresh)
    temp <- a
    a <- b
    b <- temp
  })
  test_that(paste(covtypes[ct], "Kernel Univariate Derivatives Work wrt second arg"), {
    
    a <- 0.7# First point
    b <- 0.3# Second point
    l <- 1# Lengthscale
    h <- 1e-5# Step size for finite difference approx
    thresh <- 1e-3#Allowable gap between finite differencing and analytic soltn.
    
    ##### w_ii
    expect_true(abs((activegp:::w_ii_lebesgue(a,b+h,l,ct) - activegp:::w_ii_lebesgue(a,b,l,ct)) / h - 
                      activegp:::grad_w_ii_lebesgueb(a,b,l,ct)) < thresh)
    
    ##### w_ij
    # For a > b
    expect_true(abs((activegp:::w_ij_lebesgue(a,b+h,l,ct) - activegp:::w_ij_lebesgue(a,b,l,ct)) / h - 
                      activegp:::grad_w_ij_lebesgueb(a,b,l,ct)) < thresh)
    # For a <= b
    temp <- a
    a <- b
    b <- temp
    expect_true(abs((activegp:::w_ij_lebesgue(a,b+h,l,ct) - activegp:::w_ij_lebesgue(a,b,l,ct)) / h - 
                      activegp:::grad_w_ij_lebesgueb(a,b,l,ct)) < thresh)
    temp <- a
    a <- b
    b <- temp
    
    ##### Ikk
    # For a > b
    expect_true(abs((activegp:::Ikk_lebesgue(a,b+h,l,ct) - activegp:::Ikk_lebesgue(a,b,l,ct)) / h - 
                      activegp:::grad_Ikk_lebesgueb(a,b,l,ct)) < thresh)
    # For a <= b
    temp <- a
    a <- b
    b <- temp
    expect_true(abs((activegp:::Ikk_lebesgue(a,b+h,l,ct) - activegp:::Ikk_lebesgue(a,b,l,ct)) / h - 
                      activegp:::grad_Ikk_lebesgueb(a,b,l,ct)) < thresh)
    temp <- a
    a <- b
    b <- temp
  })
  
  test_that(paste(covtypes[ct], " Wij Gradients work"), {
    set.seed(123)
    nvar <- 2
    n <- 10
    thresh <- 1e-3#Allowable gap between finite differencing and analytic soltn.
    
    design <- matrix(runif(n*nvar, 0, 1), nrow = n)
    response <- rnorm(n)
    
    model <- mleHomGP(design, response, lower = rep(1e-4, nvar), upper = rep(1,nvar),
                      noiseControl = list(g_bounds = c(1e-6, 1e-6)), covtype = covtypes[ct]) 
    C <- C_GP(model)
    
    xnew <- runif(nvar,0,1)
    xnew <- matrix(xnew, nrow = 1)
    
    ## Gradient wrt first argument, i == j
    i <- 1
    j <- 1
    theta <- rep(1, nvar)#Lengthscale vector.
    num <- c()
    for (k in 1:nvar) {
      h <- 1e-6#Finite difference step size
      
      xh <- rep(0,nvar)
      xh[k] <- h
      
      num <- rbind(num, ((drop(activegp:::W_kappa_ij2(xnew + xh, C$model$X0, theta = theta, i - 1, j - 1, ct = C$ct)) - 
                            drop(activegp:::W_kappa_ij2(xnew, C$model$X0, theta = theta, i - 1, j - 1, ct = C$ct))) / h))
    } 
    
    expect_true(norm(num - activegp:::grad_W_kappa_ij2(xnew, C$model$X0, theta = theta, i - 1, j - 1, ct = C$ct)) < thresh)
    
    ## Gradient wrt first argument, i != j
    i <- 1
    j <- 2
    theta <- rep(1, nvar)#Lengthscale vector.
    num <- c()
    for (k in 1:nvar) {
      h <- 1e-6#Finite difference step size
      
      xh <- rep(0,nvar)
      xh[k] <- h
      
      num <- rbind(num, ((drop(activegp:::W_kappa_ij2(xnew + xh, C$model$X0, theta = theta, i - 1, j - 1, ct = C$ct)) - 
                            drop(activegp:::W_kappa_ij2(xnew, C$model$X0, theta = theta, i - 1, j - 1, ct = C$ct))) / h))
    } 
    
    expect_true(norm(num - activegp:::grad_W_kappa_ij2(xnew, C$model$X0, theta = theta, i - 1, j - 1, ct = C$ct)) < thresh)
    
    ## Gradient wrt second argument, i==j
    i <- 1
    j <- 1
    theta <- rep(1, nvar)#Lengthscale vector.
    num <- c()
    for (k in 1:nvar) {
      h <- 1e-6#Finite difference step size
      
      xh <- rep(0,nvar)
      xh[k] <- h
      
      num <- rbind(num, ((drop(activegp:::W_kappa_ij2(C$model$X0, xnew + xh, theta = theta, i - 1, j - 1, ct = C$ct)) - 
                            drop(activegp:::W_kappa_ij2(C$model$X0, xnew, theta = theta, i - 1, j - 1, ct = C$ct))) / h))
    } 
    
    expect_true(norm(num - activegp:::grad_W_kappa_ij2(xnew, C$model$X0, theta = theta, i - 1, j - 1, ct = C$ct)) < thresh)
    
    ## Gradient wrt second argument, i != j
    i <- 1
    j <- 2
    theta <- rep(1, nvar)#Lengthscale vector.
    num <- c()
    for (k in 1:nvar) {
      h <- 1e-6#Finite difference step size
      
      xh <- rep(0,nvar)
      xh[k] <- h
      
      num <- rbind(num, ((drop(activegp:::W_kappa_ij2(C$model$X0, xnew + xh, theta = theta, i - 1, j - 1, ct = C$ct)) - 
                            drop(activegp:::W_kappa_ij2(C$model$X0, xnew, theta = theta, i - 1, j - 1, ct = C$ct))) / h))
    } 
    
    expect_true(norm(num - activegp:::grad_W_kappa_ij2_w2(xnew, C$model$X0, theta = theta, i - 1, j - 1, ct = C$ct)) < thresh)
  })
  
  #test_that(paste(covtypes[ct], " beta/gamma gradients work "), {
  #          set.seed(123)
  #          thresh <- 1e-3#Allowable gap between finite differencing and analytic soltn.
  
  #          # Set up problem
  #          nvar <- 2
  #          n <- 10
  #          design <- matrix(runif(n*nvar, 0, 1), nrow = n)
  #          response <- apply(design, 1, sum)
  #          model <- mleHomGP(design, response, lower = rep(1e-4, nvar), upper = rep(1,nvar),
  #                            noiseControl = list(g_bounds = c(1e-6, 1e-6)), covtype = covtypes[ct]) 
  #          C <- C_GP(model)
  #          xnew <- runif(nvar,0,1)
  #          xnew <- matrix(xnew, nrow = 1)
  
  #          # Common Precomputations
  #          if(is.null(nrow(xnew))) xnew <- matrix(xnew, nrow = 1)
  #          nvar <- ncol(xnew)
  #          kn1 <- cov_gen(xnew, C$model$X0, theta = C$model$theta, type = C$model$covtype)
  #          theta <- sqrt(C$model$theta/2)
  #          new_lambda <- predict(object = C$model, x = xnew, nugs.only = TRUE)$nugs/C$model$nu2_hat
  #          vn <- drop(1 - kn1 %*% tcrossprod(C$model$Ki, kn1)) + new_lambda + C$model$eps
  #          Kikn <- tcrossprod(C$model$Ki, kn1)
  #          Kiyn <- C$model$Ki %*% C$model$Z0 # Ki yn
  
  #          ## Check beta gradient
  #          # i == j
  #          i <- 1
  #          j <- 1
  #          num <- rep(NA, nvar)
  #          for (k in 1:nvar) {
  #            h <- 1e-5
  #            xh <- rep(0, nvar)
  #            xh[k] <- h
  #            num[k] <- (activegp:::get_betagamma(C, xnew + xh, i, j, kn1, theta, Kikn, Kiyn, vn)$beta - 
  #                       activegp:::get_betagamma(C, xnew, i, j, kn1, theta, Kikn, Kiyn, vn)$beta) / h
  #          }
  
  #          expect_true(norm(num - activegp:::get_betagamma(C, xnew, i, j, kn1, theta, Kikn, Kiyn, vn, grad = TRUE)$dbeta) < thresh)
  
  #          # i != j
  #          i <- 1
  #          j <- 2
  #          num <- rep(NA, nvar)
  #          for (k in 1:nvar) {
  #            h <- 1e-5
  #            xh <- rep(0, nvar)
  #            xh[k] <- h
  #            a <- (activegp:::get_betagamma(C, xnew + xh, i, j, kn1, theta, Kikn, Kiyn, vn) - activegp:::get_betagamma(C, xnew, i, j, kn1, theta, Kikn, Kiyn, vn)) / h
  #            num[k] <- a[1]
  #          }
  
  #          expect_true(norm(num - activegp:::get_betagamma(C, xnew, i, j, kn1, theta, Kikn, Kiyn, vn, grad = TRUE)[[1]]) < thresh)
  
  #          ## Check gamma gradient
  #          thresh <- 1e-2
  #          # i == j
  #          i <- 1
  #          j <- 1
  #          num <- rep(NA, nvar)
  #          for (k in 1:nvar) {
  #            h <- 1e-5
  #            xh <- rep(0, nvar)
  #            xh[k] <- h
  #            a <- (activegp:::get_betagamma(C, xnew + xh, i, j, kn1, theta, Kikn, Kiyn, vn) - activegp:::get_betagamma(C, xnew, i, j, kn1, theta, Kikn, Kiyn, vn)) / h
  #            num[k] <- a[2]
  #          }
  
  #          expect_true(norm(num - activegp:::get_betagamma(C, xnew, i, j, kn1, theta, Kikn, Kiyn, vn, grad = TRUE)[[2]]) < thresh)
  
  #          # i != j
  #          i <- 1
  #          j <- 2
  #          num <- rep(NA, nvar)
  #          for (k in 1:nvar) {
  #            h <- 1e-5
  #            xh <- rep(0, nvar)
  #            xh[k] <- h
  #            a <- (activegp:::get_betagamma(C, xnew + xh, i, j, kn1, theta, Kikn, Kiyn, vn) - activegp:::get_betagamma(C, xnew, i, j, kn1, theta, Kikn, Kiyn, vn)) / h
  #            num[k] <- a[2]
  #          }
  
  #          expect_true(norm(num - activegp:::get_betagamma(C, xnew, i, j, kn1, theta, Kikn, Kiyn, vn, grad = TRUE)[[2]]) < thresh)
  #})
  
  test_that(paste(covtypes[ct], "C var gradients work"), {
    set.seed(123)
    nvar <- 3
    n <- 10
    thresh <- 1e-3#Allowable gap between finite differencing and analytic soltn.
    
    design <- matrix(runif(n*nvar, 0, 1), nrow = n)
    response <- apply(design, 1, sum)
    
    model <- mleHomGP(design, response, lower = rep(1e-4, nvar), upper = rep(1,nvar),
                      noiseControl = list(g_bounds = c(1e-6, 1e-6)), covtype = covtypes[ct]) 
    C <- C_GP(model)
    
    xnew <- runif(nvar,0,1)
    xnew <- matrix(xnew, nrow = 1)
    
    ## Check gradient
    num <- rep(NA, nvar)
    for (k in 1:nvar) {
      h <- 1e-5
      xh <- rep(0, nvar)
      xh[k] <- h
      num[k] <- (C_var(C, xnew + xh) - C_var(C, xnew)) / h
    }
    expect_true(sum((num - C_var(C, xnew, grad = TRUE))^2) < thresh)
  })
  
  test_that(paste(covtypes[ct], "C var 2 gradients work"), {
    set.seed(123)
    nvar <- 3
    n <- 10
    thresh <- 1e-3#Allowable gap between finite differencing and analytic soltn.
    
    design <- matrix(runif(n*nvar, 0, 1), nrow = n)
    response <- apply(design, 1, sum)
    
    model <- mleHomGP(design, response, lower = rep(1e-4, nvar), upper = rep(1,nvar),
                      noiseControl = list(g_bounds = c(1e-6, 1e-6)), covtype = covtypes[ct]) 
    C <- C_GP(model)
    
    xnew <- runif(nvar,0,1)
    xnew <- matrix(xnew, nrow = 1)
    
    ## Check gradient
    num <- rep(NA, nvar)
    for (k in 1:nvar) {
      h <- 1e-5
      xh <- rep(0, nvar)
      xh[k] <- h
      num[k] <- (C_var2(C, xnew + xh) - C_var2(C, xnew)) / h
    }
    expect_true(sum((num - C_var2(C, xnew, grad = TRUE))^2) < thresh)
  })
  
  test_that(paste(covtypes[ct], "C Tr gradients work"), {
    set.seed(123)
    nvar <- 3
    n <- 10
    thresh <- 1e-3#Allowable gap between finite differencing and analytic soltn.
    
    design <- matrix(runif(n*nvar, 0, 1), nrow = n)
    response <- apply(design, 1, sum)
    
    model <- mleHomGP(design, response, lower = rep(1e-4, nvar), upper = rep(1,nvar),
                      noiseControl = list(g_bounds = c(1e-6, 1e-6)), covtype = covtypes[ct]) 
    C <- C_GP(model)
    
    xnew <- runif(nvar,0,1)
    xnew <- matrix(xnew, nrow = 1)
    
    ## Check gradient
    num <- rep(NA, nvar)
    for (k in 1:nvar) {
      h <- 1e-5
      xh <- rep(0, nvar)
      xh[k] <- h
      num[k] <- (C_tr(C, xnew + xh) - C_tr(C, xnew)) / h
    }
    expect_true(sum((num - C_tr(C, xnew, grad = TRUE))^2) < thresh)
  })
}

test_that("kernel_expr_stability", {
  # small t + same point
  expect_true(!any(is.nan(activegp:::grad_W_kappa_ij2(matrix(c(0.5,0.76), 1), matrix(c(0.5,0.76), 1), theta = c(0.00001, 0.001), i1 = 0, i2 = 0, ct = 1))))
  expect_true(!any(is.nan(activegp:::grad_W_kappa_ij2(matrix(c(0.5,0.76), 1), matrix(c(0.5,0.76), 1), theta = c(0.00001, 0.001), i1 = 0, i2 = 0, ct = 2))))
  expect_true(!any(is.nan(activegp:::grad_W_kappa_ij2(matrix(c(0.5,0.76), 1), matrix(c(0.5,0.76), 1), theta = c(0.00001, 0.001), i1 = 0, i2 = 0, ct = 3))))
  expect_true(!any(is.nan(activegp:::grad_W_kappa_ij2(matrix(c(0.5,0.76), 1), matrix(c(0.5,0.76), 1), theta = c(0.00001, 0.001), i1 = 0, i2 = 1, ct = 1))))
  expect_true(!any(is.nan(activegp:::grad_W_kappa_ij2(matrix(c(0.5,0.76), 1), matrix(c(0.5,0.76), 1), theta = c(0.00001, 0.001), i1 = 0, i2 = 1, ct = 2))))
  expect_true(!any(is.nan(activegp:::grad_W_kappa_ij2(matrix(c(0.5,0.76), 1), matrix(c(0.5,0.76), 1), theta = c(0.00001, 0.001), i1 = 0, i2 = 1, ct = 3))))
  
})

test_that("kernel derivatives wrt t", {
  library(numDeriv)
  for(ct in c(1)){ # cases 2 and 3 missing 
    a <- sqrt(2); b <- sqrt(3); t <- sqrt(5)
    
    expect_equal(grad(activegp:::w_ii_lebesgue, t, a = a, b = b, ct = ct), activegp:::grad_w_ii_dt_cpp(a, b, t, ct = ct))
    expect_equal(grad(activegp:::w_ij_lebesgue, t, a = a, b = b, ct = ct), activegp:::grad_w_ij_dt_cpp(a, b, t, ct = ct))
    expect_equal(grad(activegp:::Ikk_lebesgue, t, a = a, b = b, ct = ct), activegp:::grad_Ikk_dt_cpp(a, b, t, ct = ct))
  }
})

test_that("W/C/A matrix derivatives wrt t", {
  library(numDeriv)
  for(ct in c(1)){ # cases 2 and 3 missing 
    
    x1 <- matrix(c(0.5, sqrt(2), 0.17), 1)
    x2 <- matrix(c(sqrt(3), 1/3, 0.25), 1)
    thetas <- 1.5* c(0.25, 0.37, 0.75)
    
    design <- rbind(x1, x2)
    W_wrap <- function(design, theta, i1, i2, j1, j2, ct){
      return(W_kappa_ij(design, theta, i1 - 1, i2 - 1, ct)[j1, j2])
    }
    
    for(i1 in 1:3){
      for(i2 in 1:3){
        g_num <- rbind(grad(W_wrap, thetas, design = design, i1 = i1, i2 = i2, j1 = 1, j2 = 1, ct = ct),
                       grad(W_wrap, thetas, design = design, i1 = i1, i2 = i2, j1 = 2, j2 = 1, ct = ct),
                       grad(W_wrap, thetas, design = design, i1 = i1, i2 = i2, j1 = 1, j2 = 2, ct = ct),
                       grad(W_wrap, thetas, design = design, i1 = i1, i2 = i2, j1 = 2, j2 = 2, ct = ct))
        
        Wref <- W_wrap(design = design, theta = thetas, i1 = i1, i2 = i2, ct = ct)
        g_for <- cbind(as.vector(activegp:::grad_W_t(design = design, theta = thetas, i1 = i1-1, i2 = i2-1, W = Wref, it = 1-1, ct = ct)),
                       as.vector(activegp:::grad_W_t(design = design, theta = thetas, i1 = i1-1, i2 = i2-1, W = Wref, it = 2-1, ct = ct)),
                       as.vector(activegp:::grad_W_t(design = design, theta = thetas, i1 = i1-1, i2 = i2-1, W = Wref, it = 3-1, ct = ct)))
        expect_equal(g_num, g_for)
        
      }
    }
    
    ## Now for C
    
    set.seed(123)
    nvar <- 3
    n <- 50
    r <- 1
    f <- function(x) sin(sum(x))
    true_sub <- rep(1, nvar) / sqrt(nvar)
    
    # Initial design
    design <- matrix(runif(nvar*n), ncol = nvar)
    response <- apply(design, 1, f)
    
    thetas <- c(0.5, 0.6, 0.75)
    g <- sqrt(.Machine$double.eps)
    
    model <- mleHomGP(X = design, Z = response, known = list(theta = 2*thetas^2, g = g, beta0 = 0))
    C_ref <- C_GP(model)
    
    # return only one element of C
    C_wrap <- function(design, response, theta, i, j){
      modeltmp <- mleHomGP(X = design, Z = response, known = list(theta = 2*theta^2, g = g, beta0 = 0))
      C_GP(modeltmp)$mat[i,j]
    }
    
    dC <- matrix(unlist(activegp:::dC_dt(design = design, response = response, theta = thetas, C = C_ref, ct = 1, eps = g)),9)
    
    dC_num <- rbind(grad(C_wrap, thetas, design = design, response = response, i = 1, j = 1),
                    grad(C_wrap, thetas, design = design, response = response, i = 2, j = 1),
                    grad(C_wrap, thetas, design = design, response = response, i = 3, j = 1),
                    grad(C_wrap, thetas, design = design, response = response, i = 1, j = 2),
                    grad(C_wrap, thetas, design = design, response = response, i = 2, j = 2),
                    grad(C_wrap, thetas, design = design, response = response, i = 3, j = 2),
                    grad(C_wrap, thetas, design = design, response = response, i = 1, j = 3),
                    grad(C_wrap, thetas, design = design, response = response, i = 2, j = 3),
                    grad(C_wrap, thetas, design = design, response = response, i = 3, j = 3))
    
    expect_equal(dC, dC_num, tol = 2e-4)
    
    ## And for A
    A_fun <- function(design, response, theta, i, j){
      modeltmp <- mleHomGP(X = design, Z = response, known = list(theta = 2*theta^2, g = g, beta0 = 0))
      Cmat <- C_GP(modeltmp)
      decomp <- eigen(Cmat$mat, symmetric = TRUE)$vectors
      if(decomp[1,j] < 0) decomp <- -decomp
      return(decomp[i,j])
    }
    
    # A_fun(design = design, response = response, theta = thetas, i = 1, j = 1)
    dA_num <- rbind(grad(A_fun, thetas, design = design, response = response, i = 1, j = 1),
                    grad(A_fun, thetas, design = design, response = response, i = 2, j = 1),
                    grad(A_fun, thetas, design = design, response = response, i = 3, j = 1),
                    grad(A_fun, thetas, design = design, response = response, i = 1, j = 2),
                    grad(A_fun, thetas, design = design, response = response, i = 2, j = 2),
                    grad(A_fun, thetas, design = design, response = response, i = 3, j = 2),
                    grad(A_fun, thetas, design = design, response = response, i = 1, j = 3),
                    grad(A_fun, thetas, design = design, response = response, i = 2, j = 3),
                    grad(A_fun, thetas, design = design, response = response, i = 3, j = 3))
    
    dA_ex <- matrix(c(dA(design = design, response = response, theta = thetas, C = C_ref, iv = 1, ct = 1),
                      dA(design = design, response = response, theta = thetas, C = C_ref, iv = 2, ct = 1),
                      dA(design = design, response = response, theta = thetas, C = C_ref, iv = 3, ct = 1)),9, byrow = T)
    
    expect_equal(abs(dA_num), abs(dA_ex), tol = 3e-3)
    
  }
})





