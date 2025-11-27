library(testthat)

test_that('Optimization for the deepest point', {

  skip('slow')
  library(tidyverse)

  # This example is generated from test_MHD.R with n=50, p=4, mfdName='AffInv', seed=1, tol=1e-14
  load(system.file('testdata', 'SPD4.RData', package='MHD'))
  attach(testDat)
  basis <- manifold::basisTan(mfd, theta0)
  Dist <- function(x, Y) {
    manifold::distance(mfd, x, t(Y))
  }

  F <- function(v0) {
    v <- basis %*% as.matrix(v0)
    expv <- t(rieExp(mfd, theta0, v))
    D <- PairwiseDistance2(anchors, expv, Dist)
    ind <- outer(D, D, '-') <= 0 + tol
    vec <- prop[ind]
    min(vec)
  }

  p <- sqrt(length(theta0))
  origPar <- crossprod(basis, rieLog(mfd, theta0, c(diag(nrow=p))))
  -F(origPar)

  x0 <- rep(0, ncol(basis))
  lb <- rep(-1, length(x0))
  ub <- rep(1, length(x0))
  # optRes <- optim(origPar, F, method='Nelder-Mead', control=list(fnscale=-1))

  globalAlgoList <- c('NLOPT_GN_DIRECT', 
                      'NLOPT_GN_CRS2_LM', 
                      'NLOPT_GN_MLSL_LDS', 
                      'NLOPT_GN_ISRES', 
                      'NLOPT_GN_ESCH')
  local_opts <- list(algorithm='NLOPT_LN_SBPLX', xtol_rel=1e-7)
  otherOpt <- list(maxeval=1000, 
                   print_level=0)
  gloOpt <- map(globalAlgoList, function(algo) {
                     opt <- c(list(algorithm=algo), otherOpt)
                     if (algo %in% c('NLOPT_GN_MLSL_LDS')) {
                       opt <- c(opt, list(local_opts=local_opts))
                     }
                     opt
                   })

  localAlgoList <- c('NLOPT_LN_COBYLA',
                     'NLOPT_LN_PRAXIS',
                     'NLOPT_LN_NELDERMEAD',
                     'NLOPT_LN_SBPLX')

  stopCrit <- c(xtol_abs=1e-4)
  otherOptLoc <- list(maxeval=100, 
                      print_level=0)
  locOpt <- map(localAlgoList, function(algo) {
                     opt <- c(list(algorithm=algo), otherOptLoc)
                     opt
                   })


  resGlo <- map(gloOpt, function(opt) {
                     nloptr::nloptr(x0, function(x) -F(x), lb=lb, ub=ub, opts=opt)
                   })

  a <- tibble(algoGlo=globalAlgoList, resGlo=resGlo) %>% 
    mutate(resLoc = map(resGlo, function(res) {
      # Take each global result, and further polish it with each of the local methods
      
      resLoc <- map(locOpt, function(loc) {
        nloptr::nloptr(res$solution, function(x) -F(x), lb=lb, ub=ub, opts=loc)
      })
      tibble(algoLoc=localAlgoList, resLoc=resLoc)
    })) 
  resAll <- a %>% 
    unnest(resLoc) %>% 
    mutate(objective = map_dbl(resLoc, `[[`, 'objective'),
           solution = map(resLoc, `[[`, 'solution'))

})
