##!/usr/bin/Rscript
##  /home/nate/drmc/activegp/R/fun_test.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 01.11.2021
#
#withr::with_libpaths('~/.R/staging/', devtools::install())
#withr::with_libpaths('~/.R/staging/', install.packages('hetGP', repos = "https://cloud.r-project.org"))
#
#library(activegp, lib.loc = '~/.R/staging/')
#library(hetGP, lib.loc = '~/.R/staging/')
#
#N <- 100
#P <- 3
#
#X <- matrix(runif(N*P), nrow = N)
#y <- X[,2]
#
#fit <- mleHomGP(X, y, covtype = 'Matern5_2')
#tt <- Sys.time()
#Ch <- C_GP(X,y, measure = 'lebesgue')
#print(Sys.time() - tt)
#
#Ch <- C_GP(fit, measure = 'lebesgue')
#
#Lt <- Lt_GP(X, y)
#Lt_GP(C=Ch)
#ed <- eigen(Ch)
#ed$vectors %*% diag(sqrt(ed$values))
#
#Ch <- C_GP(X,y, measure = 'sample')
#
#fit <- mleHomGP(X, y, covtype = 'Matern5_2')
#measure_ind <- 0
#model
##C$mat <- quick_C(measure_ind, fit$X, fit$Ki, fit$Ki %*% y, sqrt(model$theta/2), xm = xm, xv = xv, ct = 1)
#
