
library(testthat)
library(ordPens)


context("check ordinal ANOVA (ordAOV) and ordGene function")
 

## checks on warning and error messages
test_that("ord ANOVA and ordGene work properly",
          {  
            
            # load data for ordAOV
            data(ICFCoreSetCWP) 
            x <- ICFCoreSetCWP[,1]
            x <- as.integer(x - min(x) + 1) 
            y <- ICFCoreSetCWP$phcs
            
            # generate data for ordGene
            set.seed(321) 
            ni <- 5
            n <- sum(5*ni)
            xpr <- matrix(NA, ncol = n, nrow = 100)
            mu_lin <- 3:7  
            mu_sq2 <- (-2:2)^2 * 0.5 + 3   
            a <- seq(0.75, 1.25, length.out = 10) 
            for(i in 1:10){ 
              xpr[i,] <- a[i] * rep(mu_lin, each = ni) + rnorm(n)
              xpr[i+10,] <- a[i] * rep(mu_sq2, each = ni) + rnorm(n) 
            } 
            for(i in 21:100) xpr[i,] <- 3 + rnorm(n)
            dose <- rep(c(0, 0.01, 0.05, 0.2, 1.5), each = ni)
            
             
            ### ordAOV ###
            # checks on x & y
            expect_error(  
              ordAOV(list(x), y, type = "RLRT", nsim=1000000),
              "x has to be a matrix or numeric vector" 
            )  
            
            x.neg <- x.float <- x 
            x.neg[1] <- -1
            x.float[1] <- 1.5
            
            expect_error(  
              ordAOV(x.neg, y, type = "RLRT", nsim=1000000),
              "x has to contain positive integers"
            )   
            expect_error(  
              ordAOV(x.float, y, type = "RLRT", nsim=1000000),
              "x has to contain positive integers"
            )  
            
            expect_error(  
              ordAOV(x, cbind(y, y), type = "RLRT", nsim=1000000),
              "y has to be a numeric vector"
            )   
            expect_error(  
              ordAOV(x, list(y), type = "RLRT", nsim=1000000),
              "y has to be a numeric vector"
            )  
            
            x.na <- x
            x.na[1] <- NA
            
            y.na <- y
            y.na[1] <- NA
            
            expect_error(  
              ordAOV(x.na, y, type = "RLRT", nsim=1000000)
            )  
            expect_error(  
              ordAOV(x, y.na, type = "RLRT", nsim=1000000) 
            )  
            
            ### ordGene ###
            # check xpr & lvs
            expect_error(  
              ordGene(xpr = list(xpr), lvs = dose, nsim = 1e6),
              "xpr has to be a matrix or data frame"
            )  
            expect_error(  
              ordGene(xpr = as.numeric(xpr[,1]), lvs = dose, nsim = 1e6),
              "xpr has to be a matrix or data frame"
            )   
            
            expect_error(  
              ordGene(xpr = xpr, lvs = cbind(dose, dose), nsim = 1e6),
              "lvs has to be a numeric vector"
            )   
            expect_error(  
              ordGene(xpr = xpr, lvs = as.factor(dose), nsim = 1e6),
              "lvs has to be a numeric vector"
            )   
            
            xpr.na <- xpr
            xpr.na[1] <- NA
            
            dose.na <- dose
            dose.na[1] <- NA
            
            expect_error(  
              ordGene(xpr = xpr.na, lvs = dose, nsim = 1e6),
              "Missing values in xpr are not allowed"
            )  
            
            expect_error(  
              ordGene(xpr = xpr, lvs = dose.na, nsim = 1e6),
              "Missing values in lvs are not allowed"
            )   
             
            rm(list=ls())
            
})

