
library(testthat)
library(ordPens)


context("check ordinal Smoothing/Selection/Fusion functions")
 

## checks on warning and error messages
test_that("ordSmooth, ordSelect, ordFusion work properly",
          {
            
            data(ICFCoreSetCWP) 
            x <- ICFCoreSetCWP[, 1:67] + matrix(c(rep(1, 50), rep(5, 16), 1),
                                                nrow(ICFCoreSetCWP), 67,
                                                byrow = TRUE)
            y <- ICFCoreSetCWP$phcs
            lambda <- c(1000, 500, 200, 100, 70, 50, 30, 20, 10, 1) 
            
            x <- rbind(x, rep(1,67))
            x <- rbind(x, c(rep(5, 50), rep(9,16), 5))
            y <- c(y, NA, NA)
            
            # checks on x 
            expect_error(  
              ordSmooth(x = list(x), y = y, lambda = lambda),
              "x has to be a matrix, numeric vector or data.frame" 
            ) 
            expect_error(  
              ordSelect(x = list(x), y = y, lambda = lambda),
              "x has to be a matrix, numeric vector or data.frame" 
            ) 
            expect_error(  
              ordFusion(x = list(x), y = y, lambda = lambda),
              "x has to be a matrix, numeric vector or data.frame" 
            )
            
            x.na <- x
            x.na[1] <- NA
            
            expect_error(   
              ordSmooth(x = x.na, y = y, lambda = lambda),
              "Missing values in x are not allowed" 
            ) 
            expect_error(   
              ordSelect(x = x.na, y = y, lambda = lambda),
              "Missing values in x are not allowed"
            ) 
            expect_error(   
              ordFusion(x = x.na, y = y, lambda = lambda),
              "Missing values in x are not allowed"
            ) 
            
            # checks on response y
            expect_error(  
              ordSmooth(x = x, y = list(y), lambda = lambda),
              "y has to be numeric"
            ) 
            expect_error(  
              ordSelect(x = x, y = list(y), lambda = lambda),
              "y has to be numeric"
            ) 
            expect_error(  
              ordFusion(x = x, y = list(y), lambda = lambda),
              "y has to be numeric"
            )
            
            expect_error(  
              ordSmooth(x = x, y = y, lambda = lambda, model = "logit"),
              "y has to be 0/1 coded"
            ) 
            expect_error(  
              ordSelect(x = x, y = y, lambda = lambda, model = "logit"),
              "y has to be 0/1 coded"
            ) 
            expect_error(  
              ordFusion(x = x, y = y, lambda = lambda, model = "logit"),
              "y has to be 0/1 coded"
            )
  
            expect_error(  
              ordSmooth(x = x, y = y, lambda = lambda, model = "poisson"),
              "y has to contain nonnegative integers"
            ) 
            expect_error(  
              ordSelect(x = x, y = y, lambda = lambda, model = "poisson"),
              "y has to contain nonnegative integers"
            ) 
            expect_error(  
              ordFusion(x = x, y = y, lambda = lambda, model = "poisson"),
              "y has to contain nonnegative integers"
            )
            
            y.neg <- round(y) 
            y.neg[1] <- -1
            
            expect_error(  
              ordSmooth(x = x, y = y.neg, lambda = lambda, model = "poisson"),
              "y has to contain nonnegative integers"
            ) 
            expect_error(  
              ordSelect(x = x, y = y.neg, lambda = lambda, model = "poisson"),
              "y has to contain nonnegative integers"
            ) 
            expect_error(  
              ordFusion(x = x, y = y.neg, lambda = lambda, model = "poisson"),
              "y has to contain nonnegative integers"
            )
            
            # check on other arguments
            expect_error(   
              ordSmooth(x = x, y = y, lambda = lambda, offset = 1) 
            ) 
            expect_error(   
              ordSelect(x = x, y = y, lambda = lambda, offset = 1)
            ) 
            expect_error(   
              ordFusion(x = x, y = y, lambda = lambda, offset = 1) 
            ) 
             
            expect_error(   
              ordSmooth(x = x, y = y, lambda = lambda, nonpenx = (ncol(x)+1))
            ) 
            expect_error(   
              ordSelect(x = x, y = y, lambda = lambda, nonpenx = (ncol(x)+1)) 
            ) 
            expect_error(   
              ordFusion(x = x, y = y, lambda = lambda, nonpenx = (ncol(x)+1)) 
            ) 
            
            expect_warning(
              ordSmooth(x = x, y = y, lambda = sort(lambda)),
              "lambda values should be sorted in decreasing order" 
            )
            expect_warning(
              ordSelect(x = x, y = y, lambda = sort(lambda)),
              "lambda values should be sorted in decreasing order" 
            )
            
            # checks on ordinal predictors
            expect_error(
              ordSmooth(x = x[-1,], y = y, lambda = lambda),
              "x and y do not have correct dimensions"
            )
            expect_error(
              ordSelect(x = x[-1,], y = y, lambda = lambda),
              "x and y do not have correct dimensions"
            ) 
            
            x.log <- sample(c(TRUE,FALSE), length(y), replace = T)
            
            expect_error(
              ordSmooth(x = as.matrix(x.log), y = y, lambda = lambda),
              "Entries of x have to be of type 'numeric'"
            )
            expect_error(
              ordSelect(x = as.matrix(x.log), y = y, lambda = lambda),
              "Entries of x have to be of type 'numeric'"
            ) 
            
            x.neg <- x.float <- x
            x.neg[1] <- -1  
            x.float[1] <- 1.5
            
            expect_error(
              ordSmooth(x = x.neg, y = y, lambda = lambda),
              "x has to contain positive integers"
            )
            expect_error(
              ordSelect(x = x.neg, y = y, lambda = lambda),
              "x has to contain positive integers"
            ) 
            
            expect_error(
              ordSmooth(x = x.float, y = y, lambda = lambda),
              "x has to contain positive integers"
            )
            expect_error(
              ordSelect(x = x.float, y = y, lambda = lambda),
              "x has to contain positive integers"
            ) 
            
            # checks on nominal predictors
            u <- rbinom(n=length(y), size = 5, prob = 0.5)
            
            expect_error(
              ordSmooth(x = x, y = y, lambda = lambda, u = list(u)),
              "u has to be a matrix, numeric vector or data.frame"
            )
            expect_error(
              ordSelect(x = x, y = y, lambda = lambda, u = list(u)),
              "u has to be a matrix, numeric vector or data.frame"
            )
            expect_error(
              ordFusion(x = x, y = y, lambda = lambda, u = list(u)),
              "u has to be a matrix, numeric vector or data.frame"
            )
            
            u.na <- u
            u.na[1] <- NA
            
            expect_error(
              ordSmooth(x = x, y = y, lambda = lambda, u = u.na),
              "Missing values in u are not allowed"
            )
            expect_error(
              ordSelect(x = x, y = y, lambda = lambda, u = u.na),
              "Missing values in u are not allowed"
            )
            expect_error(
              ordFusion(x = x, y = y, lambda = lambda, u = u.na),
              "Missing values in u are not allowed"
            )
            
            expect_error(
              ordSmooth(x = x, y = y, lambda = lambda, u = u[-1]),
              "u and y do not have correct dimensions"
            )
            expect_error(
              ordSelect(x = x, y = y, lambda = lambda, u = u[-1]),
              "u and y do not have correct dimensions"
            )
            expect_error(
              ordFusion(x = x, y = y, lambda = lambda, u = u[-1]),
              "u and y do not have correct dimensions"
            )
            
            u.log <- sample(c(TRUE,FALSE), length(y), replace = T)
            
            expect_error(
              ordSmooth(x = x, y = y, lambda = lambda, u = as.matrix(u.log)),
              "Entries of u have to be of type 'numeric'"
            )
            expect_error(
              ordSelect(x = x, y = y, lambda = lambda, u = as.matrix(u.log)),
              "Entries of u have to be of type 'numeric'"
            )
            expect_error(
              ordFusion(x = x, y = y[!is.na(y)], lambda = lambda, u = as.matrix(u.log[!is.na(y)])),
              "Entries of u have to be of type 'numeric'"
            )
            
            u.neg <- u.float <- u
            u.neg[1] <- -1 
            u.float[1] <- 1.5
            
            expect_error(
              ordSmooth(x = x, y = y, lambda = lambda, u = as.matrix(u.neg)),
              "u has to contain positive integers"
            )
            expect_error(
              ordSelect(x = x, y = y, lambda = lambda, u = as.matrix(u.neg)),
              "u has to contain positive integers"
            )
            expect_error(
              ordFusion(x = x, y = y[!is.na(y)], lambda = lambda, u = as.matrix(u.neg[!is.na(y)])),
              "u has to contain positive integers"
            )
            
            expect_error(
              ordSmooth(x = x, y = y, lambda = lambda, u = u.float),
              "u has to contain positive integers"
            )
            expect_error(
              ordSelect(x = x, y = y, lambda = lambda, u = u.float),
              "u has to contain positive integers"
            )
            expect_error(
              ordFusion(x = x, y = y[!is.na(y)], lambda = lambda, u = as.matrix(u.float[!is.na(y)])),
              "u has to contain positive integers"
            )
            
            # check on metric predictors 
            z <- runif(length(y)) 
            
            expect_error(
              ordSmooth(x = x, y = y, lambda = lambda, z = list(z)),
              "z has to be a matrix, numeric vector or data.frame"
            )
            expect_error(
              ordSelect(x = x, y = y, lambda = lambda, z = list(z)),
              "z has to be a matrix, numeric vector or data.frame"
            )
            expect_error(
              ordFusion(x = x, y = y, lambda = lambda, z = list(z)),
              "z has to be a matrix, numeric vector or data.frame"
            )
            
            z.na <- z
            z.na[1] <- NA
            
            expect_error(
              ordSmooth(x = x, y = y, lambda = lambda, z = z.na),
              "Missing values in z are not allowed"
            )
            expect_error(
              ordSelect(x = x, y = y, lambda = lambda, z = z.na),
              "Missing values in z are not allowed"
            )
            expect_error(
              ordFusion(x = x, y = y, lambda = lambda, z = z.na),
              "Missing values in z are not allowed"
            )
            
            expect_error(
              ordSmooth(x = x, y = y, lambda = lambda, z = z[-1]),
              "z and y do not have correct dimensions"
            )
            expect_error(
              ordSelect(x = x, y = y, lambda = lambda, z = z[-1]),
              "z and y do not have correct dimensions"
            )
            expect_error(
              ordFusion(x = x, y = y, lambda = lambda, z = z[-1]),
              "z and y do not have correct dimensions"
            )
            
            z.log <- sample(c(TRUE,FALSE), length(y), replace = T)
            
            expect_error(
              ordSmooth(x = x, y = y, lambda = lambda, z = as.matrix(z.log)),
              "Entries of z have to be of type 'numeric'"
            )
            expect_error(
              ordSelect(x = x, y = y, lambda = lambda, z = as.matrix(z.log)),
              "Entries of z have to be of type 'numeric'"
            )
            expect_error(
              ordFusion(x = x, y = y[!is.na(y)], lambda = lambda, z = as.matrix(z.log[!is.na(y)])),
              "Entries of z have to be of type 'numeric'"
            ) 
            
            rm(list=ls())
 })





