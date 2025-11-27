

library(testthat)
library(ordPens)

context("check ordinal penalized PCA (ordPCA) function")
 

## checks on warning and error messages
test_that("lambda in ordPCA gets sorted properly",
          {
            
            data(ICFCoreSetCWP) 
            H <- ICFCoreSetCWP[, 1:67] + matrix(c(rep(1, 50), rep(5, 16), 1),
                                                nrow(ICFCoreSetCWP), 67,
                                                byrow = TRUE)
            y <- ICFCoreSetCWP$phcs 
            
            expect_warning( 
              ordPCA(H, p = 2, lambda = sort(c(5, 0.5, 0.001)), 
                     Ks = c(rep(5,50), rep(9,16), 5), 
                     constr = c(rep(TRUE,50), rep(FALSE,16), TRUE) 
                      ),  
              "lambda values should be sorted in decreasing order" 
            )    
            
            rm(list=ls())
                      
})


