library(episcan)

context("test epiHSIC")

test_that('epiHSIC',{
  a <- matrix(1:4, nrow = 2)
  b <- matrix(6:1, nrow = 2)
  p <- c(1, 2)
  hsic <- function(a,b,p){
    h <- matrix(nrow = ncol(a), ncol = ncol(b))
    for (i in 1:ncol(a)) {
      for (j in 1:ncol(b)) {
        x1 <- a[,i]
        x2 <- b[,j]
        h[i, j] <-  sum(x1 * x2 * p)
      }
    }
    return(h / sqrt(length(p)))
  }
  
  expect_equal(epiHSIC(a, b, p),
               hsic(a,b,p))
  
})