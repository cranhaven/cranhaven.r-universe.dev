set.seed(12345)

test_that("0.95",{
   J <- matrix(nrow=2,c(0,0.945, 0.94,0.96, 0.955,1))
   colnames(J) <- c("low","ok","high")
   gen <- function() as.numeric(runif(1)<0.95)
   expect_bernoulli(gen,J=J,ok="ok")
   skip_on_cran()
   expect_error(expect_bernoulli(function() runif(1)<0.9,J=J,ok="ok"))
   
})


test_that("0.05",{
   skip_on_cran()
   J <- matrix(nrow=2,c(0,0.045, 0.04,0.06, 0.055,1))
   colnames(J) <- c("low","ok","high")
   gen <- function() as.numeric(runif(1)<0.05)
   expect_bernoulli(gen,J=J,ok="ok")

   expect_error(expect_bernoulli(function() runif(1)<0.07,J=J,ok="ok"))
   
})



test_that("0.05 0.95",{
   skip_on_cran()
   J <- matrix(nrow=2,c(0,0.045, 0.04,0.06, 0.055,0.945, 0.94,0.96,0.955,1))
   colnames(J) <- c("low","ok","middle", "ok", "high")
   gen <- function() as.numeric(runif(1)<0.05)
   expect_bernoulli(gen,J=J,ok="ok")
   gen <- function() as.numeric(runif(1)<0.951)
   expect_bernoulli(gen,J=J,ok="ok")

   expect_error(expect_bernoulli(function() runif(1)<0.07,J=J,ok="ok"))
   expect_error(expect_bernoulli(function() runif(1)<0.99,J=J,ok="ok"))
   
})



test_that("0",{
   skip_on_cran()
   J <- matrix(nrow=2,c(0,0.045, 0.04,1))
   colnames(J) <- c("ok", "high")
   gen <- function() as.numeric(runif(1)<0.01)
   expect_bernoulli(gen,J=J,ok="ok")

   expect_error(expect_bernoulli(function() runif(1)<0.1,J=J,ok="ok"))
})


test_that("1",{
   skip_on_cran()
   J <- matrix(nrow=2,c(0,0.9, 0.89,1))
   colnames(J) <- c("low", "ab")
   gen <- function() as.numeric(runif(1)<0.99)
   expect_bernoulli(gen,J=J,ok="ab")

   expect_error(expect_bernoulli(function() runif(1)<0.2,J=J,ok="ab"))
})

test_that("Traps",{
   gen <- function() as.numeric(runif(1)<0.02)

   ##missing column names
   J <- matrix(nrow=2,c(0,0.045, 0.04,1))
   expect_error(expect_bernoulli(gen, J=J,ok="ok"),"column names",class = "expectation_failure")

   ## not matching string in ok
   colnames(J) <- c("ok", "high")
   expect_error(expect_bernoulli(gen, J=J,ok="def"),"match",class = "expectation_failure")

   ## not overlapping  
   J <- matrix(nrow=2,c(0,0.045, 0.4,1))
   colnames(J) <- c("ok", "high")
   expect_error(expect_bernoulli(gen, J=J,ok="ok"),"overlapping",class = "expectation_failure")

   ## cover [0,1]
   J <- matrix(nrow=2,c(0.1,0.045, 0.04,1))
   colnames(J) <- c("ok", "high")
   expect_error(expect_bernoulli(gen, J=J,ok="ok"),"cover",class = "expectation_failure")
   J <- matrix(nrow=2,c(0,0.045, 0.04,.9))
   colnames(J) <- c("ok", "high")
   expect_error(expect_bernoulli(gen, J=J,ok="ok"),"cover",class = "expectation_failure")
   J <- matrix(nrow=2,c(0.04,.9, 0,0.045)) 
   colnames(J) <- c("ok", "high")
   expect_error(expect_bernoulli(gen, J=J,ok="ok"),"cover",class = "expectation_failure")


   #reverse ordering
   J <- matrix(nrow=2,c(.04,1,0,0.045))
   colnames(J) <- c("high", "ok")
   expect_bernoulli(gen, J=J,ok="ok")
   
})
