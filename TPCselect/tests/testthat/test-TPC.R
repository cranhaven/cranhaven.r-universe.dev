test_that("test the main functions (TPC)", {
  library(TPCselect)
  # parameter settings
  #generate sample data
  p = 200
  n = 200
  truebeta <- c(c(3,1.5,0,0,2),rep(0,p-5))
  rho = 0.3
  sigma =  matrix(0,p+1,p+1)
  for(i in 1:(p+1)){
    for(j in 1:(p+1)){
      sigma[i,j] = rho^(abs(i-j))
    }
  }
  x_error = 0.9*MASS::mvrnorm(n,rep(0,p+1),sigma) + 0.1*MASS::mvrnorm(n,rep(0,p+1),9*sigma)
  x = x_error[,1:p]
  error = x_error[,p+1]
  #u = runif(n)
  y = x%*%truebeta + error

  #perform variable selection via partial correlation
  TPC.fit = TPC(y,x,0.05,1,method="threshold")


  expect_equal(as.vector(TPC.fit$beta), truebeta, tolerance = 0.1)
})

test_that("test the main functions (TPC) with simple algorithm", {
  library(TPCselect)
  # parameter settings
  #generate sample data
  p = 200
  n = 200
  truebeta <- c(c(3,1.5,0,0,2),rep(0,p-5))
  rho = 0.3
  sigma =  matrix(0,p+1,p+1)
  for(i in 1:(p+1)){
    for(j in 1:(p+1)){
      sigma[i,j] = rho^(abs(i-j))
    }
  }
  x_error = 0.9*MASS::mvrnorm(n,rep(0,p+1),sigma) + 0.1*MASS::mvrnorm(n,rep(0,p+1),9*sigma)
  x = x_error[,1:p]
  error = x_error[,p+1]
  y = x%*%truebeta + error

  #perform variable selection via partial correlation
  TPC.fit = TPC(y,x,0.05,1,method="simple")


  expect_equal(as.vector(TPC.fit$beta), truebeta, tolerance = 0.1)
})

test_that("test the main functions (TPC) if other algorithm", {
  library(TPCselect)
  # parameter settings
  #generate sample data
  p = 200
  n = 200
  truebeta <- c(c(3,1.5,0,0,2),rep(0,p-5))
  rho = 0.3
  sigma =  matrix(0,p+1,p+1)
  for(i in 1:(p+1)){
    for(j in 1:(p+1)){
      sigma[i,j] = rho^(abs(i-j))
    }
  }
  x_error = 0.9*MASS::mvrnorm(n,rep(0,p+1),sigma) + 0.1*MASS::mvrnorm(n,rep(0,p+1),9*sigma)
  x = x_error[,1:p]
  error = x_error[,p+1]
  y = x%*%truebeta + error

  #perform variable selection via partial correlation
  TPC.fit = TPC(y,x,0.05,100,method="other")


  expect_equal(as.vector(TPC.fit$beta), truebeta, tolerance = 0.1)
})

test_that("test the main functions (TPC) with BIC", {
  library(TPCselect)
  # parameter settings
  #generate sample data
  p = 200
  n = 200
  truebeta <- c(c(3,1.5,0,0,2),rep(0,p-5))
  rho = 0.3
  sigma =  matrix(0,p+1,p+1)
  for(i in 1:(p+1)){
    for(j in 1:(p+1)){
      sigma[i,j] = rho^(abs(i-j))
    }
  }
  x_error = 0.9*MASS::mvrnorm(n,rep(0,p+1),sigma) + 0.1*MASS::mvrnorm(n,rep(0,p+1),9*sigma)
  x = x_error[,1:p]
  error = x_error[,p+1]
  y = x%*%truebeta + error

  #perform variable selection via partial correlation
  TPC.fit = TPC_BIC(y,x,c(0.05,0.1))


  expect_equal(as.vector(TPC.fit$beta), truebeta, tolerance = 0.2)
})

test_that("test the main functions (TPC) with BIC CV", {
  library(TPCselect)
  # parameter settings
  #generate sample data
  p = 200
  n = 200
  truebeta <- c(c(3,1.5,0,0,2),rep(0,p-5))
  rho = 0.3
  sigma =  matrix(0,p+1,p+1)
  for(i in 1:(p+1)){
    for(j in 1:(p+1)){
      sigma[i,j] = rho^(abs(i-j))
    }
  }
  x_error = 0.9*MASS::mvrnorm(n,rep(0,p+1),sigma) + 0.1*MASS::mvrnorm(n,rep(0,p+1),9*sigma)
  x = x_error[,1:p]
  error = x_error[,p+1]
  y = x%*%truebeta + error

  #perform variable selection via partial correlation
  TPC.fit = TPC_BIC(y,x,c(0.05,0.1),c(0.5,1))


  expect_equal(as.vector(TPC.fit$beta), truebeta, tolerance = 0.2)
})


test_that("test the main functions (TPC) with partial linear model", {
  library(TPCselect)
  #generate partial linear data
  truebeta <- c(c(3,1.5,0,0,2),rep(0,30-5))
  samples <- generate_toy_pldata()
  y <- samples[[1]]
  x <- samples[[2]]
  times <- samples[[3]]

  #perform variable selection via partial correlation
  TPC.fit = TPC_pl(y,x,times,0.05,1,method="threshold")

  expect_equal(as.vector(TPC.fit$beta), truebeta, tolerance = 0.2)
})
