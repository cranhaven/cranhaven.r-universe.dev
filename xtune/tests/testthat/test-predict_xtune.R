set.seed(1234)
test_that("test for predict",{
  X <- matrix(runif(500), nrow = 100)
  Y <- 1:100
  Z <- matrix(runif(100), nrow = 5)
  xtune_fitted <- xtune(X,Y,Z, family = "linear")
  expect_error(predict_xtune(xtune_fitted),paste("You need to supply a value for 'newX'"))
  newX <- matrix(runif(25), nrow = 5)
  newX[1,1] <-"1"
  expect_error(predict_xtune(xtune_fitted,newX = newX),paste("New X contains non-numeric values"))

  newX <- matrix(runif(20), nrow = 5)
  expect_error(predict_xtune(xtune_fitted,newX),paste("New X does not have the same number of columns as X train"))

  newX <- matrix(runif(25), nrow = 5)
  expect_equal(matrix(predict_xtune(xtune_fitted,newX, type = "response")),matrix(cbind(1,newX)%*%(xtune_fitted$beta.est)))

  Y <- rbinom(100,1,0.5)
  xtune_fitted <- xtune(X,Y,Z, family = "binary")
  expect_equal(sum(!predict_xtune(xtune_fitted,newX = newX,type = "class",X=X,Y=Y)%in%c(0,1)),0)
})
