context("Wrapper")

data1 <- get(load('../data/dataClass.RData'))
data2 <- get(load("../data/dataReg.RData"))

test_that("Classification", {

  # Wrapper method
  resamplingParams <- list(method = "cv", number = 5)
  fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy", tuneGrid = expand.grid(k = seq(1,10,by=2)))
  wra <- wrapperEvaluator("knn",resamplingParams, fittingParams) # wrapper method

  expect_is(wra(data1,"y",c("x1","x2","x3","x4")), "numeric")
  expect_that(wra(data1,"y",c("x1","x4")), is_a("numeric"))

  expect_is(wra(iris,"Species",c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")), "numeric")
  expect_that(wra(iris,"Species","Sepal.Length"), is_a("numeric"))

})

test_that("Regression", {

  # Wrapper method
  resamplingParams <- list(method = "cv", number = 3)
  fittingParams <- list(preProcess = c("center", "scale"), metric="RMSE")
  wra <- wrapperEvaluator("lm",resamplingParams, fittingParams) # wrapper method

  expect_is(wra(data2,"y",c("x1","x2","x3","x4")), "numeric")
  expect_that(wra(data2,"y",c("x1","x4")), is_a("numeric"))

  expect_is(wra(mtcars,"mpg",c("cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb")), "numeric")
  expect_that(wra(mtcars,"mpg","disp"), is_a("numeric"))

})
