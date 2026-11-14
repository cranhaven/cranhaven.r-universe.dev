## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(nadir)

## -----------------------------------------------------------------------------
super_learner(
  data = iris,
  formula = Petal.Width ~ Petal.Length + Sepal.Length + Sepal.Width,
  learners = list(lnr_lm, lnr_rf, lnr_earth, lnr_mean))

## -----------------------------------------------------------------------------
# We recommend storing more complicated arguments used repeatedly to simplify 
# the call to super_learner()
petal_formula <- Petal.Width ~ Petal.Length + Sepal.Length + Sepal.Width
learners <- list(lnr_lm, lnr_rf, lnr_earth, lnr_mean)

sl_model <- super_learner(
  data = iris,
  formula = petal_formula,
  learners = learners)

## -----------------------------------------------------------------------------
predict(sl_model, iris) |> head()

## -----------------------------------------------------------------------------
predict(sl_model, iris[sample.int(size = 10, n = nrow(iris)), ]) |> 
  head()

## -----------------------------------------------------------------------------
fake_iris_data <- data.frame()
fake_iris_data <- cbind.data.frame(
  Sepal.Length = 
  rnorm(
    n = 6,
    mean = mean(iris$Sepal.Length),
    sd = sd(iris$Sepal.Length)
  ),

Sepal.Width = 
  rnorm(
    n = 6,
    mean = mean(iris$Sepal.Width),
    sd = sd(iris$Sepal.Width)
  ),

Petal.Length = 
  rnorm(
    n = 6,
    mean = mean(iris$Petal.Length),
    sd = sd(iris$Petal.Length)
  )
)

predict(sl_model, fake_iris_data) |> 
  head()

## -----------------------------------------------------------------------------
sl_model_iris <- super_learner(
  data = iris,
  formula = petal_formula,
  learners = learners)

str(sl_model_iris, max.level = 2)

## -----------------------------------------------------------------------------
compare_learners(sl_model_iris)

cv_super_learner(
  data = iris, 
  formula = petal_formula,
  learners = learners)$cv_loss

## -----------------------------------------------------------------------------
var_residuals <- var(iris$Sepal.Length - predict(sl_model_iris, iris))
total_variance <- var(iris$Sepal.Length)
variance_explained <- total_variance - var_residuals 

rsquared <- variance_explained / total_variance
print(rsquared)

