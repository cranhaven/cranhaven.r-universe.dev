
# model set up -------------------------------------------------

# set up basic lm's to test properties of smelt()
lm_1 = stats::lm(mpg ~ cyl + disp + hp, data = mtcars)
lm_2 = stats::lm(mpg ~ hp + drat + wt, data = mtcars)
lm_3 = stats::lm(mpg ~ ., data = mtcars)
lm_metrics = stack_metrics(lm_1, lm_2, lm_3)
lm_coeff = stack_coeff(lm_1, lm_2, lm_3)

# set up basic glm's to test properties of smelt()
glm_1 = stats::glm(vs ~ drat + hp, data = mtcars)
glm_2 = stats::glm(vs ~ wt + qsec, data = mtcars)
glm_3 = stats::glm(vs ~ ., data = mtcars)
glm_metrics = stack_metrics(glm_1, glm_2, glm_3)
glm_coeff = stack_coeff(glm_1, glm_2, glm_3)

lmer_1 = lme4::lmer(Sepal.Length ~ (1 | Species), data = iris)
lmer_2 = lme4::lmer(Sepal.Length ~ (1 | Species) + Petal.Length, data = iris)
lmer_metrics = stack_metrics(lmer_1, lmer_2)

# testing functions --------------------------------------------
# testing stack_metrics() --------------------------------------
test_that("dimension of output tibble are correct", {
  expect_equal(dim(lm_metrics), c(3,6))
  expect_equal(dim(glm_metrics), c(3,4))
  expect_equal(dim(lmer_metrics), c(2,4))
})

test_that("output is correctly a tibble", {
  expect_s3_class(lm_metrics[1], 'tbl_df')
  expect_s3_class(glm_metrics[1], 'tbl_df')
  expect_s3_class(lmer_metrics[1], 'tbl_df')
})

test_that("correct columns are returned", {
  expect_equal(names(lm_metrics),
               c("model", "r.squared", "adj.r.squared", "MSE", "RMSE", "MAE"))
  expect_equal(names(glm_metrics),
               c("model", "deviance", "AIC", "BIC"))
  expect_equal(names(lmer_metrics),
               c("model", "deviance", "AIC", "BIC"))
})

test_that("data outputs are equal", {
  expect_snapshot_value(stack_metrics(lm_1, lm_2, lm_3), style = 'json2')
  expect_snapshot_value(stack_metrics(glm_1, glm_2, glm_3), style = 'json2')
  expect_snapshot_value(stack_metrics(lmer_1, lmer_2), style = 'json2')
})

# testing stack_coeff() ----------------------------------------
test_that("dimension of output tibble are correct", {
  expect_equal(dim(lm_coeff), c(19, 7))
  expect_equal(dim(glm_coeff), c(17, 7))
})

test_that("output is correctly a tibble", {
  expect_s3_class(lm_coeff[1], 'tbl_df')
  expect_s3_class(glm_coeff[1], 'tbl_df')
})

test_that("correct columns are returned", {
  expect_equal(names(lm_coeff),
               c("coefficient", "model_name", "estimate", "std_error",
                 "p_value", "lower_ci", "upper_ci"))
  expect_equal(names(glm_coeff),
               c("coefficient", "model_name", "estimate", "std_error",
                 "p_value", "lower_ci", "upper_ci"))
})

test_that("data outputs are equal", {
  expect_snapshot_value(stack_coeff(lm_1, lm_2, lm_3), style = 'json2')
  expect_snapshot_value(stack_coeff(glm_1, glm_2, glm_3), style = 'json2')
})


