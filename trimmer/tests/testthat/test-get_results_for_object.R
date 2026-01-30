context("get_results_for_object()")

# load training data.
trn <- datasets::mtcars

# estimate model.
mdl <- lm(mpg ~ ., data = trn)

# get results.
p_init <- predict(mdl, trn)

test_that("Expected results", {
  
  p <- get_results_for_object(obj = mdl, obj_arg_name = "object", newdata = trn, fun = predict)
  expect_identical(p, p_init)
  
})

test_that("Expected results with additional args", {
  
  p_init_terms <- predict(mdl, trn, type = "terms")
  p <- get_results_for_object(obj = mdl, obj_arg_name = "object", fun = predict, newdata = trn, type = "terms")
  expect_identical(p, p_init_terms)
  
  # additional args with no names.
  p <- get_results_for_object(obj = mdl, obj_arg_name = "object", fun = predict, trn)
  expect_identical(p, p_init)
  
  # mixed.
  p <- get_results_for_object(obj = mdl, obj_arg_name = "object", fun = predict, trn, type = "terms")
  expect_identical(p, p_init_terms)
  
})



