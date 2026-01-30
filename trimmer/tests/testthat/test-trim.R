context("trim()")

# load training data.
trn <- datasets::mtcars

# estimate model.
mdl <- lm(mpg ~ ., data = trn)

# get results.
p_init <- predict(mdl, trn)

test_that("Expected results from main function", {
  
  # NOTE: pryr::object_size() seems to behave crazy here for some reason, using object.size()
  # in stead.
  # try to trim object.
  suppressWarnings(res <- trim(obj = mdl, obj_arg_name = "object", fun = predict, size_target = 0, tolerate_warnings = TRUE, newdata = trn, verbose = FALSE))
  expect_is(res, "lm")
  expect_true(object.size(res) < object.size(mdl))
  
  # same results as before trimming.
  p_new <- predict(res, trn)
  expect_identical(p_init, p_new)
  
})

test_that("Works without providing 'obj_arg_name'", {
  
  expect_warning({res <- trim(obj = mdl, fun = predict, size_target = 0, newdata = trn, verbose = FALSE, tolerate_warnings = FALSE,)})
  expect_is(res, "lm")
  expect_true(object.size(res) < object.size(mdl))
  
  # same results as before trimming.
  p_new <- predict(res, trn)
  expect_identical(p_init, p_new)
  
})

test_that("Expected results when no trimming is actually conducted", {
  
  res <- trim(obj = mdl, fun = predict, size_target = 1e06, newdata = trn, tolerate_warnings = TRUE, verbose = FALSE)
  expect_identical(mdl, res)
  
})

test_that("expected behavior if target size has been set and cannot be achieved", {
  
  expect_warning({res <- trim(obj = mdl, fun = predict, size_target = 1e-09, newdata = trn, tolerate_warnings = FALSE, verbose = FALSE)})
  expect_is(res, "lm")
  expect_true(object.size(res) < object.size(mdl))
  
  # same results as before trimming.
  p_new <- predict(res, trn)
  expect_identical(p_init, p_new)
  
})

context("'dont_touch'") 

test_that("single entry, shallow", {
  
  # benchmark.
  res <- trim(obj = mdl, 
              obj_arg_name = "object", 
              fun = predict, 
              size_target = 0, 
              tolerate_warnings = FALSE, 
              newdata = trn, 
              verbose = FALSE,
              dont_touch = list())
  expect_true(is.null(res$model))
  expect_true(is.null(res$qr$tol))
  
  res <- trim(obj = mdl, 
              obj_arg_name = "object", 
              fun = predict, 
              size_target = 0, 
              tolerate_warnings = FALSE, 
              newdata = trn, 
              verbose = FALSE,
              dont_touch = list(c("model")))
  expect_true(!is.null(res$model))
  
})

test_that("single entry, deep", {
  
  res <- trim(obj = mdl, 
              obj_arg_name = "object", 
              fun = predict, 
              size_target = 0, 
              tolerate_warnings = FALSE, 
              newdata = trn, 
              verbose = FALSE,
              dont_touch = list(c("qr", "tol")))
  expect_true(!is.null(res$qr$tol))
  
})

test_that("multiple entries", {
  
  res <- trim(obj = mdl, 
              obj_arg_name = "object", 
              fun = predict, 
              size_target = 0, 
              tolerate_warnings = FALSE, 
              newdata = trn, 
              verbose = FALSE,
              dont_touch = list(c("model"), c("qr", "tol")))
  expect_true(!is.null(res$model))
  expect_true(!is.null(res$qr$tol))
  
})

