shap1 <- data.frame(
  age = runif(1000, -5, 5),
  income = runif(1000, -5, 5),
  married = runif(1000, -5, 5),
  sex = runif(1000, -5, 5)
)
shap2 <- list(
  data.frame(
    age = runif(1000, -5, 5),
    income = runif(1000, -5, 5),
    married = runif(1000, -5, 5),
    sex = runif(1000, -5, 5)
  ),
  data.frame(
    age = runif(1000, -5, 5),
    income = runif(1000, -5, 5),
    married = runif(1000, -5, 5),
    sex = runif(1000, -5, 5)
  ),
  data.frame(
    age = runif(1000, -5, 5),
    income = runif(1000, -5, 5),
    married = runif(1000, -5, 5),
    sex = runif(1000, -5, 5)
  )
)


ex1 <- 3
ex2 <- c(4, 5, 6)

real_ex1 <- mean((rowSums(shap1) + ex1) * (rowSums(shap2[[1]]) + ex2[1]))


test_that("Basic case works", {
  res <- mshap(
    shap_1 = shap1,
    shap_2 = shap2[[1]],
    ex_1 = ex1,
    ex_2 = ex2[1]
  )
  expect_equal(class(res$expected_value), "numeric")
  expect_equal(class(res$shap_vals), c("tbl_df", "tbl", "data.frame"))
  expect_equal(res$expected_value, real_ex1)
})

test_that("Multi Case Works", {
  res <- mshap(
    shap_1 = shap1,
    shap_2 = shap2,
    ex_1 = ex1,
    ex_2 = ex2
  )
  res2 <- mshap(
    shap_1 = shap2,
    shap_2 = shap1,
    ex_1 = ex2,
    ex_2 = ex1
  )
  expect_equal(class(res[[1]]), "list")
  expect_equal(length(res), 3)
  expect_equal(class(res2[[1]]), "list")
  expect_equal(length(res2), 3)
})

test_that("Different Names Works", {
  res <- mshap(
    shap_1 = shap1,
    shap_2 = shap2,
    ex_1 = ex1,
    ex_2 = ex2,
    shap_1_names = c("Age", "Income", "Married", "Sex"),
    shap_2_names = c("Age", "Income", "Children", "American")
  )
  
  expect_equal(ncol(res[[1]]$shap_vals), 6)
})

test_that("Matrices Work", {
  res <- mshap(
    shap_1 = shap1 %>% as.matrix(),
    shap_2 = shap2[[1]] %>% as.matrix(),
    ex_1 = ex1 %>% as.matrix(),
    ex_2 = ex2[1] %>% as.matrix()
  )
  expect_equal(class(res$expected_value), "numeric")
  expect_equal(class(res$shap_vals), c("tbl_df", "tbl", "data.frame"))
  expect_equal(res$expected_value, real_ex1)
})

test_that("Warnings Work", {
  expect_warning(
    mshap(
      shap_1 = shap1,
      shap_2 = shap2[[1]],
      ex_1 = ex1,
      ex_2 = ex2
    )
  )
  expect_warning(
    mshap(
      shap_1 = shap1,
      shap_2 = shap2[[1]],
      ex_1 = ex2,
      ex_2 = ex1
    )
  )
})

test_that("Error Checking Works", {
  # Error when two list objects are passed
  expect_error(
    mshap(
      shap_1 = shap2,
      shap_2 = shap2,
      ex_1 = ex1,
      ex_2 = ex2
    )
  )
  # Error when the arguments have different numbers of rows
  expect_error(
    mshap(
      shap_1 = shap1[-1,],
      shap_2 = shap2,
      ex_1 = ex1,
      ex_2 = ex2
    )
  )
  # Error when only one of the names arguments is supplied
  expect_error(
    mshap(
      shap_1 = shap1,
      shap_2 = shap2,
      ex_1 = ex1,
      ex_2 = ex2,
      shap_1_names = c("Age", "Income", "Married", "Sex")
    )
  )
  # Error when not all elements of the data frames are numeric
  expect_error(
    mshap(
      shap_1 = shap1 %>% mutate(age = as.character(age)),
      shap_2 = shap2,
      ex_1 = ex1,
      ex_2 = ex2
    )
  )
  # Error when the expected values are not numeric
  expect_error(
    mshap(
      shap_1 = shap1 %>% mutate(age = as.character(age)),
      shap_2 = shap2,
      ex_1 = "a",
      ex_2 = ex2
    )
  )
  # Error when names is not supplied and the data frame are different dimensions
  expect_error(
    mshap(
      shap_1 = shap1 %>% mutate(newvar = 1:nrow(.)),
      shap_2 = shap2,
      ex_1 = ex1,
      ex_2 = ex2
    )
  )
})

