context("get_params")

# error messages ----
test_that("error messages", {
  expect_error(get_params("A"), "data must be a data frame or matrix")
  expect_error(get_params(iris, FALSE), "between must be a numeric or character vector")
})

# defaults ----
test_that("defaults", {
  checkiris <- get_params(iris)
  irisnames <- c("n", "var", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "mean", "sd")
  
  expect_equal(nrow(checkiris), 4)
  expect_equal(ncol(checkiris), 8)
  expect_equal(names(checkiris), irisnames)
})

# defaults with between ----
test_that("defaults with between", {
  checkiris <- get_params(iris, "Species")
  irisnames <- c("Species", "n", "var", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "mean", "sd")
  
  expect_equal(nrow(checkiris), 12)
  expect_equal(ncol(checkiris), 9)
  expect_equal(names(checkiris), irisnames)
})

# long ----
test_that("long", {
  df_long <- sim_design(within = 2, between = 2, r = 0.5, 
                        empirical = TRUE, long = TRUE, plot = FALSE)
  checklong <- get_params(df_long)
  
  expect_equal(checklong$B1, c("B1a", "B1a", "B1b", "B1b") %>% as.factor())
  expect_equal(checklong$n, c(100,100,100,100))
  expect_equal(checklong$var, factor(c("W1a", "W1b", "W1a", "W1b")))
  expect_equal(checklong$mean, c(0,0,0,0))
  expect_equal(checklong$sd, c(1,1,1,1))
  expect_equal(checklong$W1a, c(1,.5,1,.5))
  expect_equal(checklong$W1b, c(.5,1,.5,1))
})

# is_pos_def ----
test_that("is_pos_def", {
  expect_equal(is_pos_def(matrix(c(1, .5, .5, 1), 2)), TRUE)
  
  bad_matrix <- matrix(c(1, .9, .9, 
                        .9, 1, -.2,
                        .9, -.2, 1), 3)
  expect_equal(is_pos_def(bad_matrix), FALSE)
})


# order ----
test_that("order", {
  x <- sim_design(
    within = list(time = c("pre", "post"),
                  condition = c("ctl", "exp"))
  )
  
  p <- get_params(x)
  expect_equal(as.character(p$var), names(p)[3:6])
  
  x <- sim_design(
    between = list(grp = c("B", "A")),
    within = list(time = c("pre", "post"),
                  condition = c("ctl", "exp"))
  )
  p <- get_params(x, between = "grp")
  expect_equal(as.character(p$grp), rep(LETTERS[2:1], each = 4))
  expect_equal(as.character(p$var), rep(names(p)[4:7], 2))
})


# from design ----
test_that("from design", {
  x <- sim_design(
    between = list(grp = c("B", "A")),
    within = list(time = c("pre", "post"),
                  condition = c("ctl", "exp"))
  )
  p <- get_params(x)
  expect_equal(as.character(p$grp), rep(LETTERS[2:1], each = 4))
  expect_equal(as.character(p$var), rep(names(p)[4:7], 2))
  
  # override between
  p <- get_params(x, between = 0)
  expect_true(!"grp" %in% names(p))
  expect_equal(as.character(p$var), names(p)[3:6])
  
  # override dv
  p <- get_params(x, dv = c("pre_exp", "post_ctl"))
  expect_equal(as.character(p$grp), rep(LETTERS[2:1], each = 2))
  expect_equal(as.character(p$var), rep(c("pre_exp", "post_ctl"), 2))
  
  # when long
  x <- sim_design(
    between = list(grp = c("B", "A")),
    within = list(time = c("pre", "post"),
                  condition = c("ctl", "exp")),
    long = TRUE
  )
  p <- get_params(x)
  expect_equal(as.character(p$grp), rep(LETTERS[2:1], each = 4))
  expect_equal(as.character(p$var), rep(names(p)[4:7], 2))
})

  