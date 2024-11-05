test_that("errors", {
  expect_error(sim_joint_dist())
  expect_error(sim_joint_dist(mtcars, A), 
               "Some columns were not in the data table: A", fixed = TRUE)
})

# cols ----
test_that("cols", {
  mtnew <- sim_joint_dist(mtcars, cyl, vs)
  expect_equal(names(mtnew), c("cyl", "vs"))
  expect_equal(nrow(mtnew), 100)
  
  mtnew <- sim_joint_dist(mtcars, "cyl", "vs", n = 20)
  expect_equal(names(mtnew), c("cyl", "vs"))
  expect_equal(nrow(mtnew), 20)
  
  mtnew <- sim_joint_dist(mtcars, "cyl", vs, n = 20)
  expect_equal(names(mtnew), c("cyl", "vs"))
  expect_equal(nrow(mtnew), 20)
  
  var <- "vs"
  mtnew <- sim_joint_dist(mtcars, "cyl", !!var, n = 20)
  expect_equal(names(mtnew), c("cyl", "vs"))
  expect_equal(nrow(mtnew), 20)
})

# empirical ----
test_that("empirical", {
  data <- data.frame(
    A = rep(1:2, each = 10),
    B = rep(1:2, 10)
  )
  
  set.seed(1)
  not_emp <- sim_joint_dist(data)
  n <- dplyr::count(not_emp, A, B)$n
  expect_equal(n == c(25, 25, 25, 25), c(F, F, F, F))
  
  set.seed(1)
  not_emp2 <- sim_joint_dist(data, empirical = FALSE)
  n <- dplyr::count(not_emp2, A, B)$n
  expect_equal(n == c(25, 25, 25, 25), c(F, F, F, F))
  
  set.seed(1)
  emp <- sim_joint_dist(data, empirical = TRUE)
  n <- dplyr::count(emp, A, B)$n
  expect_equal(n, c(25, 25, 25, 25))
  
  # more categories than n
  set.seed(1)
  data <- expand.grid(A = 1:10, B = 1:10)
  emp <- sim_joint_dist(data, A, B, n = 10, empirical = TRUE)
  n <- dplyr::count(emp, A, B)$n
  expect_equal(n, rep(1, 10))
  
  # samples differently each time
  emp2 <- sim_joint_dist(data, A, B, n = 10, empirical = TRUE)
  expect_false(all(emp$A == emp2$A))
})