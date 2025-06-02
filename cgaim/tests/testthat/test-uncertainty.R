
#----- A model
set.seed(2020)
n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
x4 <- rnorm(n)
x5 <- rnorm(n)
mu <- 4 * exp(8 * x1) / (1 + exp(8 * x1)) + exp(x3) + sin(x4 + x5)
y <- mu + rnorm(n)
df3 <- data.frame(y, x1, x2, x3, x4, x5)

ans <- cgaim(y ~ g(x1, x2, label = "i1", s_opts = list(sp = 0)) + 
    s(x3, s_opts = list(bs = "cr")) + g(x4, x5, label = "i2", fcons = "inc"),
  data = df3)

#----- Check confint

test_that("both methods work", {
  cin <- confint(ans, type = "norm", B = 10)
  expect_equal(dim(cin$alpha), c(4, 2))
  expect_equal(dim(cin$beta), c(4, 2))
  
  cib <- confint(ans, type = "boot", B = 10)
  expect_equal(dim(cib$alpha), c(4, 2))
  expect_equal(dim(cib$beta), c(4, 2))
})

test_that("parm argument works", {
  cin <- confint(ans, parm = 1, type = "norm", B = 10)
  expect_length(cin, 1)
  expect_named(cin, "alpha")
  
  cin <- confint(ans, parm = 2, type = "norm", B = 10)
  expect_length(cin, 1)
  expect_named(cin, "beta")
  
  cin <- confint(ans, parm = c("beta", "alpha"), type = "norm", B = 10)
  expect_length(cin, 2)
  expect_named(cin, c("alpha", "beta"))
  
  cib <- confint(ans, parm = c("g", "alpha"), type = "boot", B = 10)
  expect_length(cib, 2)
  expect_named(cib, c("alpha", "g"))
})

test_that("both confint methods are equivalent", {
  
  # Calling confint.cgaim directly
  set.seed(1)
  cib1 <- confint(ans, type = "boot", B = 10)
  
  # Creating replications first
  set.seed(1)
  bans <- boot.cgaim(ans, B = 10)
  cib2 <- confint(bans)
  
  # Check they are identical
  expect_equal(cib1, cib2)
})

#----- Check vcov

test_that("vcov methods work", {
  v <- vcov(ans, type = "norm", B = 10)
  expect_equal(colnames(v), names(unlist(ans$alpha)))
  
  v <- vcov(ans, type = "boot", B = 10)
  expect_equal(colnames(v), names(unlist(ans$alpha)))
})

test_that("vcov parameter specification works", {
  v <- vcov(ans); expect_equal(colnames(v), names(unlist(ans$alpha)))
  v <- vcov(ans, parm = "alpha"); expect_equal(colnames(v), 
    names(unlist(ans$alpha)))
  v <- vcov(ans, parm = "beta"); expect_equal(colnames(v), names(ans$beta))
  expect_error(vcov(ans, parm = "kappa"))
  v <- vcov(ans, parm = "a"); expect_equal(colnames(v), 
    names(unlist(ans$alpha)))
  v <- vcov(ans, parm = "b"); expect_equal(colnames(v), names(ans$beta))
  v <- vcov(ans, parm = NULL); expect_equal(colnames(v), 
    names(unlist(ans$alpha)))
})

test_that("both bootstrap vcov methods are equivalent", {
  
  # Calling vcov.cgaim directly
  set.seed(1)
  v1 <- vcov(ans, type = "boot", B = 10)
  
  # Creating replications first
  set.seed(1)
  bans <- boot.cgaim(ans, B = 10)
  v2 <- vcov(bans)
  
  # Check they are identical
  expect_equal(v1, v2)
})

test_that("vcov with constraints work", {
  
  # new model with constraints on alpha coefficients
  ans2 <- cgaim(y ~ g(x1, x2, label = "i1", s_opts = list(sp = 0), 
      acons = list(monotone = -1)) + s(x3, s_opts = list(bs = "cr")) + 
      g(x4, x5, label = "i2", fcons = "inc", acons = list(sign = 1)),
    data = df3)
  
  # Check we get results
  v <- vcov(ans2, type = "norm", B = 10)
  expect_equal(colnames(v), names(unlist(ans$alpha)))
  
  v <- vcov(ans2, type = "boot", B = 10)
  expect_equal(colnames(v), names(unlist(ans$alpha)))
})