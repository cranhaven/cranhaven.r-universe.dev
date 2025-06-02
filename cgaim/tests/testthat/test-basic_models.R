
#----------------------------
# Create data
#----------------------------

#---- A very simple single-index model
set.seed(1989)
n <- 200
x1 <- rnorm(n)
x2 <- x1 + rnorm(n)
z <- x1 + x2
y <- z + rnorm(n)
df1 <- data.frame(y, x1, x2)

#---- A simple two-index model
set.seed(2020)
n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
x4 <- rnorm(n)
mu <- 4 * exp(8 * x1) / (1 + exp(8 * x1)) + exp(x3)
y <- mu + rnorm(n)
df2 <- data.frame(y, x1, x2, x3, x4)

#----------------------------
# Test models without constraints
#----------------------------
ans <- cgaim(y ~ g(x1, x2), data = df1)
cia <- confint(ans, B = 10)
plot(ans, ci = cia)

ans <- cgaim(y ~ g(x1, x2) + g(x3, x4), data = df2)
cia <- confint(ans, B = 10)
plot(ans, ci = cia, select = 2)


#----------------------------
# Test alpha constraints
#----------------------------

#----- Monotonicity constraints
# Single-index
ans1 <- cgaim(y ~ g(x1, x2, acons = list(monotone = 1)), 
  data = df1)
ans2 <- cgaim(y ~ g(x1, x2, acons = list(monotone = -1)), 
  data = df1)

# Two-index
ans3 <- cgaim(y ~ g(x1, x2, acons = list(monotone = 1)) + 
    g(x3, x4, Cmat = diff(diag(2))), 
  data = df2)
ans4 <- cgaim(y ~ g(x1, x2, acons = list(monotone = -1)) + 
    g(x3, x4, Cmat = -diff(diag(2))), 
  data = df2)
  
test_that("monotonicity constraints on alpha work", {
  expect_gte(t(c(-1, 1)) %*% unlist(ans1$alpha), 0)
  expect_gte(t(c(1, -1)) %*% unlist(ans2$alpha), 0)
  expect_true(all(Matrix::bdiag(diff(diag(2)), diff(diag(2))) %*% 
      unlist(ans3$alpha) >= 0))
  expect_true(all(Matrix::bdiag(-diff(diag(2)), -diff(diag(2))) %*% 
      unlist(ans4$alpha) >= 0))
})

#----- Sign constraints
# Single-index
ans2 <- cgaim(y ~ g(x1, x2, acons = list(sign = -1)), 
  data = df1)

# Two-index
ans3 <- cgaim(y ~ g(x1, x2, acons = list(sign = 1)) + 
    g(x3, x4, acons = list(sign = 1)), 
  data = df2)
ans4 <- cgaim(y ~ g(x1, x2, acons = list(sign = -1)) + 
    g(x3, x4, acons = list(sign = -1)), 
  data = df2)
  
test_that("sign constraints on alpha work", {
  expect_true(all(-diag(2) %*% unlist(ans2$alpha) > 0))
  expect_true(all(diag(4) %*% unlist(ans3$alpha) > 0))
  expect_true(all(-diag(4) %*% unlist(ans4$alpha) > 0))
})

#----------------------------
# Test smoothing constraints
#----------------------------

#----- Monotone increasing constraints
ans1 <- cgaim(y ~ g(x1, x2, fcons = "inc"), 
  data = df1, control = list(sm_method = "scam"))
ans2 <- cgaim(y ~ g(x1, x2, fcons = "inc") + g(x3, x4, fcons = "inc"), 
  data = df2, control = list(sm_method = "scam"))

test_that("Monotone increasing constraint on smooths works with 'scam'",{
  expect_true(all(diff(ans1$gfit[order(ans1$indexfit)]) >= 0))
  expect_true(all(round(diff(ans2$gfit[order(ans2$indexfit[,1]), 1]), 10) >= 0))
  expect_true(all(round(diff(ans2$gfit[order(ans2$indexfit[,2]), 2]), 10) >= 0))
})

ans1 <- cgaim(y ~ g(x1, x2, fcons = "inc"), 
  data = df1, control = list(sm_method = "scar"))
ans2 <- cgaim(y ~ g(x1, x2, fcons = "inc") + g(x3, x4, fcons = "inc"), 
  data = df2, control = list(sm_method = "scar"))

test_that("Monotone increasing constraint on smooths works with 'scar'",{
  expect_true(all(round(diff(ans1$gfit[order(ans1$indexfit)]), 10) >= 0))
  expect_true(all(round(diff(ans2$gfit[order(ans2$indexfit[,1]), 1]), 10) >= 0))
  expect_true(all(round(diff(ans2$gfit[order(ans2$indexfit[,2]), 2]), 10) >= 0))
})

ans1 <- cgaim(y ~ g(x1, x2, fcons = "inc"),
  data = df1, control = list(sm_method = "cgam"))
ans2 <- cgaim(y ~ g(x1, x2, fcons = "inc") + g(x3, x4, fcons = "inc"),
  data = df2, control = list(sm_method = "cgam"))

test_that("Monotone increasing constraint on smooths works with 'cgam'",{
  expect_true(all(diff(ans1$gfit[order(ans1$indexfit)]) >= 0))
  expect_true(all(diff(ans2$gfit[order(ans2$indexfit[,1]), 1]) >= 0))
  expect_true(all(diff(ans2$gfit[order(ans2$indexfit[,2]), 2]) >= 0))
})

#----- Monotone decreasing constraints
ans1 <- cgaim(y ~ g(x1, x2, fcons = "dec"), 
  data = df1, control = list(sm_method = "scam"))
ans2 <- cgaim(y ~ g(x1, x2, fcons = "dec") + g(x3, x4, fcons = "dec"), 
  data = df2, control = list(sm_method = "scam"))

test_that("Monotone increasing constraint on smooths works with 'scam'",{
  expect_true(all(round(diff(ans1$gfit[order(ans1$indexfit)]), 10) <= 0))
  expect_true(all(round(diff(ans2$gfit[order(ans2$indexfit[,1]), 1]), 10) <= 0))
  expect_true(all(round(diff(ans2$gfit[order(ans2$indexfit[,2]), 2]), 10) <= 0))
})

ans1 <- cgaim(y ~ g(x1, x2, fcons = "dec"), 
  data = df1, control = list(sm_method = "scar"))
ans2 <- cgaim(y ~ g(x1, x2, fcons = "dec") + g(x3, x4, fcons = "dec"), 
  data = df2, control = list(sm_method = "scar"))

test_that("Monotone increasing constraint on smooths works with 'scar'",{
  expect_true(all(round(diff(ans1$gfit[order(ans1$indexfit)]), 10) <= 0))
  expect_true(all(round(diff(ans2$gfit[order(ans2$indexfit[,1]), 1]), 10) <= 0))
  expect_true(all(round(diff(ans2$gfit[order(ans2$indexfit[,2]), 2]), 10) <= 0))
})

ans1 <- cgaim(y ~ g(x1, x2, fcons = "dec"),
  data = df1, control = list(sm_method = "cgam"))
ans2 <- cgaim(y ~ g(x1, x2, fcons = "dec") + g(x3, x4, fcons = "dec"),
  data = df2, control = list(sm_method = "cgam"))

test_that("Monotone increasing constraint on smooths works with 'cgam'",{
  expect_true(all(diff(ans1$gfit[order(ans1$indexfit)]) <= 0))
  expect_true(all(diff(ans2$gfit[order(ans2$indexfit[,1]), 1]) <= 0))
  expect_true(all(diff(ans2$gfit[order(ans2$indexfit[,2]), 2]) <= 0))
})

#----------------------------
# Model with a covariate and more complex setting
#----------------------------

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
cia <- confint(ans, B = 5)
plot(ans, ci = cia, select = 3)
