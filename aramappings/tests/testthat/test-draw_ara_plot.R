tolerance <- 0.001

set.seed(1000000)

#################################   Set data   #################################


X <- wine[, 2:ncol(wine)] # Select a subset of variables
X <- apply(as.matrix.noquote(X), 2, as.numeric)

Z <- scale(X) # standardize

N <- nrow(Z)
n <- ncol(Z)


m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)

mapping <- ara_unconstrained_l2(Z, V)
P <- mapping$P

###############################  Test arguments  ###############################

# Test types

Zcopy <- Z
Zcopy[1, 1] <- "a"
test_that("Function halts if Z is not numeric", {
  expect_error(draw_ara_plot_2d_standardized(Zcopy, X, V, P))
})

Zlist <- as.list(Z)
test_that("Function halts if Z is not a matrix", {
  expect_error(draw_ara_plot_2d_standardized(Zlist, X, V, P))
})

Xcopy <- X
Xcopy[1, 1] <- "a"
test_that("Function halts if X is not numeric", {
  expect_error(draw_ara_plot_2d_standardized(Z, Xcopy, V, P))
})

Xlist <- as.list(X)
test_that("Function halts if X is not a matrix", {
  expect_error(draw_ara_plot_2d_standardized(Z, Xlist, V, P))
})

Vcopy <- V
Vcopy[1, 1] <- "a"
test_that("Function halts if V is not numeric", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, Vcopy, P))
})

Vlist <- as.list(V)
test_that("Function halts if V is not a matrix", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, Vlist, P))
})

Pcopy <- P
Pcopy[1, 1] <- "a"
test_that("Function halts if P is not numeric", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, Pcopy))
})

Plist <- as.list(P)
test_that("Function halts if P is not a matrix", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, Plist))
})


wcopy <- runif(n, 0, 1)
wcopy[1] <- "1"
test_that("Function halts if weights is not numeric", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P, weights = wcopy))
})


axis_lines <- c("2", "4", "7")
test_that("Function halts if axis_lines is not numeric", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P,
    axis_lines = axis_lines
  ))
})


test_that("Function halts if color_variable is not numeric", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P, color_variable = "1"))
})

test_that("Function halts if color_variable is not an integer", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P, color_variable = 1.5))
})


test_that("Function halts if mismatch in number of observations", {
  expect_error(draw_ara_plot_2d_standardized(Z[2:N, ], X, V, P))
})

test_that("Function halts if mismatch in number of observations", {
  expect_error(draw_ara_plot_2d_standardized(Z, X[2:N, ], V, P))
})

test_that("Function halts if mismatch in number of observations", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P[2:N, ]))
})

test_that("Function halts if mismatch in number of variables", {
  expect_error(draw_ara_plot_2d_standardized(Z[, 2:n], X, V, P))
})

test_that("Function halts if mismatch in number of variables", {
  expect_error(draw_ara_plot_2d_standardized(Z, X[, 2:n], V, P))
})

test_that("Function halts if mismatch in number of variables", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V[2:n, ], P))
})

m <- 3
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
test_that("Function halts if visualization space is not bidimensional", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P))
})

mapping <- ara_unconstrained_l2(Z, V)
P <- mapping$P
m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
test_that("Function halts if visualization space is not bidimensional", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P))
})


# Test values
mapping <- ara_unconstrained_l2(Z, V)
P <- mapping$P

Zcopy <- Z
Zcopy[1, 1] <- NA
test_that("Function halts if Z contains missing values (NA)", {
  expect_error(draw_ara_plot_2d_standardized(Zcopy, X, V, P))
})

Xcopy <- X
Xcopy[1, 1] <- NA
test_that("Function halts if X contains missing values (NA)", {
  expect_error(draw_ara_plot_2d_standardized(Z, Xcopy, V, P))
})

Vcopy <- V
Vcopy[1, 1] <- NA
test_that("Function halts if V contains missing values (NA)", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, Vcopy, P))
})

Pcopy <- P
Pcopy[1, 1] <- NA
test_that("Function halts if P contains missing values (NA)", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, Pcopy))
})

wcopy <- runif(n, 0, 1)
wcopy[1] <- NA
test_that("Function halts if weights contains missing values (NA)", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P, weights = wcopy))
})


# Additional preconditions on input parameters -------------------------

mapping <- ara_unconstrained_l2(Z, V)
P <- mapping$P
test_that("Function halts if weights does not contain n entries", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P,
    weights = c(1, 0.5, 1)
  ))
})

w <- runif(n + 1, 0, 1)
test_that("Function halts if weights does not contain n entries", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P, weights = w))
})

w <- runif(n, 0, 1)
w[1] <- -1
test_that("Function halts if weights contains negative entries", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P, weights = w))
})

test_that("Function halts if axis_lines contains duplicate entries", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P,
    axis_lines = c(2, 4, 2)
  ))
})

test_that("Function halts if axis_lines contains entries outside [1,n]", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P,
    axis_lines = c(0, 2, 4)
  ))
})

test_that("Function halts if axis_lines contains entries outside [1,n]", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P,
    axis_lines = c(1, 2, 15)
  ))
})

test_that("Function halts if axis_lines contains fractional entries", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P,
    axis_lines = c(1, 2.5, 7)
  ))
})

test_that("Function halts if axis_lines contains more than n entries", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P,
    axis_lines = 1:(n + 1)
  ))
})

test_that("Function halts if color_variable is not an integer in [1,n]", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P,
    color_variable = 0
  ))
})

test_that("Function halts if color_variable is not an integer in [1,n]", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P,
    color_variable = n + 1
  ))
})

test_that("Function halts if color_variable is not an integer in [1,n]", {
  expect_error(draw_ara_plot_2d_standardized(Z, X, V, P,
    color_variable = 1.5
  ))
})






###################  Test draw_ara_plot_2d_standardized()  #####################

weights <- runif(n, 0, 1)
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
mapping <- ara_unconstrained_l2(Z, V, weights = weights, solver = "formula")
P <- mapping$P
axis_lines <- c(2, 4, 7, 9)
color_variable <- 3


test_that("draw_ara_plot_2d_standardized() generates a ggplot object", {
  expect_match(
    class(draw_ara_plot_2d_standardized(Z, X, V, P,
      weights = weights,
      axis_lines = axis_lines,
      color_variable = color_variable
    )),
    "ggplot2::ggplot",
    fixed=TRUE,
    all=FALSE
  )
})


