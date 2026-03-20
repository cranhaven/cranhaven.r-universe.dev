tolerance <- 0.001

set.seed(1000000)

#################################   Set data   #################################

X <- wine[, 2:ncol(wine)] # Select a subset of variables

X <- scale(X) # standardize

N <- nrow(X)
n <- ncol(X)

###############################  Test arguments  ###############################

m <- 3
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
Xcopy <- X
Xcopy[1, 1] <- "a"
test_that("Function halts if X is not numeric", {
  expect_error(ara_unconstrained_l2(Xcopy, V))
})

Xlist <- as.list(X)
test_that("Function halts if X is not a matrix", {
  expect_error(ara_unconstrained_l2(Xlist, V))
})

V[1, 1] <- "a"
test_that("Function halts if V is not numeric", {
  expect_error(ara_unconstrained_l2(X, V))
})

V <- matrix(rnorm(n * m), nrow = n, ncol = m)
Vlist <- as.list(V)
test_that("Function halts if V is not a matrix", {
  expect_error(ara_unconstrained_l2(X, Vlist))
})

w <- runif(n, 0, 1)
w[1] <- "1"
test_that("Function halts if weights is not numeric", {
  expect_error(ara_unconstrained_l2(X, V, weights = w))
})

test_that("Function halts if solver is not a string", {
  expect_error(ara_unconstrained_l2(X, V, solver = 1))
})




m <- 4
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
test_that("Function halts if the number of columns of V is greater than 3", {
  expect_error(ara_unconstrained_l2(X, V))
})

m <- 0
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
test_that("Function halts if the number of columns of V is zero", {
  expect_error(ara_unconstrained_l2(X, V))
})

m <- 2
V <- matrix(rnorm((n + 1) * m), nrow = n + 1, ncol = m)
test_that("Function halts if the number axis vectors (rows of V) is different
          than the number data variables (rows of X)", {
  expect_error(ara_unconstrained_l2(X, V))
})

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
aux <- X[1, 1]
X[1, 1] <- NA
test_that("Function halts the data (X) has missing values", {
  expect_error(ara_unconstrained_l2(X, V))
})
X[1, 1] <- aux

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[1, 1] <- NA
test_that("Function halts the matrix of axis vectors (V) has missing values", {
  expect_error(ara_unconstrained_l2(X, V))
})

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
w <- runif(n, 0, 1)
w[1] <- NA
test_that("Function halts if the weight vector has missing values", {
  expect_error(ara_unconstrained_l2(X, V, weights = w))
})

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
w <- runif(n, 0, 1)
w[1] <- -1
test_that("Function halts if weight vector contains negative entries", {
  expect_error(ara_unconstrained_l2(X, V, weights = w))
})

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
w <- runif(n - 1, 0, 1)
test_that("Function halts if the length of the weight vector is not equal to the
          number of data variables/axis vectors", {
  expect_error(ara_unconstrained_l2(X, V, weights = w))
})

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
test_that("Function halts if the specified solver is not 'formula' or 'CVXR'", {
  expect_error(ara_unconstrained_l2(X, V, solver = "some invalid solver"))
})



###########################  Test valid projections  ###########################

for (m in 1:3) {
  # Matrix of axis vectors
  V <- matrix(rnorm(n * m), nrow = n, ncol = m)

  R_formula <- ara_unconstrained_l2(X, V)
  R_CVXR <- ara_unconstrained_l2(X, V, solver = "CVXR")

  if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
    test_that("Methods reach same objective value", {
      expect_equal(abs(R_formula$objval - R_CVXR$objval),
        0,
        tolerance = tolerance
      )
    })
  }


  # Weights
  w <- runif(n, 0, 1)

  R_formula <- ara_unconstrained_l2(X, V, weights = w)
  R_CVXR <- ara_unconstrained_l2(X, V, weights = w, solver = "CVXR")

  if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
    test_that("Methods reach same objective value", {
      expect_equal(abs(R_formula$objval - R_CVXR$objval),
        0,
        tolerance = tolerance
      )
    })
  }
}



###################  Test projections for rank deficient V  ####################


#####  m = 2, rank(V) = 1  #####

m <- 2

# Matrix of axis vectors
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[, 2] <- 2 * V[, 1] # linearly dependent columns

R_formula <- ara_unconstrained_l2(X, V)
R_CVXR <- ara_unconstrained_l2(X, V, solver = "CVXR")

if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R_formula$objval - R_CVXR$objval),
      0,
      tolerance = tolerance
    )
  })
}


# Weights
w <- runif(n, 0, 1)

R_formula <- ara_unconstrained_l2(X, V, weights = w)
R_CVXR <- ara_unconstrained_l2(X, V, weights = w, solver = "CVXR")

if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R_formula$objval - R_CVXR$objval),
      0,
      tolerance = tolerance
    )
  })
}



# Matrix of axis vectors
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[, 2] <- 0 * V[, 2] # linearly dependent rows

R_formula <- ara_unconstrained_l2(X, V)
R_CVXR <- ara_unconstrained_l2(X, V, solver = "CVXR")

if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R_formula$objval - R_CVXR$objval),
      0,
      tolerance = tolerance
    )
  })
}


# Weights
w <- runif(n, 0, 1)

R_formula <- ara_unconstrained_l2(X, V, weights = w)
R_CVXR <- ara_unconstrained_l2(X, V, weights = w, solver = "CVXR")

if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R_formula$objval - R_CVXR$objval),
      0,
      tolerance = tolerance
    )
  })
}




#####  m = 3, rank(V) = 2  #####

m <- 3

# Matrix of axis vectors
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[, 3] <- 2 * V[, 1] # linearly dependent columns

R_formula <- ara_unconstrained_l2(X, V)
R_CVXR <- ara_unconstrained_l2(X, V, solver = "CVXR")

if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R_formula$objval - R_CVXR$objval),
      0,
      tolerance = tolerance
    )
  })
}


# Weights
w <- runif(n, 0, 1)

R_formula <- ara_unconstrained_l2(X, V, weights = w)
R_CVXR <- ara_unconstrained_l2(X, V, weights = w, solver = "CVXR")

if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R_formula$objval - R_CVXR$objval),
      0,
      tolerance = tolerance
    )
  })
}



# Matrix of axis vectors
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[, 3] <- 0 * V[, 3]

R_formula <- ara_unconstrained_l2(X, V)
R_CVXR <- ara_unconstrained_l2(X, V, solver = "CVXR")

if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R_formula$objval - R_CVXR$objval),
      0,
      tolerance = tolerance
    )
  })
}


# Weights
w <- runif(n, 0, 1)

R_formula <- ara_unconstrained_l2(X, V, weights = w)
R_CVXR <- ara_unconstrained_l2(X, V, weights = w, solver = "CVXR")

if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R_formula$objval - R_CVXR$objval),
      0,
      tolerance = tolerance
    )
  })
}




#####  m = 3, rank(V) = 1  #####

m <- 3

# Matrix of axis vectors
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[, 2] <- 2 * V[, 1] # linearly dependent columns
V[, 3] <- 3 * V[, 1] # linearly dependent columns

R_formula <- ara_unconstrained_l2(X, V)
R_CVXR <- ara_unconstrained_l2(X, V, solver = "CVXR")

if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R_formula$objval - R_CVXR$objval),
      0,
      tolerance = tolerance
    )
  })
}


# Weights
w <- runif(n, 0, 1)

R_formula <- ara_unconstrained_l2(X, V, weights = w)
R_CVXR <- ara_unconstrained_l2(X, V, weights = w, solver = "CVXR")

if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R_formula$objval - R_CVXR$objval),
      0,
      tolerance = tolerance
    )
  })
}



# Matrix of axis vectors
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[, 2] <- 0 * V[, 2]
V[, 3] <- 0 * V[, 3]

R_formula <- ara_unconstrained_l2(X, V)
R_CVXR <- ara_unconstrained_l2(X, V, solver = "CVXR")

if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R_formula$objval - R_CVXR$objval),
      0,
      tolerance = tolerance
    )
  })
}


# Weights
w <- runif(n, 0, 1)

R_formula <- ara_unconstrained_l2(X, V, weights = w)
R_CVXR <- ara_unconstrained_l2(X, V, weights = w, solver = "CVXR")

if (!any(is.na(R_formula$objval)) && !any(is.na(R_CVXR$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R_formula$objval - R_CVXR$objval),
      0,
      tolerance = tolerance
    )
  })
}
