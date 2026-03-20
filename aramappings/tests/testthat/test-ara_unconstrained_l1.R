tolerance <- 0.002

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
  expect_error(ara_unconstrained_l1(Xcopy, V))
})

Xlist <- as.list(X)
test_that("Function halts if X is not a matrix", {
  expect_error(ara_unconstrained_l1(Xlist, V))
})

V[1, 1] <- "a"
test_that("Function halts if V is not numeric", {
  expect_error(ara_unconstrained_l1(X, V))
})

V <- matrix(rnorm(n * m), nrow = n, ncol = m)
Vlist <- as.list(V)
test_that("Function halts if V is not a matrix", {
  expect_error(ara_unconstrained_l1(X, Vlist))
})

w <- runif(n, 0, 1)
w[1] <- "1"
test_that("Function halts if weights is not numeric", {
  expect_error(ara_unconstrained_l1(X, V, weights = w))
})

test_that("Function halts if solver is not a string", {
  expect_error(ara_unconstrained_l1(X, V, solver = 1))
})

test_that("Function halts if use_glpkAPI_simplex is not logical (Boolean)", {
  expect_error(ara_unconstrained_l1(X, V, use_glpkAPI_simplex = 0))
})

test_that("Function halts if use_glpkAPI_simplex is not logical (Boolean)", {
  expect_error(ara_unconstrained_l1(X, V, use_glpkAPI_simplex = 1))
})

test_that("Function halts if cluster does not inheret from classes 'SOCKcluster'
          or 'cluster'", {
  expect_error(ara_unconstrained_l1(X, V, cluster = 1))
})



m <- 4
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
test_that("Function halts if the number of columns of V is greater than 3", {
  expect_error(ara_unconstrained_l1(X, V))
})

m <- 0
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
test_that("Function halts if the number of columns of V is zero", {
  expect_error(ara_unconstrained_l1(X, V))
})

m <- 2
V <- matrix(rnorm((n + 1) * m), nrow = n + 1, ncol = m)
test_that("Function halts if the number axis vectors (rows of V) is different
          than the number data variables (rows of X)", {
  expect_error(ara_unconstrained_l1(X, V))
})

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
aux <- X[1, 1]
X[1, 1] <- NA
test_that("Function halts if the data (X) has missing values", {
  expect_error(ara_unconstrained_l1(X, V))
})
X[1, 1] <- aux

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[1, 1] <- NA
test_that("Function halts if the matrix of axis vectors (V) has missing values", {
  expect_error(ara_unconstrained_l1(X, V))
})

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
w <- runif(n, 0, 1)
w[1] <- NA
test_that("Function halts if the weight vector has missing values", {
  expect_error(ara_unconstrained_l1(X, V, weights = w))
})

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
w <- runif(n, 0, 1)
w[1] <- -1
test_that("Function halts if the weight vector contains negative entries", {
  expect_error(ara_unconstrained_l1(X, V, weights = w))
})

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
w <- runif(n - 1, 0, 1)
test_that("Function halts if the length of the weight vector is not equal to
          the number of data variables/axis vectors", {
  expect_error(ara_unconstrained_l1(X, V, weights = w))
})

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
test_that("Function halts if the specified solver is not 'clarabel', 'glpkAPI',
          'Rglpk'', or 'CVXR'", {
  expect_error(ara_unconstrained_l1(X,
    V,
    solver = "some invalid solver"
  ))
})

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
test_that("Function halts if the flag use_glpkAPI_simplex is not Boolean ", {
  expect_error(ara_unconstrained_l1(X, V, use_glpkAPI_simplex = 3))
})

m <- 2
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
test_that("Function halts if the argument cluster for parallel processing is
          invalid", {
  expect_error(ara_unconstrained_l1(X, V, cluster = 3))
})



###########################  Test valid projections  ###########################

for (m in 1:3) {
  # Matrix of axis vectors
  V <- matrix(rnorm(n * m), nrow = n, ncol = m)

  # Weights
  w <- runif(n, 0, 1)

  # Correct result
  R <- ara_unconstrained_l1(X, V, weights = w, solver = "clarabel")

  R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "CVXR")
  if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
    test_that("Methods reach same objective value", {
      expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
    })
  }


  R_test <- ara_unconstrained_l1(X, V,
    weights = w, solver = "glpkAPI",
    use_glpkAPI_simplex = TRUE
  )
  if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
    test_that("Methods reach same objective value", {
      expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
    })
  }

  R_test <- ara_unconstrained_l1(X, V,
    weights = w, solver = "glpkAPI",
    use_glpkAPI_simplex = FALSE
  )
  if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
    test_that("Methods reach same objective value", {
      expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
    })
  }

  R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "Rglpk")
  if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
    test_that("Methods reach same objective value", {
      expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
    })
  }
}


# Set the number of CPU cores/workers
# NCORES <- parallelly::availableCores(omit = 1)
# NCORES <- max(1,parallel::detectCores() - 1)
NCORES <- 2L

if (exists("cl")) {
  parallel::stopCluster(cl)
  rm(cl)
}
cl <- parallel::makeCluster(NCORES)
on.exit(parallel::stopCluster(cl))


for (m in 1:3) {
  # Matrix of axis vectors
  V <- matrix(rnorm(n * m), nrow = n, ncol = m)

  # Weights
  w <- runif(n, 0, 1)

  # Correct result
  R <- ara_unconstrained_l1(X, V, weights = w, solver = "clarabel")

  R_test <- ara_unconstrained_l1(X, V,
    weights = w, solver = "clarabel",
    cluster = cl
  )
  if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
    test_that("Methods reach same objective value", {
      expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
    })
  }


  R_test <- ara_unconstrained_l1(X, V,
    weights = w, solver = "glpkAPI",
    use_glpkAPI_simplex = TRUE, cluster = cl
  )
  if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
    test_that("Methods reach same objective value", {
      expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
    })
  }


  R_test <- ara_unconstrained_l1(X, V,
    weights = w, solver = "glpkAPI",
    use_glpkAPI_simplex = FALSE, cluster = cl
  )
  if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
    test_that("Methods reach same objective value", {
      expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
    })
  }


  R_test <- ara_unconstrained_l1(X, V,
    weights = w, solver = "Rglpk",
    cluster = cl
  )
  if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
    test_that("Methods reach same objective value", {
      expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
    })
  }
}






###################  Test projections for rank deficient V  ####################

#####  m = 2, rank(V) = 1  #####

m <- 2

# Matrix of axis vectors
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[, 2] <- 0 * V[, 1] # linearly dependent columns

# Weights
w <- runif(n, 0, 1)

# Correct result
R <- ara_unconstrained_l1(X, V, weights = w, solver = "clarabel")

R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "CVXR")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  weights = w, solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  weights = w, solver = "glpkAPI",
  use_glpkAPI_simplex = FALSE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "Rglpk")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}



# Correct result - no weights
R <- ara_unconstrained_l1(X, V, solver = "clarabel")

R_test <- ara_unconstrained_l1(X, V, solver = "CVXR")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  solver = "glpkAPI",
  use_glpkAPI_simplex = FALSE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V, solver = "Rglpk")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}






# Matrix of axis vectors
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[, 2] <- 2 * V[, 1] # linearly dependent columns

# Weights
w <- runif(n, 0, 1)

# Correct result
R <- ara_unconstrained_l1(X, V, weights = w, solver = "clarabel")

R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "CVXR")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  weights = w, solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  weights = w, solver = "glpkAPI",
  use_glpkAPI_simplex = FALSE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "Rglpk")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}



# Correct result - no weights
R <- ara_unconstrained_l1(X, V, solver = "clarabel")

R_test <- ara_unconstrained_l1(X, V, solver = "CVXR")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  solver = "glpkAPI",
  use_glpkAPI_simplex = FALSE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V, solver = "Rglpk")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}



######  m = 3, rank(V) = 2  #####

m <- 3

# Matrix of axis vectors
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[, 3] <- 2 * V[, 1] # linearly dependent columns

# Weights
w <- runif(n, 0, 1)

# Correct result
R <- ara_unconstrained_l1(X, V, weights = w, solver = "clarabel")

R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "CVXR")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  weights = w, solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  weights = w, solver = "glpkAPI",
  use_glpkAPI_simplex = FALSE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "Rglpk")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}



# Correct result - no weights
R <- ara_unconstrained_l1(X, V, solver = "clarabel")

R_test <- ara_unconstrained_l1(X, V, solver = "CVXR")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  solver = "glpkAPI",
  use_glpkAPI_simplex = FALSE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V, solver = "Rglpk")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}




# Matrix of axis vectors
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[, 3] <- 0 * V[, 3]

# Weights
w <- runif(n, 0, 1)

# Correct result
R <- ara_unconstrained_l1(X, V, weights = w, solver = "clarabel")

R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "CVXR")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  weights = w, solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  weights = w, solver = "glpkAPI",
  use_glpkAPI_simplex = FALSE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "Rglpk")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}



# Correct result - no weights
R <- ara_unconstrained_l1(X, V, solver = "clarabel")

R_test <- ara_unconstrained_l1(X, V, solver = "CVXR")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  solver = "glpkAPI",
  use_glpkAPI_simplex = FALSE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V, solver = "Rglpk")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}



######  m = 3, rank(V) = 1  #####

m <- 3

# Matrix of axis vectors
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[, 2] <- 2 * V[, 1] # linearly dependent columns
V[, 3] <- 3 * V[, 1] # linearly dependent columns

# Weights
w <- runif(n, 0, 1)

# Correct result
R <- ara_unconstrained_l1(X, V, weights = w, solver = "clarabel")

R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "CVXR")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  weights = w, solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  weights = w, solver = "glpkAPI",
  use_glpkAPI_simplex = FALSE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "Rglpk")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}


# Correct result - no weights
R <- ara_unconstrained_l1(X, V, solver = "clarabel")

R_test <- ara_unconstrained_l1(X, V, solver = "CVXR")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  solver = "glpkAPI",
  use_glpkAPI_simplex = FALSE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V, solver = "Rglpk")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}




# Matrix of axis vectors
V <- matrix(rnorm(n * m), nrow = n, ncol = m)
V[, 2] <- 0 * V[, 1] # linearly dependent columns
V[, 3] <- 0 * V[, 1] # linearly dependent columns

# Weights
w <- runif(n, 0, 1)

# Correct result
R <- ara_unconstrained_l1(X, V, weights = w, solver = "clarabel")

R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "CVXR")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  weights = w, solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  weights = w, solver = "glpkAPI",
  use_glpkAPI_simplex = FALSE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V, weights = w, solver = "Rglpk")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}


# Correct result - no weights
R <- ara_unconstrained_l1(X, V, solver = "clarabel")

R_test <- ara_unconstrained_l1(X, V, solver = "CVXR")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V,
  solver = "glpkAPI",
  use_glpkAPI_simplex = FALSE
)
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}

R_test <- ara_unconstrained_l1(X, V, solver = "Rglpk")
if (!any(is.na(R$objval)) && !any(is.na(R_test$objval))) {
  test_that("Methods reach same objective value", {
    expect_equal(abs(R$objval - R_test$objval), 0, tolerance = tolerance)
  })
}
