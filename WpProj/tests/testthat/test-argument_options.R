test_that("transport_options returns the correct methods", {
  expected_methods <- c("exact", "sinkhorn", "greenkhorn", "hilbert", "rank", "univariate.approximation.pwr")
  expect_equal(transport_options(), expected_methods)
})


test_that("L1_penalty_options returns the correct penalties", {
  expected_penalties <- c("lasso", "ols", "mcp", "elastic.net", 
                          "scad", "mcp.net", "scad.net", "grp.lasso", 
                          "grp.lasso.net", "grp.mcp", "grp.scad", 
                          "grp.mcp.net", "grp.scad.net", "sparse.grp.lasso")
  expect_equal(L1_penalty_options(), expected_penalties)
})

# Test for default behavior
test_that("L1_method_options with default arguments", {
  result <- L1_method_options()
  expect_true(is.list(result))
  # Check if default values are set correctly
  expect_length(result$penalty, 1L)
  expect_length(result$lambda, 0)
  expect_equal(result$nlambda, 500L)
  expect_equal(result$lambda.min.ratio, 1e-4)
  expect_equal(result$gamma, 1)
  expect_equal(result$maxit, 500L)
  expect_null(result$model.size)
  expect_equal(result$tol, 1e-7)
  expect_false(result$display.progress)
  expect_null(result$solver.options)
})

# Test for individual arguments
test_that("L1_method_options with different penalties", {
  penalties <- L1_penalty_options()
  for (penalty in penalties) {
    result <- L1_method_options(penalty = penalty)
    expect_true(is.list(result))
    expect_equal(result$penalty, penalty)
  }
})

# Test for lambda and nlambda
test_that("L1_method_options with lambda and nlambda", {
  result <- L1_method_options(lambda = c(0.1, 0.2), nlambda = 10L)
  expect_equal(result$lambda, c(0.1, 0.2))
  expect_equal(result$nlambda, 10L)
})

# Test for lambda.min.ratio, gamma, and maxit
test_that("L1_method_options with lambda.min.ratio, gamma, and maxit", {
  result <- L1_method_options(lambda.min.ratio = 0.001, gamma = 2, maxit = 1000L)
  expect_equal(result$lambda.min.ratio, 0.001)
  expect_equal(result$gamma, 2)
  expect_equal(result$maxit, 1000L)
})

# Test for model.size and tol
test_that("L1_method_options with model.size and tol", {
  result <- L1_method_options(model.size = 10L, tol = 1e-6)
  expect_equal(result$model.size, 10L)
  expect_equal(result$tol, 1e-6)
})

# Test for display.progress
test_that("L1_method_options with display.progress", {
  result <- L1_method_options(display.progress = TRUE)
  expect_true(result$display.progress)
})

# Test for solver.options
test_that("L1_method_options with solver.options", {
  custom_options <- list(option1 = "value1", option2 = "value2")
  result <- L1_method_options(solver.options = custom_options)
  expect_equal(result[c("option1", "option2")], custom_options)
})

# Test for error handling
test_that("L1_method_options with invalid inputs", {
  expect_error(L1_method_options(nlambda = -1))
  expect_error(L1_method_options(lambda.min.ratio = -0.5))
  expect_error(L1_method_options(gamma = -1))
  expect_error(L1_method_options(maxit = -100L))
  expect_error(L1_method_options(model.size = -5L))
  expect_error(L1_method_options(tol = -1e-6))
})

# Test for default behavior
test_that("binary_program_method_options with default arguments", {
  result <- binary_program_method_options(solver.options = list())
  expect_true(is.list(result))
  expect_equal(result$maxit, 500L)
  expect_equal(result$infimum.maxit, 100L)
  expect_equal(result$transport.method, transport_options()[1])
  expect_equal(result$epsilon, 0.05)
  expect_equal(result$OTmaxit, 100L)
  expect_null(result$model.size)
  expect_equal(result$tol, 1e-7)
  expect_false(result$display.progress)
  expect_null(result$parallel)
})

# Test for error handling for each argument
test_that("binary_program_method_options with invalid inputs", {
  expect_error(binary_program_method_options(maxit = -1L, solver.options = list()))
  expect_error(binary_program_method_options(infimum.maxit = -1L, solver.options = list()))
  expect_error(binary_program_method_options(epsilon = -0.01, solver.options = list()))
  expect_error(binary_program_method_options(OTmaxit = -1L, solver.options = list()))
  expect_error(binary_program_method_options(model.size = -5L, solver.options = list()))
  expect_error(binary_program_method_options(tol = -1e-6, solver.options = list()))
})


# Test for transport.method
test_that("binary_program_method_options with different transport methods", {
  valid_methods <- transport_options()
  for (method in valid_methods) {
    result <- binary_program_method_options(transport.method = method, solver.options = list())
    expect_equal(result$transport.method, method)
  }
  # Test for an invalid transport method
  expect_error(binary_program_method_options(transport.method = "invalid_method", solver.options = list()))
})

# Test for parallel
test_that("binary_program_method_options with parallel options", {
  # Assuming parallel can be either NULL, TRUE, or FALSE (adjust as per your function's implementation)
  result <- binary_program_method_options(parallel = NULL, solver.options = list())
  expect_null(result$parallel)
  result <- binary_program_method_options(parallel = 5L, solver.options = list())
  expect_equal(result$parallel, 5L)
  # Test for an invalid parallel setting
  expect_error(binary_program_method_options(parallel = "invalid_setting", solver.options = list()))
})

# Test for solver.options
test_that("binary_program_method_options with solver options", {
  custom_options <- list(option1 = "value1", option2 = "value2")
  result <- binary_program_method_options(solver.options = custom_options)
  expect_equal(result[c("option1","option2")], custom_options)
  # Test for missing solver.options
  expect_silent(binary_program_method_options())
})

# Test for default behavior
test_that("stepwise_method_options with default arguments", {
  result <- stepwise_method_options()
  expect_true(is.list(result))
  expect_null(result$force)
  expect_equal(result$direction, c("backward"))
  expect_equal(result$method, c("selection.variable"))
  expect_equal(result$transport.method, transport_options()[1])
  expect_equal(result$OTmaxit, 100)
  expect_equal(result$epsilon, 0.05)
  expect_null(result$model.size)
  expect_false(result$display.progress)
  expect_null(result$parallel)
  expect_true(result$calc.theta)
})


# Test for error handling for each argument
test_that("stepwise_method_options with invalid inputs", {
  expect_error(stepwise_method_options(force = "invalid"))
  expect_error(stepwise_method_options(direction = "invalid_direction"))
  expect_error(stepwise_method_options(method = "invalid_method"))
  expect_error(stepwise_method_options(transport.method = "invalid_transport"))
  expect_error(stepwise_method_options(OTmaxit = -1))
  expect_error(stepwise_method_options(epsilon = -0.01))
  expect_error(stepwise_method_options(model.size = -5))
  # ... (other error checks)
})

# Test for default behavior
test_that("stepwise_method_options with default arguments", {
  result <- stepwise_method_options()
  expect_true(is.list(result))
  # Add checks for default values of each argument
})

# Tests for the 'force' argument
test_that("stepwise_method_options with force argument", {
  expect_true(is.list(stepwise_method_options(force = NULL)))
  expect_true(is.list(stepwise_method_options(force = c(1, 2))))
  expect_error(stepwise_method_options(force = "invalid"))
  expect_error(stepwise_method_options(force = -1))
  expect_error(stepwise_method_options(force = 1.1))
})

# Tests for the 'direction' argument
test_that("stepwise_method_options with direction argument", {
  expect_true(is.list(stepwise_method_options(direction = "backward")))
  expect_true(is.list(stepwise_method_options(direction = "forward")))
  expect_error(stepwise_method_options(direction = "invalid_direction"))
})

# Tests for the 'method' argument
test_that("stepwise_method_options with method argument", {
  expect_true(is.list(stepwise_method_options(method = "binary program")))
  expect_true(is.list(stepwise_method_options(method = "projection")))
  expect_error(stepwise_method_options(method = "invalid_method"))
})

# Tests for the 'transport.method' argument
test_that("stepwise_method_options with transport.method argument", {
  valid_methods <- transport_options()
  for (method in valid_methods) {
    expect_true(is.list(stepwise_method_options(transport.method = method)))
  }
  expect_error(stepwise_method_options(transport.method = "invalid_transport"))
})

# Tests for the 'OTmaxit' argument
test_that("stepwise_method_options with OTmaxit argument", {
  expect_true(is.list(stepwise_method_options(OTmaxit = 100)))
  expect_error(stepwise_method_options(OTmaxit = -1))
  expect_error(stepwise_method_options(OTmaxit = "invalid"))
})

# Tests for the 'epsilon' argument
test_that("stepwise_method_options with epsilon argument", {
  expect_true(is.list(stepwise_method_options(epsilon = 0.05)))
  expect_error(stepwise_method_options(epsilon = -0.01))
  expect_error(stepwise_method_options(epsilon = "invalid"))
})

# Tests for the 'model.size' argument
test_that("stepwise_method_options with model.size argument", {
  expect_true(is.list(stepwise_method_options(model.size = NULL)))
  expect_true(is.list(stepwise_method_options(model.size = 10)))
  expect_error(stepwise_method_options(model.size = -1))
  expect_error(stepwise_method_options(model.size = "invalid"))
})

# Tests for the 'display.progress' argument
test_that("stepwise_method_options with display.progress argument", {
  expect_true(is.list(stepwise_method_options(display.progress = TRUE)))
  expect_true(is.list(stepwise_method_options(display.progress = FALSE)))
  expect_error(stepwise_method_options(display.progress = "invalid"))
})

# Tests for the 'parallel' argument
test_that("stepwise_method_options with parallel argument", {
  # Assuming parallel can be TRUE, FALSE, or NULL
  expect_error(is.list(stepwise_method_options(parallel = TRUE)))
  expect_error(is.list(stepwise_method_options(parallel = FALSE)))
  expect_true(is.list(stepwise_method_options(parallel = NULL)))
  expect_true(is.list(stepwise_method_options(parallel = 5L)))
  expect_error(stepwise_method_options(parallel = "invalid"))
})

# Tests for the 'calc.theta' argument
test_that("stepwise_method_options with calc.theta argument", {
  expect_true(is.list(stepwise_method_options(calc.theta = TRUE)))
  expect_true(is.list(stepwise_method_options(calc.theta = FALSE)))
  expect_error(stepwise_method_options(calc.theta = "invalid"))
})

# Test for default behavior
test_that("simulated_annealing_method_options with default arguments", {
  result <- simulated_annealing_method_options()
  expect_true(is.list(result))
  # Add checks for default values of each argument
})

# Test for error handling for each argument
test_that("simulated_annealing_method_options with invalid inputs", {
  expect_error(simulated_annealing_method_options(force = "invalid"))
  expect_error(simulated_annealing_method_options(method = "invalid_method"))
  expect_error(simulated_annealing_method_options(transport.method = "invalid_transport"))
  expect_error(simulated_annealing_method_options(OTmaxit = -1))
  expect_error(simulated_annealing_method_options(epsilon = -0.01))
  expect_error(simulated_annealing_method_options(maxit = -1))
  expect_error(simulated_annealing_method_options(temps = -1))
  expect_error(simulated_annealing_method_options(max.time = -1))
  expect_error(simulated_annealing_method_options(proposal.method = "invalid_proposal"))
  expect_error(simulated_annealing_method_options(energy.distribution = "invalid_distribution"))
  expect_error(simulated_annealing_method_options(cooling.schedule = "invalid_schedule"))
  expect_error(simulated_annealing_method_options(model.size = -1))
  expect_error(simulated_annealing_method_options(display.progress = "invalid"))
  expect_error(simulated_annealing_method_options(parallel = "invalid"))
  expect_error(simulated_annealing_method_options(calc.theta = "invalid"))
})

# Test for default behavior
test_that("simulated_annealing_method_options with default arguments", {
  result <- simulated_annealing_method_options()
  expect_true(is.list(result))
  # Add checks for default values of each argument
})

# Tests for the 'force' argument
test_that("simulated_annealing_method_options with force argument", {
  expect_true(is.list(simulated_annealing_method_options(force = NULL)))
  expect_true(is.list(simulated_annealing_method_options(force = c(1, 2))))
  expect_error(simulated_annealing_method_options(force = "invalid"))
  expect_error(simulated_annealing_method_options(force = -1))
})

# Tests for the 'method' argument
test_that("simulated_annealing_method_options with method argument", {
  expect_true(is.list(simulated_annealing_method_options(method = "binary program")))
  expect_true(is.list(simulated_annealing_method_options(method = "projection")))
  expect_error(simulated_annealing_method_options(method = "invalid_method"))
})

# Tests for the 'transport.method' argument
test_that("simulated_annealing_method_options with transport.method argument", {
  valid_methods <- transport_options()
  for (method in valid_methods) {
    expect_true(is.list(simulated_annealing_method_options(transport.method = method)))
  }
  expect_error(simulated_annealing_method_options(transport.method = "invalid_transport"))
})

# Tests for the 'OTmaxit' argument
test_that("simulated_annealing_method_options with OTmaxit argument", {
  expect_true(is.list(simulated_annealing_method_options(OTmaxit = 100L)))
  expect_error(simulated_annealing_method_options(OTmaxit = -1))
})

# Tests for the 'epsilon' argument
test_that("simulated_annealing_method_options with epsilon argument", {
  expect_true(is.list(simulated_annealing_method_options(epsilon = 0.05)))
  expect_error(simulated_annealing_method_options(epsilon = -0.01))
})

# Tests for the 'maxit' argument
test_that("simulated_annealing_method_options with maxit argument", {
  expect_true(is.list(simulated_annealing_method_options(maxit = 1L)))
  expect_error(simulated_annealing_method_options(maxit = -1L))
})

# Tests for the 'temps' argument
test_that("simulated_annealing_method_options with temps argument", {
  expect_true(is.list(simulated_annealing_method_options(temps = 1000L)))
  expect_error(simulated_annealing_method_options(temps = -1L))
})

# Tests for the 'max.time' argument
test_that("simulated_annealing_method_options with max.time argument", {
  expect_true(is.list(simulated_annealing_method_options(max.time = 3600)))
  expect_error(simulated_annealing_method_options(max.time = -1))
})

# Tests for the 'proposal.method' argument
test_that("simulated_annealing_method_options with proposal.method argument", {
  expect_true(is.list(simulated_annealing_method_options(proposal.method = "covariance")))
  expect_true(is.list(simulated_annealing_method_options(proposal.method = "uniform")))
  expect_error(simulated_annealing_method_options(proposal.method = "invalid_proposal"))
})

# Tests for the 'energy.distribution' argument
test_that("simulated_annealing_method_options with energy.distribution argument", {
  expect_true(is.list(simulated_annealing_method_options(energy.distribution = "boltzman")))
  expect_true(is.list(simulated_annealing_method_options(energy.distribution = "bose-einstein")))
  expect_error(simulated_annealing_method_options(energy.distribution = "invalid_distribution"))
})

# Tests for the 'cooling.schedule' argument
test_that("simulated_annealing_method_options with cooling.schedule argument", {
  expect_true(is.list(simulated_annealing_method_options(cooling.schedule = "Geman-Geman")))
  expect_true(is.list(simulated_annealing_method_options(cooling.schedule = "exponential")))
  expect_error(simulated_annealing_method_options(cooling.schedule = "invalid_schedule"))
})

# Tests for the 'model.size' argument
test_that("simulated_annealing_method_options with model.size argument", {
  expect_true(is.list(simulated_annealing_method_options(model.size = NULL)))
  expect_true(is.list(simulated_annealing_method_options(model.size = 10)))
  expect_error(simulated_annealing_method_options(model.size = -1))
})

# Tests for the 'display.progress' argument
test_that("simulated_annealing_method_options with display.progress argument", {
  expect_true(is.list(simulated_annealing_method_options(display.progress = TRUE)))
  expect_true(is.list(simulated_annealing_method_options(display.progress = FALSE)))
  expect_error(simulated_annealing_method_options(display.progress = "invalid"))
})

# Tests for the 'parallel' argument
test_that("simulated_annealing_method_options with parallel argument", {
  # Assuming parallel can be cluster, integer, or NULL
  expect_error(is.list(simulated_annealing_method_options(parallel = TRUE)))
  expect_error(is.list(simulated_annealing_method_options(parallel = FALSE)))
  expect_true(is.list(simulated_annealing_method_options(parallel = NULL)))
  expect_true(is.list(simulated_annealing_method_options(parallel = 5L)))
  cl <- parallel::makeCluster(1)
  expect_true(is.list(simulated_annealing_method_options(parallel = cl)))
  parallel::stopCluster(cl)
  expect_error(simulated_annealing_method_options(parallel = "invalid"))
})

# Tests for the 'calc.theta' argument
test_that("simulated_annealing_method_options with calc.theta argument", {
  expect_true(is.list(simulated_annealing_method_options(calc.theta = TRUE)))
  expect_true(is.list(simulated_annealing_method_options(calc.theta = FALSE)))
  expect_error(simulated_annealing_method_options(calc.theta = "invalid"))
})


# Edge case tests for the 'force' argument
test_that("edge cases for force argument in simulated_annealing_method_options", {
  # Test with an empty vector
  expect_true(is.list(simulated_annealing_method_options(force = integer(0))))
  # Test with a large vector
  expect_true(is.list(simulated_annealing_method_options(force = 1:1000)))
})

# Edge case tests for the 'method' argument
test_that("edge cases for method argument in simulated_annealing_method_options", {
  # Test with multiple methods specified
  expect_true(is.list(simulated_annealing_method_options(method = c("binary program", "projection"))))
})

# Edge case tests for the 'OTmaxit' argument
test_that("edge cases for OTmaxit argument in simulated_annealing_method_options", {
  # Test with zero (if valid)
  expect_error(is.list(simulated_annealing_method_options(OTmaxit = 0L)))
  # Test with a very large number
  expect_true(is.list(simulated_annealing_method_options(OTmaxit = 100000L)))
})

# Edge case tests for the 'epsilon' argument
test_that("edge cases for epsilon argument in simulated_annealing_method_options", {
  # Test with a very small positive number
  expect_true(is.list(simulated_annealing_method_options(epsilon = .Machine$double.eps)))
  # Test with a larger number
  expect_true(is.list(simulated_annealing_method_options(epsilon = 1)))
})

# Edge case tests for the 'temps' argument
test_that("edge cases for temps argument in simulated_annealing_method_options", {
  # Test with zero (if valid)
  expect_error(is.list(simulated_annealing_method_options(temps = 0L)))
  # Test with a very large number
  expect_true(is.list(simulated_annealing_method_options(temps = 100000L)))
})

# Edge case tests for the 'max.time' argument
test_that("edge cases for max.time argument in simulated_annealing_method_options", {
  # Test with zero (if valid)
  expect_error(is.list(simulated_annealing_method_options(max.time = 0)))
  # Test with a very large number
  expect_true(is.list(simulated_annealing_method_options(max.time = 100000)))
})

# Edge case tests for the 'model.size' argument
test_that("edge cases for model.size argument in simulated_annealing_method_options", {
  # Test with zero (if valid)
  expect_error(is.list(simulated_annealing_method_options(model.size = 0)))
  # Test with a very large number
  expect_true(is.list(simulated_annealing_method_options(model.size = 10000)))
})


# Test for default arguments
test_that("default arguments work correctly", {
  result <- L0_method_options()
  expect_equal(result$method, "selection.variable")
  expect_is(result$transport.method, "character") # or appropriate type
  expect_equal(result$epsilon, 0.05)
  expect_equal(result$OTmaxit, 100L)
  expect_null(result$parallel)
})

# Test for argument validation
test_that("invalid inputs are handled correctly", {
  # Testing epsilon
  expect_error(L0_method_options(epsilon = -1))
  expect_error(L0_method_options(epsilon = "not a number"))
  
  # Testing OTmaxit
  expect_error(L0_method_options(OTmaxit = -1))
  expect_error(L0_method_options(OTmaxit = "not a number"))
  
  # Testing parallel
  expect_error(L0_method_options(parallel = "not a number or cluster"))
  expect_error(L0_method_options(parallel = -1))
})

# Test for correct output structure
test_that("output structure is correct", {
  result <- L0_method_options()
  expect_true(is.list(result))
  expect_true(all(names(result) == c("method", "transport.method", "epsilon", "OTmaxit", "parallel")))
  expect_type(result$method, "character")
  expect_type(result$transport.method, "character") # or appropriate type
  expect_type(result$epsilon, "double")
  expect_type(result$OTmaxit, "integer")
  
  
  expect_error(is.list(L0_method_options(parallel = TRUE)))
  expect_error(is.list(L0_method_options(parallel = FALSE)))
  expect_true(is.list(L0_method_options(parallel = NULL)))
  expect_true(is.list(L0_method_options(parallel = 5L)))
  cl <- parallel::makeCluster(1)
  expect_true(is.list(L0_method_options(parallel = cl)))
  parallel::stopCluster(cl)
  expect_error(L0_method_options(parallel = "invalid"))
  
})

# Additional tests for specific method input
test_that("method input is handled correctly", {
  result_binary <- L0_method_options(method = "binary program")
  expect_equal(result_binary$method, "selection.variable")
  
  result_projection <- L0_method_options(method = "projection")
  expect_equal(result_projection$method, "projection")
  
})

