test_that("check.IglmTerm generates informative error messages with term name", {
  # Create a dummy data_object
  data_obj_undirected <- list(directed = FALSE)
  class(data_obj_undirected) <- "iglm.data"

  data_obj_directed <- list(directed = TRUE)
  class(data_obj_directed) <- "iglm.data"

  # Test directed error
  arglist_mutual <- list(base_name = "mutual", label = "mutual(mode = 'global')")
  expect_error(
    check.IglmTerm(data_obj_undirected, arglist_mutual, directed = TRUE),
    pattern = "Term 'mutual' is only for directed networks."
  )

  # Test directed error fallback to label if base_name is missing
  arglist_mutual_no_base <- list(label = "mutual(mode = 'global')")
  expect_error(
    check.IglmTerm(data_obj_undirected, arglist_mutual_no_base, directed = TRUE),
    pattern = "Term 'mutual' is only for directed networks."
  )

  # Test directed error fallback to caller function name
  # We define a function starting with InitIglmTerm.
  InitIglmTerm.test_term <- function(data_obj, arglist) {
    check.IglmTerm(data_obj, arglist, directed = TRUE)
  }
  expect_error(
    InitIglmTerm.test_term(data_obj_undirected, list()),
    pattern = "Term 'test_term' is only for directed networks."
  )

  # Test mandatory argument error
  expect_error(
    check.IglmTerm(data_obj_directed, arglist_mutual, mandatory = "mode"),
    pattern = "Argument 'mode' is mandatory for term 'mutual'."
  )

  # Test expected argument value error
  arglist_invalid_mode <- list(base_name = "mutual", mode = "invalid_mode")
  expect_error(
    check.IglmTerm(data_obj_directed, arglist_invalid_mode, expected = list(mode = c("global", "local"))),
    pattern = "Argument 'mode' of term 'mutual' must be one of: global, local"
  )

  # Test expected type error (numeric)
  arglist_non_numeric <- list(base_name = "cov_z", z_matrix = "not_numeric")
  expect_error(
    check.IglmTerm(data_obj_directed, arglist_non_numeric, expected = list(z_matrix = "numeric")),
    pattern = "Argument 'z_matrix' of term 'cov_z' must be numeric."
  )

  # Test expected type error (matrix)
  expect_error(
    check.IglmTerm(data_obj_directed, arglist_non_numeric, expected = list(z_matrix = "matrix")),
    pattern = "Argument 'z_matrix' of term 'cov_z' must be a matrix or numeric vector."
  )

  # Test expected NA/NaN error for numeric vector
  arglist_na_numeric <- list(base_name = "cov_z", z_matrix = c(1, NA, 3))
  expect_error(
    check.IglmTerm(data_obj_directed, arglist_na_numeric, expected = list(z_matrix = "numeric")),
    pattern = "Argument 'z_matrix' of term 'cov_z' contains missing \\(NA/NaN\\) values."
  )

  # Test expected type error (scalar_numeric)
  arglist_vector_numeric <- list(base_name = "gwdegree", decay = c(0.1, 0.2))
  expect_error(
    check.IglmTerm(data_obj_directed, arglist_vector_numeric, expected = list(decay = "scalar_numeric")),
    pattern = "Argument 'decay' of term 'gwdegree' must be a single numeric value."
  )

  # Test unexpected argument error on gwdegree
  expect_error(
    InitIglmTerm.gwdegree(data_obj_directed, list(base_name = "gwdegree", x_i = 1)),
    pattern = "Unexpected argument 'x_i' passed to term 'gwdegree'."
  )

  # Test positional arguments normalization (..1, ..2)
  cov_term <- InitIglmTerm.cov_z(data_obj_directed, list(base_name = "cov_z", label = "cov_z(mat)", ..1 = matrix(1, 2, 2)))
  expect_equal(cov_term$term_name, "cov_z_global")

  gwesp_term <- InitIglmTerm.gwesp(data_obj_undirected, list(base_name = "gwesp", label = "gwesp(0.5)", ..1 = 0.5))
  expect_equal(gwesp_term$term_name, "gwesp_global_symm")
  expect_equal(as.numeric(gwesp_term$data), 0.5)

  # Test positional normalization across various terms in full formula context
  edges_term <- InitIglmTerm.edges(data_obj_undirected, list(base_name = "edges", label = "edges('local')", ..1 = "local"))
  expect_equal(edges_term$term_name, "edges_local")

  cov_x_term <- InitIglmTerm.cov_x(data_obj_undirected, list(base_name = "cov_x", label = "cov_x(c(1, 2))", ..1 = c(1, 2)))
  expect_equal(cov_x_term$term_name, "cov_x")
  expect_equal(as.vector(cov_x_term$data), c(1, 2))

  cov_y_term <- InitIglmTerm.cov_y(data_obj_undirected, list(base_name = "cov_y", label = "cov_y(c(3, 4))", ..1 = c(3, 4)))
  expect_equal(cov_y_term$term_name, "cov_y")
  expect_equal(as.vector(cov_y_term$data), c(3, 4))

  attr_xy_term <- InitIglmTerm.attribute_xy(data_obj_undirected, list(base_name = "attribute_xy", label = "attribute_xy('local')", ..1 = "local"))
  expect_equal(attr_xy_term$term_name, "attribute_xy_local")

  gwdsp_term <- InitIglmTerm.gwdsp(data_obj_undirected, list(base_name = "gwdsp", label = "gwdsp(0.25)", ..1 = 0.25))
  expect_equal(gwdsp_term$term_name, "gwdsp_global_symm")
  expect_equal(as.numeric(gwdsp_term$data), 0.25)

  gwdeg_term <- InitIglmTerm.gwdegree(data_obj_undirected, list(base_name = "gwdegree", label = "gwdegree(0.75)", ..1 = 0.75))
  expect_equal(gwdeg_term$term_name, "gwdegree_global")
  expect_equal(as.numeric(gwdeg_term$data), 0.75)

  # Mixed positional and named arguments
  cov_z_mixed <- InitIglmTerm.cov_z(data_obj_undirected, list(base_name = "cov_z", label = "cov_z(mat, mode = 'local')", ..1 = matrix(2, 2, 2), mode = "local"))
  expect_equal(cov_z_mixed$term_name, "cov_z_local")
  expect_equal(cov_z_mixed$data, matrix(2, 2, 2))

  # Test unexpected extra positional argument error
  expect_error(
    InitIglmTerm.gwdegree(data_obj_directed, list(base_name = "gwdegree", ..1 = 0.5, ..2 = "local", ..3 = "extra")),
    pattern = "Unexpected argument '..3' passed to term 'gwdegree'."
  )

  # Test no-argument terms reject excess arguments
  expect_error(
    InitIglmTerm.attribute_x(data_obj_undirected, list(base_name = "attribute_x", ..1 = "extra")),
    pattern = "Unexpected argument '..1' passed to term 'attribute_x'."
  )
  expect_error(
    InitIglmTerm.attribute_y(data_obj_undirected, list(base_name = "attribute_y", foo = 1)),
    pattern = "Unexpected argument 'foo' passed to term 'attribute_y'."
  )
  expect_error(
    InitIglmTerm.transitive(data_obj_undirected, list(base_name = "transitive", ..1 = "extra")),
    pattern = "Unexpected argument '..1' passed to term 'transitive'."
  )
  expect_error(
    InitIglmTerm.nonisolates(data_obj_undirected, list(base_name = "nonisolates", ..1 = "extra")),
    pattern = "Unexpected argument '..1' passed to term 'nonisolates'."
  )
  expect_error(
    InitIglmTerm.isolates(data_obj_undirected, list(base_name = "isolates", ..1 = "extra")),
    pattern = "Unexpected argument '..1' passed to term 'isolates'."
  )
})

test_that("Positional formula arguments evaluate correctly in model estimation and simulation", {
  n_units <- 10
  neighborhood <- matrix(1, nrow = n_units, ncol = n_units)

  x_val <- rep(c(1, 0), length.out = n_units)
  y_val <- rep(c(0, 1), length.out = n_units)
  z_net <- matrix(c(1, 2, 2, 3, 3, 4), ncol = 2, byrow = TRUE)
  cov_mat <- matrix(0.5, nrow = n_units, ncol = n_units)

  d <- iglm.data(
    x_attribute = x_val,
    y_attribute = y_val,
    z_network = z_net,
    neighborhood = neighborhood,
    n_units = n_units,
    directed = FALSE
  )

  # Formula with unnamed positional arguments: edges("local"), cov_z(cov_mat), gwesp(0.5)
  mod <- iglm(
    formula = d ~ edges("local") + cov_z(cov_mat) + gwesp(0.5),
    coef = c(-1, 0.2, 0.1),
    sampler = sampler.iglm(n_burn_in = 2, n_simulation = 2)
  )

  expect_equal(names(mod$sufficient_statistics), c("edges('local')", "cov_z(cov_mat)", "gwesp(0.5)"))
  expect_no_error(mod$simulate(display_progress = FALSE))
})

