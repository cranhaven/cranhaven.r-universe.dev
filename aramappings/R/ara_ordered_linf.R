#' Ordered Adaptable Radial Axes (ARA) mappings using the L-infinity norm
#'
#' @description
#' \code{ara_ordered_linf()} computes \strong{ordered} \strong{Adaptable Radial
#' Axes} (ARA) mappings for the \strong{Linf norm}
#'
#' @details
#' \code{ara_ordered_linf()} computes low-dimensional point representations of
#' high-dimensional numerical data (\code{X}) according to the data
#' visualization method "Adaptable Radial Axes" (M. Rubio-Sánchez, A. Sanchez,
#' and D. J. Lehmann (2017), doi: 10.1111/cgf.13196), which
#' describes a collection of convex norm optimization problems aimed at
#' minimizing estimates of original values in \code{X} through dot products of
#' the mapped points with the axis vectors (rows of \code{V}). This particular
#' function solves the constrained optimization problem in Eq. (14), for the
#' L-infinity norm. The inequality constraint ensures that the estimates for a
#' selected variable are ordered in accordance with its original values. In
#' other words, ignoring any ties, the estimate for the data observation with
#' the i-th smallest value will correspond to the i-th smallest estimate.
#'
#' @inheritParams ara_unconstrained_l1
#' @param variable
#' Integer that indicates the variable (in \[1,n\]) for which the estimates of
#' high-dimensional data will be exact. Default: variable = 1.
#'
#' @returns
#' A list with the three following entries:
#' \describe{
#'   \item{\code{P}}{A numeric N x m matrix containing the mapped points. Each
#'   row is the low-dimensional representation of a data observation in X.}
#'   \item{\code{status}}{A vector of length N where the i-th element contains
#'   the status of the chosen solver when calculating the mapping of the i-th
#'   data observation. The type of the elements depends on the particular chosen
#'   solver.}
#'   \item{\code{objval}}{The numeric objective value associated with the
#'   solution to the optimization problem, considering matrix norms.}
#' }
#' If the chosen solver fails to map the data (i.e., fails to solve the related
#' optimization problem), \code{P} will contain \code{NA} (not available)
#' values. In that case, \code{objval} will also be \code{NA}.
#'
#' @inherit ara_unconstrained_l1 references
#'
#' @export
#'
#' @examples
#' # Define subset of (numerical) variables
#' # 1:"mpg", 4:"horsepower", 5:"weight", 6:"acceleration"
#' selected_variables <- c(1, 4, 5, 6)
#' n <- length(selected_variables)
#'
#' # Retain only selected variables and rename dataset as X
#' X <- auto_mpg[, selected_variables] # Select a subset of variables
#'
#' # Remove rows with missing values from X
#' N <- nrow(X)
#' rows_to_delete <- NULL
#' for (i in 1:N) {
#'   if (sum(is.na(X[i, ])) > 0) {
#'     rows_to_delete <- c(rows_to_delete, -i)
#'   }
#' }
#' X <- X[rows_to_delete, ]
#'
#' # Convert X to matrix
#' X <- apply(as.matrix.noquote(X), 2, as.numeric)
#'
#' # Standardize data
#' Z <- scale(X)
#'
#' # Define axis vectors (2-dimensional in this example)
#' r <- c(0.8, 1, 1.2, 1)
#' theta <- c(225, 100, 315, 80) * 2 * pi / 360
#' V <- pracma::zeros(n, 2)
#' for (i in 1:n) {
#'   V[i,1] <- r[i] * cos(theta[i])
#'   V[i,2] <- r[i] * sin(theta[i])
#' }
#'
#' # Select variable for exact estimates, and use it for coloring the embedded
#' # points
#' variable <- sample(1:n, 1)
#'
#' # Compute the mapping
#' mapping <- ara_ordered_linf(
#'   Z,
#'   V,
#'   variable = variable,
#'   solver = "clarabel"
#' )
#'
#' # Select variables with labeled axis lines on ARA plot
#' axis_lines <- variable
#'
#' # Draw the ARA plot
#' draw_ara_plot_2d_standardized(
#'   Z,
#'   X,
#'   V,
#'   mapping$P,
#'   axis_lines = axis_lines,
#'   color_variable = variable
#' )
#'
#'
ara_ordered_linf <- function(
    X,
    V,
    variable = 1,
    solver = "clarabel") {
  ###################   Check validity of input parameters   ###################

  # Check data types -----------------------------------------------------------

  if ((!inherits(X, "matrix") && !inherits(X, "array")) || !is.double(X)) {
    stop("Input error: X must be a numeric matrix")
  }

  if ((!inherits(V, "matrix") && !inherits(V, "array")) || !is.double(V)) {
    stop("Input error: V must be a numeric matrix")
  }

  if (!is.numeric(variable)) {
    stop("Input error: variable must be numeric")
  }

  if (!is.character(solver)) {
    stop("Input error: solver must be a string")
  }


  # Check dimensions of matrices -----------------------------------------------

  nX <- ncol(X)
  nV <- nrow(V)
  m <- ncol(V)

  if ((m < 1) || (m > 3)) {
    stop("Input error: The dimensionality of the visualization space (columns of
         V) must be 1, 2, or 3")
  }

  if (nX != nV) {
    stop("Input error: The number of variables of X (columns) must match the
         number of variables of V (rows)")
  }

  n <- nX


  # Check that inputs have no missing values -----------------------------------

  if (any(is.na(X))) {
    stop("Input error: X cannot contain missing values)")
  }

  if (any(is.na(V))) {
    stop("Input error: V cannot contain missing values)")
  }


  # Check additional preconditions on input parameters -------------------------

  if ((variable < 1) || (variable > n) || (variable %% 1 != 0)) {
    stop("Input error: variable must be an integer in [1,n], where n is the
         number of variables")
  }

  if ((!pracma::strcmpi(solver, "clarabel")) &&
    (!pracma::strcmpi(solver, "glpkAPI")) &&
    (!pracma::strcmpi(solver, "Rglpk")) &&
    (!pracma::strcmpi(solver, "CVXR"))) {
    stop('Input error: solver must be "clarabel", "glpkAPI", "Rglpk", or
         "CVXR"')
  }


  ############################   Compute mapping   #############################

  if (pracma::strcmpi(solver, "CVXR")) {
    outputs <- ara_ordered_linf_CVXR(
      X,
      V,
      variable
    )
  } else if (pracma::strcmpi(solver, "glpkAPI")) {
    outputs <- ara_ordered_linf_glpkAPI(
      X,
      V,
      variable
    )
  } else if (pracma::strcmpi(solver, "clarabel")) {
    outputs <- ara_ordered_linf_clarabel(
      X,
      V,
      variable
    )
  } else { # Rglpk
    outputs <- ara_ordered_linf_Rglpk(
      X,
      V,
      variable
    )
  }

  if (!is.na(outputs$objval)) {
    rank_V <- Matrix::rankMatrix(V)[1]
    if (rank_V < m) {
      Q <- compute_orthogonal_projection_matrix(V, rank_V)
      outputs$P <- outputs$P %*% Q
    }
  }

  list(
    P = outputs$P,
    status = outputs$status,
    objval = outputs$objval
  )
}



#' @noRd
ara_ordered_linf_CVXR <- function(
    X,
    V,
    variable) {
  N <- nrow(X)
  n <- ncol(X)
  m <- ncol(V)

  sort_indices <- order(X[, variable])
  v_k <- V[variable, ]

  Pvar <- CVXR::Variable(N, m)
  tvar <- CVXR::Variable()

  obj <- CVXR::Minimize(tvar)

  constraints <- list()
  constraints <- append(
    constraints,
    -tvar * pracma::ones(N, n) <= Pvar %*% t(V) - X
  )
  constraints <- append(
    constraints,
    Pvar %*% t(V) - X <= tvar * pracma::ones(N, n)
  )

  constraints <- append(
    constraints,
    Pvar[sort_indices[1:(N - 1)], ] %*%
      v_k <= Pvar[sort_indices[2:N], ] %*% v_k
  )

  prob <- CVXR::Problem(obj, constraints)
  solution <- CVXR::solve(prob, solver = "ECOS")

  extract_CVXR_points_status_objval(
    solution,
    Pvar,
    V,
    N,
    m
  )
}


#' @noRd
ara_ordered_linf_glpkAPI <- function(
    X,
    V,
    variable) {
  N <- nrow(X)
  n <- ncol(X)
  m <- ncol(V)

  sort_indices <- order(X[, variable])

  Q <- pracma::repmat(pracma::Reshape(1:(2 * n * N), 2 * n, N), m, 1)
  rows <- c(1:(2 * n * N), unlist(as.list(Q)))

  cols <- c(rep(1, 2 * n * N), rep(2:(N * m + 1), each = 2 * n))

  vals <- c(
    rep(-1, 2 * n * N),
    pracma::repmat(unlist(as.list(rbind(-V, V))), 1, N)
  )


  # Additional inequality constraints
  coo_lists_ineq <- ara_ordered_inequality_coo_lists(
    V[variable, ],
    sort_indices,
    2 * N * n,
    1
  )

  rows <- c(rows, coo_lists_ineq$rows)
  cols <- c(cols, coo_lists_ineq$cols)
  vals <- c(vals, coo_lists_ineq$vals)

  b <- c(unlist(as.list(t(cbind(-X, X)))), rep(0, N - 1))

  obj <- rep(0, 1 + m * N)
  obj[1] <- 1

  ne <- 2 * N * n * (m + 1) + 2 * m * (N - 1) # nonzero elements

  nrows <- 2 * n * N + N - 1
  ncols <- 1 + N * m

  kind <- rep(glpkAPI::GLP_CV, ncols)
  type_cols <- c(glpkAPI::GLP_LO, rep(glpkAPI::GLP_FR, N * m))
  clower <- c(0, rep(-Inf, N * m))
  cupper <- rep(Inf, 1 + N * m)

  type_rows <- rep(glpkAPI::GLP_UP, nrows)
  rlower <- rep(-Inf, nrows)
  rupper <- b

  solve_glpkAPI_wrapper(
    nrows,
    ncols,
    kind,
    clower,
    cupper,
    obj,
    type_cols,
    rlower,
    rupper,
    type_rows,
    ne,
    rows,
    cols,
    vals,
    TRUE,
    2,
    N,
    m
  )
}


#' @noRd
ara_ordered_linf_clarabel <- function(
    X,
    V,
    variable) {
  N <- nrow(X)
  n <- ncol(X)
  m <- ncol(V)

  sort_indices <- order(X[, variable])

  Q <- pracma::repmat(pracma::Reshape(1:(2 * n * N), 2 * n, N), m, 1)
  rows <- c(1:(2 * n * N), unlist(as.list(Q)))

  cols <- c(rep(1, 2 * n * N), rep(2:(N * m + 1), each = 2 * n))

  vals <- c(
    rep(-1, 2 * n * N),
    pracma::repmat(unlist(as.list(rbind(-V, V))), 1, N)
  )


  # Additional inequality constraints
  coo_lists_ineq <- ara_ordered_inequality_coo_lists(
    V[variable, ],
    sort_indices,
    2 * N * n,
    1
  )

  rows <- c(rows, coo_lists_ineq$rows)
  cols <- c(cols, coo_lists_ineq$cols)
  vals <- c(vals, coo_lists_ineq$vals)

  b <- c(unlist(as.list(t(cbind(-X, X)))), rep(0, N - 1))

  obj <- rep(0, 1 + m * N)
  obj[1] <- 1

  A <- Matrix::sparseMatrix(
    i = rows,
    j = cols,
    x = vals,
    index1 = TRUE,
    repr = "C"
  )

  cones <- list(l = (2 * n * N + N - 1))

  solve_clarabel_wrapper(
    A,
    b,
    obj,
    cones,
    NULL,
    2,
    N,
    m
  )
}


#' @noRd
ara_ordered_linf_Rglpk <- function(
    X,
    V,
    variable) {
  N <- nrow(X)
  n <- ncol(X)
  m <- ncol(V)

  sort_indices <- order(X[, variable])

  Q <- pracma::repmat(pracma::Reshape(1:(2 * n * N), 2 * n, N), m, 1)
  rows <- c(1:(2 * n * N), unlist(as.list(Q)))

  cols <- c(rep(1, 2 * n * N), rep(2:(N * m + 1), each = 2 * n))

  vals <- c(
    rep(-1, 2 * n * N),
    pracma::repmat(unlist(as.list(rbind(-V, V))), 1, N)
  )


  # Additional inequality constraints
  coo_lists_ineq <- ara_ordered_inequality_coo_lists(
    V[variable, ],
    sort_indices,
    2 * N * n,
    1
  )

  rows <- c(rows, coo_lists_ineq$rows)
  cols <- c(cols, coo_lists_ineq$cols)
  vals <- c(vals, coo_lists_ineq$vals)

  b <- c(unlist(as.list(t(cbind(-X, X)))), rep(0, N - 1))

  obj <- rep(0, 1 + m * N)
  obj[1] <- 1

  A <- slam::simple_triplet_matrix(
    rows,
    cols,
    vals,
    nrow = 2 * n * N + N - 1,
    ncol = 1 + N * m
  )

  bounds <- list(
    lower = list(
      ind = 2:(1 + N * m),
      val = rep(-Inf, N * m)
    ),
    upper = list()
  )

  dirs <- rep("<=", 2 * n * N + N - 1)

  solve_Rglpk_wrapper(
    A,
    b,
    obj,
    bounds,
    dirs,
    2,
    N,
    m
  )
}
