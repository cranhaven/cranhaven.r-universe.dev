#' Exact Adaptable Radial Axes (ARA) mappings using the L1 norm
#'
#' @description
#' \code{ara_exact_l1()} computes \strong{exact} \strong{Adaptable Radial Axes}
#' (ARA) mappings for the \strong{L1 norm}
#'
#'
#'
#' @details
#' \code{ara_exact_l1()} computes low-dimensional point representations of
#' high-dimensional numerical data (\code{X}) according to the data
#' visualization method "Adaptable Radial Axes" (M. Rubio-Sánchez, A. Sanchez,
#' and D. J. Lehmann (2017), doi: 10.1111/cgf.13196), which
#' describes a collection of convex norm optimization problems aimed at
#' minimizing estimates of original values in \code{X} through dot products of
#' the mapped points with the axis vectors (rows of \code{V}). This particular
#' function solves the constrained optimization problem in Eq. (13), for the L1
#' vector norm. Its equality constraint forces estimates to be exact for one of
#' the data variables.
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
#' If the chosen solver fails to map one or more data observations (i.e., fails
#' to solve the related optimization problems), their rows in \code{P} will
#' contain \code{NA} (not available) values. In that case, \code{objval} will
#' also be \code{NA}.
#'
#' @inherit ara_unconstrained_l1 references
#'
#' @export
#'
#' @examples
#' # Define subset of (numerical) variables of the auto_mpg dataset
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
#' # Set the number of CPU cores/workers
#' # NCORES <- parallelly::availableCores(omit = 1)
#' # NCORES <- max(1,parallel::detectCores() - 1)
#' NCORES <- 2L
#'
#' # Create a cluster for parallel processing
#' cl <- parallel::makeCluster(NCORES)
#'
#' # Compute the mapping
#' mapping <- ara_exact_l1(
#'   Z,
#'   V,
#'   variable = variable,
#'   solver = "glpkAPI",
#'   use_glpkAPI_simplex = TRUE,
#'   cluster = cl
#' )
#'
#' # Stop cluster
#' parallel::stopCluster(cl)
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
ara_exact_l1 <- function(
    X,
    V,
    variable = 1,
    solver = "glpkAPI",
    use_glpkAPI_simplex = TRUE,
    cluster = NULL) {
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

  if (!is.logical(use_glpkAPI_simplex)) {
    stop("Input error: use_glpkAPI_simplex must be logical (Boolean)")
  }

  if ((!is.null(cluster)) &&
    !(inherits(cluster, "SOCKcluster") || inherits(cluster, "cluster"))) {
    stop("Input error: invalid cluster argument")
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
    outputs <- ara_exact_l1_CVXR(
      X,
      V,
      variable
    )
  } else {
    if (pracma::strcmpi(solver, "glpkAPI")) {
      outputs <- ara_exact_l1_glpkAPI(
        X,
        V,
        variable,
        use_glpkAPI_simplex,
        cluster
      )
    } else if (pracma::strcmpi(solver, "clarabel")) {
      outputs <- ara_exact_l1_clarabel(
        X,
        V,
        variable,
        cluster
      )
    } else {
      outputs <- ara_exact_l1_Rglpk(
        X,
        V,
        variable,
        cluster
      )
    }
    if (m == 1) {
      outputs$P <- t(outputs$P)
    }
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
ara_exact_l1_CVXR <- function(
    X,
    V,
    variable) {
  N <- nrow(X)
  n <- ncol(X)
  m <- ncol(V)

  v_k <- V[variable, ]
  x_k <- X[, variable]

  Pvar <- CVXR::Variable(N, m)
  Tvar <- CVXR::Variable(N, n)

  obj <- CVXR::Minimize(sum(Tvar))

  constraints <- list()
  constraints <- append(constraints, -Tvar <= Pvar %*% t(V) - X)
  constraints <- append(constraints, Pvar %*% t(V) - X <= Tvar)

  constraints <- append(constraints, Pvar %*% v_k == x_k)

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
ara_exact_l1_glpkAPI <- function(
    X,
    V,
    variable,
    use_glpkAPI_simplex,
    cluster) {
  N <- nrow(X)
  n <- ncol(X)
  m <- ncol(V)

  v_k <- V[variable, ]

  obj <- c(rep(1, n), rep(0, m))

  ne <- 2 * n * (m + 1) + m

  # Matrix of inequality constraints
  coo_lists <- ara_l1_norm_coo_lists(1, V, 1)

  # Append equality constraint to the sparse matrix
  rows <- c(rep(1, m), coo_lists$rows)
  cols <- c((n + 1):(n + m), coo_lists$cols)
  vals <- c(v_k, coo_lists$vals)

  nrows <- 2 * n + 1
  ncols <- n + m

  kind <- rep(glpkAPI::GLP_CV, ncols)
  type_cols <- c(rep(glpkAPI::GLP_LO, n), rep(glpkAPI::GLP_FR, m))
  clower <- c(rep(0, n), rep(-Inf, m))
  cupper <- rep(Inf, n + m)

  type_rows <- c(glpkAPI::GLP_FX, rep(glpkAPI::GLP_UP, nrows))

  if (is.null(cluster)) {
    sol <- apply(X = X, MARGIN = 1, function(x) {
      min_exact_glpkAPI(
        x,
        nrows,
        ncols,
        kind,
        clower,
        cupper,
        obj,
        type_cols,
        type_rows,
        ne,
        rows,
        cols,
        vals,
        variable,
        use_glpkAPI_simplex,
        m
      )
    })
  } else {
    parallel::clusterEvalQ(cluster, library(glpkAPI))
    parallel::clusterExport(cluster,
      c("min_exact_glpkAPI", "solve_glpkAPI_wrapper"),
      envir = environment()
    )

    sol <- parallel::parApply(cluster, X = X, MARGIN = 1, function(x) {
      min_exact_glpkAPI(
        x,
        nrows,
        ncols,
        kind,
        clower,
        cupper,
        obj,
        type_cols,
        type_rows,
        ne,
        rows,
        cols,
        vals,
        variable,
        use_glpkAPI_simplex,
        m
      )
    })
  }

  sol_matrix <- pracma::Reshape(unlist(sol), m + 2, N)

  list(
    P = t(sol_matrix[1:m, ]),
    status = sol_matrix[m + 1, ],
    objval = sum(sol_matrix[m + 2, ])
  )
}


#' @noRd
ara_exact_l1_clarabel <- function(
    X,
    V,
    variable,
    cluster) {
  N <- nrow(X)
  n <- ncol(X)
  m <- ncol(V)

  v_k <- V[variable, ]

  obj <- c(rep(1, n), rep(0, m))

  A <- rbind(c(rep(0, n), v_k), cbind(diag(-1, n), -V), cbind(diag(-1, n), V))

  cones <- list(z = 1, l = (2 * n))

  if (is.null(cluster)) {
    sol <- apply(X = X, MARGIN = 1, function(x) {
      min_exact_clarabel(
        x,
        A,
        obj,
        cones,
        variable,
        m
      )
    })
  } else {
    parallel::clusterEvalQ(cluster, library(clarabel))
    parallel::clusterExport(cluster,
      c("min_exact_clarabel", "solve_clarabel_wrapper"),
      envir = environment()
    )

    sol <- parallel::parApply(cluster, X = X, MARGIN = 1, function(x) {
      min_exact_clarabel(
        x,
        A,
        obj,
        cones,
        variable,
        m
      )
    })
  }

  sol_matrix <- pracma::Reshape(unlist(sol), m + 2, N)

  list(
    P = t(sol_matrix[1:m, ]),
    status = sol_matrix[m + 1, ],
    objval = sum(sol_matrix[m + 2, ])
  )
}


#' @noRd
ara_exact_l1_Rglpk <- function(
    X,
    V,
    variable,
    cluster) {
  N <- nrow(X)
  n <- ncol(X)
  m <- ncol(V)

  v_k <- V[variable, ]

  obj <- c(rep(1, n), rep(0, m))

  # Matrix of inequality constraints
  coo_lists <- ara_l1_norm_coo_lists(1, V, 1)

  # Append equality constraint to the sparse matrix
  rows <- c(rep(1, m), coo_lists$rows)
  cols <- c((n + 1):(n + m), coo_lists$cols)
  vals <- c(v_k, coo_lists$vals)

  A <- slam::simple_triplet_matrix(
    rows,
    cols,
    vals,
    nrow = 1 + 2 * n,
    ncol = n + m
  )

  bounds <- list(
    lower = list(ind = (1 + n):(n + m), val = rep(-Inf, m)),
    upper = list()
  )

  dirs <- c("==", rep("<=", 2 * n))

  if (is.null(cluster)) {
    sol <- apply(X = X, MARGIN = 1, function(x) {
      min_exact_Rglpk(
        x,
        A,
        obj,
        bounds,
        dirs,
        variable,
        m
      )
    })
  } else {
    parallel::clusterEvalQ(cluster, library(Rglpk))
    parallel::clusterExport(cluster,
      c("min_exact_Rglpk", "solve_Rglpk_wrapper"),
      envir = environment()
    )

    sol <- parallel::parApply(cluster, X = X, MARGIN = 1, function(x) {
      min_exact_Rglpk(
        x,
        A,
        obj,
        bounds,
        dirs,
        variable,
        m
      )
    })
  }

  sol_matrix <- pracma::Reshape(unlist(sol), m + 2, N)

  list(
    P = t(sol_matrix[1:m, ]),
    status = sol_matrix[m + 1, ],
    objval = sum(sol_matrix[m + 2, ])
  )
}
