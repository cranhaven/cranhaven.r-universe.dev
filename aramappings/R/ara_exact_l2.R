#' Exact Adaptable Radial Axes (ARA) mappings using the L2 norm
#'
#' @description
#' \code{ara_exact_l2()} computes \strong{exact} \strong{Adaptable Radial Axes}
#' (ARA) mappings for the \strong{L2 norm}
#'
#' @details
#' \code{ara_exact_l2()} computes low-dimensional point representations of
#' high-dimensional numerical data (\code{X}) according to the data
#' visualization method "Adaptable Radial Axes" (M. Rubio-Sánchez, A. Sanchez,
#' and D. J. Lehmann (2017), doi: 10.1111/cgf.13196), which
#' describes a collection of convex norm optimization problems aimed at
#' minimizing estimates of original values in \code{X} through dot products of
#' the mapped points with the axis vectors (rows of \code{V}). This particular
#' function solves the constrained optimization problem in Eq. (13), for the
#' squared-Euclidean norm. Its equality constraint forces estimates to be exact
#' for one of the data variables. The problem admits closed-form solutions.
#'
#' @inheritParams ara_unconstrained_l1
#' @param variable
#' Integer that indicates the variable (in \[1,n\]) for which the estimates of
#' high-dimensional data will be exact. Default: variable = 1.
#' @param solver
#' String indicating a package or method for solving the optimization problem.
#' It can be "formula" (default), where the solution is obtained through a
#' closed-form formula, or "CVXR".
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
#' The output status vector returns the 2-norm condition number of \code{V}. If
#' the chosen solver fails to map the data (i.e., fails to solve the related
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
#' mapping <- ara_exact_l2(
#'   Z,
#'   V,
#'   variable = variable,
#'   solver = "formula"
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
ara_exact_l2 <- function(
    X,
    V,
    variable = 1,
    solver = "formula") {
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

  if ((!pracma::strcmpi(solver, "formula")) &&
    (!pracma::strcmpi(solver, "CVXR"))) {
    stop('Input error: solver must be "formula" or "CVXR"')
  }


  ############################   Compute mapping   #############################

  if (pracma::strcmpi(solver, "CVXR")) {
    ara_exact_l2_CVXR(
      X,
      V,
      variable
    )
  } else {
    ara_exact_l2_formula(
      X,
      V,
      variable
    )
  }
}



#' @noRd
ara_exact_l2_CVXR <- function(
    X,
    V,
    variable) {
  N <- nrow(X)
  m <- ncol(V)

  v_k <- V[variable, ]
  x_k <- X[, variable]

  Pvar <- CVXR::Variable(N, m)

  obj <- CVXR::Minimize(CVXR::sum_squares(Pvar %*% t(V) - X))

  constraints <- list(Pvar %*% v_k == x_k)

  prob <- CVXR::Problem(obj, constraints)
  solution <- CVXR::solve(prob, solver = "ECOS")

  CVXR_output <- extract_CVXR_points_status_objval(
    solution,
    Pvar,
    V,
    N,
    m
  )

  if (!is.na(CVXR_output$objval)) {
    rank_V <- Matrix::rankMatrix(V)[1]
    if (rank_V < m) {
      Q <- compute_orthogonal_projection_matrix(V, rank_V)
      CVXR_output$P <- CVXR_output$P %*% Q
    }
  }

  list(
    P = CVXR_output$P,
    status = CVXR_output$status,
    objval = CVXR_output$objval
  )
}


#' @noRd
ara_exact_l2_formula <- function(
    X,
    V,
    variable) {
  N <- nrow(X)
  m <- ncol(V)

  v_k <- V[variable, ]
  x_k <- X[, variable]

  VtV <- t(V) %*% V
  r <- Matrix::rankMatrix(V)[1]

  if (r == m) {
    VtVinv <- solve(VtV)
    denominator <- (t(v_k) %*% VtVinv %*% v_k)

    P <- matrix(
      X %*% V %*% VtVinv -
        ((X %*% V %*% VtVinv %*% v_k - x_k) %*% t(v_k) %*% VtVinv) /
          denominator[1],
      N, m
    )
  } else if (r == 1) { # All the axis vectors share the same direction

    denominator <- t(v_k) %*% v_k

    P <- matrix((x_k %*% t(v_k)) / denominator[1], N, m)
  } else { # m=3, but r=2, so all the 3d axis vectors lie on the same 2d plane

    # Create matrix U, whose columns form a basis for the plane spanned by the
    # axis vectors
    U <- matrix(0, 3, 2)
    U[, 1] <- V[variable, ]

    j <- 1
    U[, 2] <- V[j, ]
    while (Matrix::rankMatrix(U)[1] < 2) {
      j <- j + 1
      U[, 2] <- V[j, ]
    }

    Voriginal <- V
    V <- V %*% U
    v_k <- t(U) %*% v_k

    VtV <- t(V) %*% V
    VtVinv <- solve(VtV)
    denominator <- (t(v_k) %*% VtVinv %*% v_k)

    Q <- X %*% V %*% VtVinv -
      ((X %*% V %*% VtVinv %*% v_k - x_k) %*% t(v_k) %*% VtVinv) /
        denominator[1]

    P <- Q %*% t(U)
    V <- Voriginal
  }

  list(
    P = P,
    status = rep(pracma::cond(V), N),
    objval = norm(P %*% t(V) - X, type = "F")^2
  )
}
