#' Unconstrained Adaptable Radial Axes (ARA) mappings using the L2 norm
#'
#' @description
#' \code{ara_unconstrained_l2()} computes \strong{unconstrained}
#' \strong{Adaptable Radial Axes} (ARA) mappings for the \strong{L2 norm}
#'
#' @details
#' \code{ara_unconstrained_l2()} computes low-dimensional point representations
#' of high-dimensional numerical data (\code{X}) according to the data
#' visualization method "Adaptable Radial Axes" (M. Rubio-Sánchez, A. Sanchez,
#' and D. J. Lehmann (2017), doi: 10.1111/cgf.13196), which
#' describes a collection of convex norm optimization problems aimed at
#' minimizing estimates of original values in \code{X} through dot products of
#' the mapped points with the axis vectors (rows of \code{V}). This particular
#' function solves the unconstrained optimization problem in Eq. (10), for the
#' squared-Euclidean norm. Optional non-negative weights (\code{weights})
#' associated with each data variable can be supplied to solve the problem in
#' Eq. (15).
#'
#'
#' @inheritParams ara_unconstrained_l1
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
#'   solution to the optimization problem, considering matrix norms, and
#'   ignoring weights.}
#' }
#' When solver is "formula" this function always produces valid solutions
#' (\code{P}), since the pseudo-inverse matrix always exists. Thus, the output
#' status vector is not relevant, but is returned in consonance with other
#' adaptable radial axes functions in the package. If \pkg{CVRX} were used and
#' failed to map the data observations (i.e., failed to solve the related
#' optimization problem), \code{P} would be a matrix containing \code{NA} (not
#' available) values, and \code{objval} would be also be \code{NA}.
#'
#' @references
#' M. Rubio-Sánchez, A. Sanchez, D. J. Lehmann: Adaptable radial axes plots for
#' improved multivariate data visualization. Computer Graphics Forum 36, 3
#' (2017), 389–399. [doi:10.1111/cgf.13196](https://onlinelibrary.wiley.com/doi/10.1111/cgf.13196)
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
#' # Define weights
#' weights <- c(1, 0.75, 0.75, 1)
#'
#' # Compute the mapping
#' mapping <- ara_unconstrained_l2(
#'   Z,
#'   V,
#'   weights = weights,
#'   solver = "formula"
#' )
#'
#' # Select variables with labeled axis lines on ARA plot
#' axis_lines <- c(1, 4) # 1:"mpg", 4:"acceleration")
#'
#' # Select variable used for coloring embedded points
#' color_variable <- 1 # "mpg"
#'
#' # Draw the ARA plot
#' draw_ara_plot_2d_standardized(
#'   Z,
#'   X,
#'   V,
#'   mapping$P,
#'   weights = weights,
#'   axis_lines = axis_lines,
#'   color_variable = color_variable
#' )
#'
#'
ara_unconstrained_l2 <- function(
    X,
    V,
    weights = rep(1, ncol(X)),
    solver = "formula") {
  ###################   Check validity of input parameters   ###################

  # Check data types -----------------------------------------------------------

  if ((!inherits(X, "matrix") && !inherits(X, "array")) || !is.double(X)) {
    stop("Input error: X must be a numeric matrix")
  }

  if ((!inherits(V, "matrix") && !inherits(V, "array")) || !is.double(V)) {
    stop("Input error: V must be a numeric matrix")
  }

  if (!is.double(weights) && !is.integer(weights)) {
    stop("Input error: weights must be a numeric vector")
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

  if (any(is.na(weights))) {
    stop("Input error: weights cannot contain missing values)")
  }


  # Check additional preconditions on input parameters -------------------------

  if ((length(weights) != n) || (min(weights) < 0)) {
    stop("Input error: weights must be vector of length n with non-negative
         values")
  }

  if ((!pracma::strcmpi(solver, "formula")) &&
    (!pracma::strcmpi(solver, "CVXR"))) {
    stop('Input error: solver must be "formula" or "CVXR"')
  }


  ############################   Compute mapping   #############################

  use_weights <- FALSE
  if (length(unique(weights)) > 1) {
    X_original <- X
    V_original <- V

    W <- diag(weights)
    X <- X %*% W
    V <- W %*% V
    use_weights <- TRUE
  }

  if (pracma::strcmpi(solver, "CVXR")) {
    outputs <- ara_unconstrained_l2_CVXR(
      X,
      V
    )
  } else {
    outputs <- ara_unconstrained_l2_formula(
      X,
      V
    )
  }

  if (use_weights) {
    outputs$objval <- norm(outputs$P %*% t(V_original) -
      X_original, type = "F")^2
  }

  return(outputs)
}



#' @noRd
ara_unconstrained_l2_CVXR <- function(
    X,
    V) {
  N <- nrow(X)
  m <- ncol(V)

  Pvar <- CVXR::Variable(N, m)

  obj <- CVXR::Minimize(CVXR::sum_squares(Pvar %*% t(V) - X))

  constraints <- list()

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
ara_unconstrained_l2_formula <- function(
    X,
    V) {
  N <- nrow(X)

  P <- X %*% t(pracma::pinv(V))

  list(
    P = P,
    status = rep(pracma::cond(V), N),
    objval = norm(P %*% t(V) - X, type = "F")^2
  )
}
