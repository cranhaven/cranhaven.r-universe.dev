#' Define a Multivariate Functional Data Object
#'
#' This is the constructor function for objects of the mfd class.
#' It is a wrapper to \code{fda::\link[fda]{fd}},
#' but it forces the coef argument to  be
#' a three-dimensional array of coefficients even if
#' the functional data is univariate.
#' Moreover, it allows to include the original raw data from which
#' you get the smooth functional data.
#' Finally, it also includes the matrix of precomputed inner products
#' of the basis functions, which can be useful to speed up computations
#' when calculating inner products between functional observations
#'
#' @param coef
#' A three-dimensional array of coefficients:
#'
#' * the first dimension corresponds to basis functions.
#'
#' * the second dimension corresponds to the number of
#' multivariate functional observations.
#'
#' * the third dimension corresponds to variables.
#' @param basisobj
#' A functional basis object defining the basis,
#' as provided to \code{fda::\link[fda]{fd}}, but there is no default.
#' @param fdnames
#' A list of length 3, each member being a string vector
#' containing labels for the levels of the corresponding dimension
#' of the discrete data.
#'
#' The first dimension is for a single character indicating the argument
#' values,
#' i.e. the variable on the functional domain.
#'
#' The second is for replications, i.e. it denotes the functional observations.
#'
#' The third is for functional variables' names.
#' @param raw
#' A data frame containing the original discrete data.
#' Default is NULL, however, if provided, it must contain:
#'
#' a column (indicated by the \code{id_var} argument)
#' denoting the functional observations,
#' which must correspond to values in \code{fdnames[[2]]},
#'
#' a column named as \code{fdnames[[1]]},
#' returning the argument values of each function
#'
#' as many columns as the functional variables,
#' named as in \code{fdnames[[3]]},
#' containing the discrete functional values for each variable.
#' @param id_var
#' A single character value indicating the column
#' in the \code{raw} argument
#' containing the functional observations (as in \code{fdnames[[2]]}),
#' default is NULL.
#' @param B
#' A matrix with the inner products of the basis functions.
#' If NULL, it is calculated from the basis object provided.
#' Default is NULL.
#'
#' @return
#' A multivariate functional data object
#' (i.e., having class \code{mfd}),
#' which is a list with components named
#' \code{coefs}, \code{basis}, and \code{fdnames},
#' as for class \code{fd},
#' with possibly in addition the components \code{raw} and \code{id_var}.
#'
#' @details
#' To check that an object is of this class, use function is.mfd.
#'
#' @references
#' Ramsay, James O., and Silverman, Bernard W. (2006),
#' \emph{Functional Data Analysis}, 2nd ed., Springer, New York.
#'
#' Ramsay, James O., and Silverman, Bernard W. (2002),
#' \emph{Applied Functional Data Analysis}, Springer, New York.
#'
#' @export
#'
#' @examples
#' library(funcharts)
#' library(fda)
#' set.seed(0)
#' nobs <- 5
#' nbasis <- 10
#' nvar <- 2
#' coef <- array(rnorm(nobs * nbasis * nvar), dim = c(nbasis, nobs, nvar))
#' bs <- create.bspline.basis(rangeval = c(0, 1), nbasis = nbasis)
#' mfdobj <- mfd(coef = coef, basisobj = bs)
#' plot_mfd(mfdobj)
#'
mfd <- function(coef,
                basisobj,
                fdnames = NULL,
                raw = NULL,
                id_var = NULL,
                B = NULL) {

  if (is.null(fdnames)) {
    fdnames <- list(
      "t",
      paste0("rep", seq_len(dim(coef)[2])),
      paste0("var", seq_len(dim(coef)[3]))
    )
  }
  if (sum(is.null(raw), is.null(id_var)) == 1) {
    stop("If either raw or id_var are not NULL, both must be not NULL")
  }
  if (!is.null(raw)) {
    # if (!(fdnames[[1]] %in% names(raw))) {
    #   stop("fdnames[[1]] must be the name of a column of the raw data.")
    # }
    if (!is.data.frame(raw)) {
      stop("raw must be a data frame.")
    }
    if (!(is.character(id_var) & length(id_var) == 1)) {
      stop ("id_var must be a single character")
    }
    if (!(class(raw[[id_var]]) %in% c("character", "factor"))) {
      stop("column id_var of raw data frame must be a character or factor.")
    }
    if (!(setequal(unique(raw[[id_var]]), fdnames[[2]]))) {
      stop(paste0("column id_var of raw data frame must contain",
                  "the same values as fdnames[[2]]."))
    }
  }
  if (!(basisobj$type %in% c("bspline", "fourier", "const", "expon",
                             "monom", "polygonal", "power"))) {
    stop("supported basis systems are bspline, fourier, constant,
         exponential, monomial, polygonal, power")
  }
  if (!is.array(coef)) {
    stop("'coef' is not array")
  }
  coefd <- dim(coef)
  ndim <- length(coefd)
  if (ndim != 3) {
    stop("'coef' not of dimension 3")
  }

  if (coefd[1] != basisobj$nbasis) {
    stop("1st dimension of coef must be equal to basisobj$nbasis")
  }

  fdobj <- fda::fd(coef, basisobj, fdnames)
  fdobj$raw <- raw
  fdobj$id_var <- id_var
  nb <- basisobj$nbasis

  if (is.null(B)) {
    if (basisobj$type == "bspline") {
      B <- fda::inprod.bspline(fda::fd(diag(nb), basisobj))
    }
    if (basisobj$type == "fourier") {
      B <- diag(nb)
    }
    if (basisobj$type == "const") {
      B <- matrix(diff(basisobj$rangeval))
    }
    if (basisobj$type == "expon") {
      out_mat <- outer(basisobj$params, basisobj$params, "+")
      exp_out_mat <- exp(out_mat)
      B <- (exp_out_mat ^ basisobj$rangeval[2] -
              exp_out_mat ^ basisobj$rangeval[1]) / out_mat
      B[out_mat == 0] <- diff(basisobj$rangeval)
    }
    if (basisobj$type == "monom") {
      out_mat <- outer(basisobj$params, basisobj$params, "+") + 1
      B <- (basisobj$rangeval[2] ^ out_mat -
              basisobj$rangeval[1] ^ out_mat) / out_mat
    }
    if (basisobj$type == "polygonal") {
      B <- inprod_fd(fda::fd(diag(basisobj$nbasis), basisobj),
                     fda::fd(diag(basisobj$nbasis), basisobj))
    }
    if (basisobj$type == "power") {
      out_mat <- outer(basisobj$params, basisobj$params, "+") + 1
      B <- (basisobj$rangeval[2] ^ out_mat -
              basisobj$rangeval[1] ^ out_mat) / out_mat
      B[out_mat == 0] <- log(basisobj$rangeval[2]) -
        log(basisobj$rangeval[1])
    }
  }
  if (!is.matrix(B)) stop("B must be a matrix")
  if (nrow(B) != nb | ncol(B) != nb)
    stop("B must have the right number of rows and columns")
  fdobj$basis$B <- B
  class(fdobj) <- c("mfd", "fd")
  fdobj
}


#' Confirm Object has Class \code{mfd}
#'
#' Check that an argument is a multivariate
#' functional data object of class \code{mfd}.
#'
#' @param mfdobj An object to be checked.
#'
#' @return a logical value: TRUE if the class is correct, FALSE otherwise.
#' @export
#'
is.mfd <- function(mfdobj) if (inherits(mfdobj, "mfd")) TRUE else FALSE

#' Add multivariate functional data
#'
#' Adds two objects of class `mfd` (elementwise on their coefficient arrays).
#' The two objects must share the same basis system and number of variables.
#' If one object contains a single replication (i.e., one observation) and the
#' other contains multiple, the single replication is replicated across
#' observations before addition.
#'
#' If \code{mfdobj2} is missing, the function returns \code{mfdobj1} unchanged
#' (i.e., unary plus).
#'
#' @param mfdobj1,mfdobj2 Objects of class `mfd`. If \code{mfdobj2} is missing,
#'   unary plus is applied.
#'
#' @details
#' Let the coefficient arrays have dimensions \eqn{(nbasis, nobs, nvar)}.
#' The following checks/rules are enforced:
#' \itemize{
#'   \item Both inputs must be `mfd` objects; otherwise an error is thrown.
#'   \item The basis systems must be identical (checked via \code{identical()}).
#'   \item The number of variables must match.
#'   \item For the number of observations: if both \eqn{nobs_1} and \eqn{nobs_2}
#'         are greater than one, they must be equal; otherwise, the object with
#'         \eqn{nobs = 1} is replicated to match the other.
#' }
#'
#' @return An object of class `mfd` with coefficients equal to the (possibly
#'   replicated) sum of the inputs. The \code{fdnames} are taken from the input
#'   providing the observation indexing after replication (if any), otherwise
#'   from \code{mfdobj1}.
#'
#' @seealso \code{\link{nobs}}, \code{\link{nbasis}}, \code{\link{nvar}}, \code{\link{mfd}}
#'
#' @examples
#' # Assuming mfdobj_a and mfdobj_b are 'mfd' objects on the same basis:
#' # mfdobj_a + mfdobj_b
#' # plus_mfd(mfdobj_a, mfdobj_b)
#'
#' @export
plus_mfd <- function(mfdobj1, mfdobj2) {
  if (missing(mfdobj2)) {
    return(mfdobj1)
  }
  if (!is.mfd(mfdobj1) | !is.mfd(mfdobj2)) {
    stop("Both arguments must be of class 'mfd'.")
  }
  if (!identical(mfdobj1$basis, mfdobj2$basis)) {
    stop("The basis systems of the two objects are different.")
  }
  bs <- mfdobj1$basis
  n1 <- nobs(mfdobj1)
  n2 <- nobs(mfdobj2)
  if (n1 != n2 & n1 > 1 & n2 > 1) {
    stop("The number of replications in the two objects are different.")
  }
  p1 <- nvar(mfdobj1)
  p2 <- nvar(mfdobj2)
  if (p1 != p2) {
    stop("The number of variables in the two objects are different.")
  }
  A1 <- mfdobj1$coefs
  A2 <- mfdobj2$coefs
  if (n1 == n2) {
    A <- A1 + A2
    fdnames <- mfdobj1$fdnames
  }
  if (n1 == 1 & n2 > 1) {
    A <- A1[, rep(1, n2), ] + A2
    fdnames <- mfdobj2$fdnames
  }
  if (n1 > 1 & n2 == 1) {
    A <- A1 + A2[, rep(1, n1), ]
    fdnames <- mfdobj1$fdnames
  }
  mfd(A, bs, fdnames)
}

#' @rdname plus_mfd
#' @export
`+.mfd` <- function(mfdobj1, mfdobj2) {
  # Case: scalar + mfd
  if (is.numeric(mfdobj1) && length(mfdobj1) == 1 && is.mfd(mfdobj2)) {
    out <- mfdobj2
    out$coefs <- out$coefs + mfdobj1
    return(out)
  }
  # Case: mfd + scalar
  if (is.mfd(mfdobj1) && is.numeric(mfdobj2) && length(mfdobj2) == 1) {
    out <- mfdobj1
    out$coefs <- out$coefs + mfdobj2
    return(out)
  }
  # Case: both mfd → original behavior
  if (is.mfd(mfdobj1) && is.mfd(mfdobj2)) {
    return(plus_mfd(mfdobj1, mfdobj2))
  }
  # Otherwise → error
  stop("At least one of the operands must be an 'mfd' object, and the other either 'mfd' or scalar numeric.")
}



#' Subtract multivariate functional data (and unary negation)
#'
#' Subtracts two objects of class `mfd` (elementwise on their coefficient arrays).
#' The same basis, variable-count, and observation-replication rules as in
#' \code{\link{plus_mfd}} apply. If \code{mfdobj2} is missing, returns the
#' unary negation of \code{mfdobj1}.
#'
#' @param mfdobj1,mfdobj2 Objects of class `mfd`. If \code{mfdobj2} is missing,
#'   unary minus is applied to \code{mfdobj1}.
#'
#' @return An object of class `mfd` with coefficients equal to the (possibly
#'   replicated) difference \code{mfdobj1 - mfdobj2}, or the negation of
#'   \code{mfdobj1} for unary minus.
#'
#' @seealso \code{\link{plus_mfd}}, \code{\link{nobs}}, \code{\link{nbasis}}, \code{\link{nvar}}, \code{\link{mfd}}
#'
#' @examples
#' # mfdobj_a - mfdobj_b
#' # minus_mfd(mfdobj_a, mfdobj_b)
#' # Unary minus:
#' # -mfdobj_a
#'
#' @export
minus_mfd <- function(mfdobj1, mfdobj2) {
  if (missing(mfdobj2)) {
    mfdobj1$coefs <- -mfdobj1$coefs
    return(mfdobj1)
  }
  mfdobj2$coefs <- -mfdobj2$coefs
  plus_mfd(mfdobj1, mfdobj2)
}

#' @rdname minus_mfd
#' @export
`-.mfd` <- function(mfdobj1, mfdobj2) {
  # Unary minus
  if (missing(mfdobj2)) {
    out <- mfdobj1
    out$coefs <- -out$coefs
    return(out)
  }
  # Case: scalar - mfd
  if (is.numeric(mfdobj1) && length(mfdobj1) == 1 && is.mfd(mfdobj2)) {
    out <- mfdobj2
    out$coefs <- mfdobj1 - out$coefs
    return(out)
  }
  # Case: mfd - scalar
  if (is.mfd(mfdobj1) && is.numeric(mfdobj2) && length(mfdobj2) == 1) {
    out <- mfdobj1
    out$coefs <- out$coefs - mfdobj2
    return(out)
  }
  # Case: both mfd → original behavior
  if (is.mfd(mfdobj1) && is.mfd(mfdobj2)) {
    return(minus_mfd(mfdobj1, mfdobj2))
  }
  # Otherwise → error
  stop("At least one of the operands must be an 'mfd' object, and the other either 'mfd' or scalar numeric.")
}




#' Pointwise product of multivariate functional data (and scalar multiplication)
#'
#' Computes the elementwise (pointwise) product of two objects of class `mfd`,
#' returning an `mfd` on the same basis. If one object contains a single
#' replication (one observation) and the other contains multiple, the single
#' replication is recycled across observations before multiplication.
#'
#' Alternatively, it also compute the product of an `mfd` object with a numeric scalar.
#'
#' @param mfdobj1,mfdobj2 Objects of class `mfd` defined on the same basis.
#'
#' @details
#' Let coefficient arrays have dimensions \eqn{(nbasis, nobs, nvar)}.
#' The function:
#' \itemize{
#'   \item requires both inputs to be `mfd` objects;
#'   \item requires identical basis systems (checked with \code{identical()});
#'   \item requires the same number of variables;
#'   \item for observations: if both \eqn{nobs_1} and \eqn{nobs_2} are greater
#'         than one, they must be equal; otherwise, the object with
#'         \eqn{nobs = 1} is replicated to match the other.
#' }
#'
#' Internally, coefficient arrays are converted to \code{\link[fda]{fd}} objects
#' and multiplied via \code{\link[fda]{times.fd}}, with \code{basisobj} set to
#' the common basis so that the result is re-expanded on the same basis.
#'
#' @return An object of class `mfd` whose coefficients are the pointwise product
#'   of the inputs (with recycling if needed). The basis is the common input
#'   basis. The \code{fdnames} are inherited from the input that supplies the
#'   observation indexing after any replication.
#'
#' @seealso \code{\link{plus_mfd}}, \code{\link{minus_mfd}},
#'   \code{\link[stats]{nobs}}, \code{\link{nvar}}, \code{\link{nbasis}},
#'   \code{\link[fda]{times.fd}}, \code{\link{mfd}}
#'
#' @examples
#' # Assuming mfdobj_a and mfdobj_b are 'mfd' objects on the same basis:
#' # mfdobj_a * mfdobj_b   # elementwise product
#' # 2 * mfdobj_a          # scalar multiplication
#' # mfdobj_a * 0.5        # scalar multiplication
#'
#' @export
times_mfd <- function(mfdobj1, mfdobj2) {
  if (!is.mfd(mfdobj1) | !is.mfd(mfdobj2)) {
    stop("Both arguments must be of class 'mfd'.")
  }
  if (!identical(mfdobj1$basis, mfdobj2$basis)) {
    stop("The basis systems of the two objects are different.")
  }
  bs <- mfdobj1$basis
  n1 <- nobs(mfdobj1)
  n2 <- nobs(mfdobj2)
  if (n1 != n2 & n1 > 1 & n2 > 1) {
    stop("The number of replications in the two objects are different.")
  }
  p1 <- nvar(mfdobj1)
  p2 <- nvar(mfdobj2)
  if (p1 != p2) {
    stop("The number of variables in the two objects are different.")
  }
  A1 <- mfdobj1$coefs
  A2 <- mfdobj2$coefs
  if (n1 == n2) {
    fdobj1 <- fda::fd(A1, bs)
    fdobj2 <- fda::fd(A2, bs)
    fdprod <- fda::times.fd(fdobj1, fdobj2, basisobj = bs)
    fdnames <- mfdobj1$fdnames
  }
  if (n1 == 1 & n2 > 1) {
    A1 <- A1[, rep(1, n2), ]
    fdobj1 <- fda::fd(A1, bs)
    fdobj2 <- fda::fd(A2, bs)
    fdprod <- fda::times.fd(fdobj1, fdobj2, basisobj = bs)
    fdnames <- mfdobj2$fdnames
  }
  if (n1 > 1 & n2 == 1) {
    A2 <- A2[, rep(1, n1), ]
    fdobj1 <- fda::fd(A1, bs)
    fdobj2 <- fda::fd(A2, bs)
    fdprod <- fda::times.fd(fdobj1, fdobj2, basisobj = bs)
    fdnames <- mfdobj1$fdnames
  }
  A <- fdprod$coefs
  out <- mfd(A, bs, fdnames)
  return(out)
}

#' @rdname times_mfd
#' @export
`*.mfd` <- function(mfdobj1, mfdobj2) {
  # Case: scalar * mfd
  if (is.numeric(mfdobj1) && length(mfdobj1) == 1 && is.mfd(mfdobj2)) {
    out <- mfdobj2
    out$coefs <- out$coefs * mfdobj1
    return(out)
  }
  # Case: mfd * scalar
  if (is.mfd(mfdobj1) && is.numeric(mfdobj2) && length(mfdobj2) == 1) {
    out <- mfdobj1
    out$coefs <- out$coefs * mfdobj2
    return(out)
  }
  # Case: both mfd → use times_mfd
  if (is.mfd(mfdobj1) && is.mfd(mfdobj2)) {
    return(times_mfd(mfdobj1, mfdobj2))
  }
  # Otherwise → error
  stop("At least one of the operands must be an 'mfd' object, and the other either 'mfd' or scalar numeric.")
}



#' Simulate multivariate functional data
#'
#' Simulate random coefficients and create a multivariate functional data
#' object of class `mfd`.
#' It is mainly for internal use, to check that the package functions
#' work.
#'
#' @param nobs Number of functional observations to be simulated.
#' @param nbasis Number of basis functions.
#' @param nvar Number of functional covariates.
#' @param seed Deprecated: use \code{set.seed()} before calling
#' the function for reproducibility.
#'
#' @return
#' A simulated object of class `mfd`.
#' @export
#'
#' @examples
#' library(funcharts)
#' data_sim_mfd()
data_sim_mfd <- function(nobs = 5,
                         nbasis = 5,
                         nvar = 2,
                         seed) {
  if (!missing(seed)) {
    warning(paste0("argument seed is deprecated; ",
                   "please use set.seed()
                   before calling the function instead."),
            call. = FALSE)
  }
  coef <- array(stats::rnorm(nobs * nbasis * nvar),
                dim = c(nbasis, nobs, nvar))
  bs <- fda::create.bspline.basis(rangeval = c(0, 1), nbasis = nbasis)
  mfd(coef = coef, basisobj = bs)
}



#' Extract observations and/or variables from \code{mfd} objects.
#'
#' @param mfdobj An object of class \code{mfd}.
#' @param i
#' Index specifying functional observations to extract or replace.
#' They can be numeric, character,
#' or logical vectors or empty (missing) or NULL.
#' Numeric values are coerced to integer as by as.integer
#' (and hence truncated towards zero).
#' The can also be negative integers,
#' indicating functional observations to leave out of the selection.
#' Logical vectors indicate TRUE for the observations to select.
#' Character vectors will be matched
#' to the argument \code{fdnames[[2]]} of \code{mfdobj},
#' i.e. to functional observations' names.
#' @param j
#' Index specifying functional variables to extract or replace.
#' They can be numeric, logical,
#' or character vectors or empty (missing) or NULL.
#' Numeric values are coerced to integer as by as.integer
#' (and hence truncated towards zero).
#' The can also be negative integers,
#' indicating functional variables to leave out of the selection.
#' Logical vectors indicate TRUE for the variables to select.
#' Character vectors will be matched
#' to the argument \code{fdnames[[3]]} of \code{mfdobj},
#' i.e. to functional variables' names.
#'
#' @return a \code{mfd} object with selected observations and variables.
#'
#' @details
#' This function adapts the \code{fda::"[.fd"}
#' function to be more robust and suitable
#' for the \code{mfd} class.
#' In fact, whatever the number of observations
#' or variables you want to extract,
#' it always returns a \code{mfd} object with a three-dimensional coef array.
#' In other words, it behaves as you would
#' always use the argument \code{drop=FALSE}.
#' Moreover, you can extract observations
#' and variables both by index numbers and by names,
#' as you would normally do when using
#' \code{`[`} with standard vector/matrices.
#'
#' @export
#' @examples
#' library(funcharts)
#' library(fda)
#'
#' # In the following, we extract the first one/two observations/variables
#' # to see the difference with `[.fd`.
#' mfdobj <- data_sim_mfd()
#' fdobj <- fd(mfdobj$coefs, mfdobj$basis, mfdobj$fdnames)
#'
#' # The argument `coef` in `fd`
#' # objects is converted to a matrix when possible.
#' dim(fdobj[1, 1]$coef)
#' # Not clear what is the second dimension:
#' # the number of replications or the number of variables?
#' dim(fdobj[1, 1:2]$coef)
#' dim(fdobj[1:2, 1]$coef)
#'
#' # The argument `coef` in `mfd` objects is always a three-dimensional array.
#' dim(mfdobj[1, 1]$coef)
#' dim(mfdobj[1, 1:2]$coef)
#' dim(mfdobj[1:2, 1]$coef)
#'
#' # Actually, `[.mfd` works as `[.fd` when passing also `drop = FALSE`
#' dim(fdobj[1, 1, drop = FALSE]$coef)
#' dim(fdobj[1, 1:2, drop = FALSE]$coef)
#' dim(fdobj[1:2, 1, drop = FALSE]$coef)
#'
"[.mfd" <- function(mfdobj, i = TRUE, j = TRUE) {
  if (!(is.mfd(mfdobj))) {
    stop(paste0("First argument must be a ",
                "multivariate functional data object."))
  }

  coefs <- mfdobj$coefs[, i, j, drop = FALSE]
  fdnames <- mfdobj$fdnames
  fdnames[[2]] <- dimnames(coefs)[[2]]
  fdnames[[3]] <- dimnames(coefs)[[3]]
  if (is.null(mfdobj$raw))
    raw_filtered <- id_var <- NULL else {
      raw <- mfdobj$raw
      id_var <- mfdobj$id_var
      raw_filtered <- raw[raw[[id_var]] %in% fdnames[[2]], , drop = FALSE]
    }

  mfd(coef = coefs, basisobj = mfdobj$basis, fdnames = fdnames,
      raw = raw_filtered, id_var = id_var, B = mfdobj$basis$B)
}

#' Inner products of functional data contained in \code{mfd} objects.
#'
#' @param mfdobj1
#' A multivariate functional data object of class \code{mfd}.
#' @param mfdobj2
#' A multivariate functional data object of class \code{mfd}.
#' It must have the same functional variables as \code{mfdobj1}.
#' If NULL, it is equal to \code{mfdobj1}.
#'
#' @return
#' a three-dimensional array of \emph{L^2} inner products.
#' The first dimension is the number of functions in argument mfdobj1,
#' the second dimension is the same thing for argument mfdobj2,
#' the third dimension is the number of functional variables.
#' If you sum values over the third dimension,
#' you get a matrix of inner products
#' in the product Hilbert space of multivariate functional data.
#'
#' @details
#' Note that \emph{L^2} inner products are not calculated
#' for couples of functional data
#' from different functional variables.
#' This function is needed to calculate the
#' inner product in the product Hilbert space
#' in the case of multivariate functional data,
#' which for each observation is the sum of the \emph{L^2}
#' inner products obtained for each functional variable.
#'
#' @export
#' @examples
#' library(funcharts)
#' set.seed(123)
#' mfdobj1 <- data_sim_mfd()
#' mfdobj2 <- data_sim_mfd()
#' inprod_mfd(mfdobj1)
#' inprod_mfd(mfdobj1, mfdobj2)
inprod_mfd <- function(mfdobj1, mfdobj2 = NULL) {

  if (!(is.mfd(mfdobj1))) {
    stop("First argument is not a multivariate functional data object.")
  }
  if (!is.null(mfdobj2)) {
    if (!(is.mfd(mfdobj2))) {
      stop("Second argument is not a multivariate functional data object.")
    }
    if (length(mfdobj1$fdnames[[3]]) != length(mfdobj2$fdnames[[3]])) {
      stop(paste0("mfdobj1 and mfdobj2 do not have the ",
                  "same number of functional variables."))
    }
  } else mfdobj2 <- mfdobj1

  variables <- mfdobj1$fdnames[[3]]
  n_var <- length(variables)
  ids1 <- mfdobj1$fdnames[[2]]
  ids2 <- mfdobj2$fdnames[[2]]
  n_obs1 <- length(ids1)
  n_obs2 <- length(ids2)
  bs1 <- mfdobj1$basis
  bs2 <- mfdobj2$basis

  inprods <- array(NA, dim = c(n_obs1, n_obs2, n_var),
                   dimnames = list(ids1, ids2, variables))
  C1 <- mfdobj1$coefs
  C2 <- mfdobj2$coefs
  inprods[] <- vapply(seq_len(n_var), function(jj) {
    C1jj <- matrix(C1[, , jj], nrow = dim(C1)[1], ncol = dim(C1)[2])
    C2jj <- matrix(C2[, , jj], nrow = dim(C2)[1], ncol = dim(C2)[2])

    if (identical(bs1, bs2)) {
      if (bs1$type == "fourier") {
        out <- as.matrix(t(C1jj) %*% C2jj)
      }
      if (bs1$type %in% c("bspline", "expon", "monom", "polygonal", "power")) {
        W <- bs1$B
        out <- as.matrix(t(C1jj) %*% W %*% C2jj)
      }
      if (bs1$type == "const") {
        W <- bs1$B
        out <- t(C1jj) %*% C2jj * diff(bs1$rangeval)
      }
    } else {
      fdobj1_jj <- fda::fd(C1jj, bs1)
      fdobj2_jj <- fda::fd(C2jj, bs2)
      out <- inprod_fd(fdobj1_jj, fdobj2_jj)
    }
    out
  }, numeric(dim(mfdobj1$coefs)[2] * dim(mfdobj2$coefs)[2]))

  inprods

}

#' Norm of Multivariate Functional Data
#'
#' Norm of multivariate functional data contained
#' in a \code{mfd} object.
#'
#' @param mfdobj A multivariate functional data object of class \code{mfd}.
#'
#' @return
#' A vector of length equal to the number of replications
#' in \code{mfdobj},
#' containing the norm of each multivariate functional observation
#' in the product Hilbert space,
#' i.e. the sum of \emph{L^2} norms for each functional variable.
#' @export
#' @examples
#' library(funcharts)
#' mfdobj <- data_sim_mfd()
#' norm.mfd(mfdobj)
#'
norm.mfd <- function(mfdobj) {

  if (!(is.mfd(mfdobj))) {
    stop("Input is not a multivariate functional data object.")
  }

  inprods <- inprod_mfd_diag(mfdobj)
  sqrt(rowSums(inprods))

}



#' Inner product of two multivariate functional data objects,
#' for each observation
#'
#' @param mfdobj1 A multivariate functional data object of class \code{mfd}.
#' @param mfdobj2 A multivariate functional data object of class \code{mfd},
#' with the same number of functional variables and observations
#' as \code{mfdobj1}.
#' If NULL, then \code{mfdobj2=mfdobj1}. Default is NULL.
#'
#' @return It calculates the inner product of two
#'  multivariate functional data objects.
#' The main function \code{inprod} of the package \code{fda}
#' calculates inner products among
#' all possible couples of observations.
#' This means that, if \code{mfdobj1} has \code{n1} observations
#' and \code{mfdobj2} has \code{n2} observations,
#' then for each variable \code{n1 X n2} inner products are calculated.
#' However, often one is interested only in calculating
#' the \code{n} inner products
#' between the \code{n} observations of \code{mfdobj1} and
#' the corresponding \code{n}
#' observations of \code{mfdobj2}. This function provides
#' this "diagonal" inner products only,
#' saving a lot of computation with respect to using
#' \code{fda::inprod} and then extracting the
#' diagonal elements.
#' Note that the code of this function calls a modified version
#' of \code{fda::inprod()}.
#' @export
#'
#' @examples
#' mfdobj <- data_sim_mfd()
#' inprod_mfd_diag(mfdobj)
#'
inprod_mfd_diag <- function(mfdobj1, mfdobj2 = NULL) {

  if (!is.mfd(mfdobj1)) {
    stop("Only mfd class allowed for mfdobj1 input")
  }

  if (!(fda::is.fd(mfdobj2) | is.null(mfdobj2))) {
    stop("mfdobj2 input must be of class mfd, or NULL")
  }

  nvar1 <- dim(mfdobj1$coefs)[3]
  nobs1 <- dim(mfdobj1$coefs)[2]

  if (fda::is.fd(mfdobj2)) {
    nvar2 <- dim(mfdobj2$coefs)[3]
    if (nvar1 != nvar2) {
      stop("mfdobj1 and mfdobj2 must have the same number of variables")
    }
    nobs2 <- dim(mfdobj2$coefs)[2]
    if (nobs1 != nobs2) {
      stop("mfdobj1 and mfdobj2 must have the same number of observations")
    }
  }

  if (is.null(mfdobj2)) mfdobj2 <- mfdobj1

  bs1 <- mfdobj1$basis
  bs2 <- mfdobj2$basis

  if (identical(bs1, bs2)) {
    if (bs1$type == "fourier") {
      inprods <- vapply(seq_len(nvar1), function(jj) {
        C1jj <- mfdobj1$coefs[, , jj]
        C2jj <- mfdobj2$coefs[, , jj]
        colSums(as.matrix(C1jj * C2jj))
      }, numeric(dim(mfdobj1$coefs)[2]))
    }
    if (bs1$type %in% c("bspline", "expon", "monom", "polygonal", "power")) {
      inprods <- vapply(seq_len(nvar1), function(jj) {
        C1jj <- mfdobj1$coefs[, , jj]
        C2jj <- mfdobj2$coefs[, , jj]
        rowSums(as.matrix(t(C1jj) %*% bs1$B * t(C2jj)))
      }, numeric(dim(mfdobj1$coefs)[2]))
    }
    if (bs1$type == "const") {
      inprods <- vapply(seq_len(nvar1), function(jj) {
        C1jj <- mfdobj1$coefs[, , jj]
        C2jj <- mfdobj2$coefs[, , jj]
        t(C1jj) %*% C2jj * diff(bs1$rangeval)
        as.numeric(C1jj * C2jj * diff(bs1$rangeval))
      }, numeric(dim(mfdobj1$coefs)[2]))
    }
  } else {
    inprods <- vapply(seq_len(nvar1), function(jj) {

      fdobj1_jj <- fda::fd(matrix(mfdobj1$coefs[, , jj],
                                  nrow = dim(mfdobj1$coefs)[1],
                                  ncol = dim(mfdobj1$coefs)[2]),
                           bs1)

      fdobj2_jj <- fda::fd(matrix(mfdobj2$coefs[, , jj],
                                  nrow = dim(mfdobj2$coefs)[1],
                                  ncol = dim(mfdobj2$coefs)[2]),
                           bs2)
      out <- inprod_fd_diag_single(fdobj1_jj, fdobj2_jj)
    }, numeric(dim(mfdobj1$coefs)[2]))
  }

  if (nobs1 == 1) inprods <- matrix(inprods, nrow = 1)
  inprods

}

#' @noRd
#'
inprod_fd_diag_single <- function(fdobj1, fdobj2 = NULL) {

  if (!fda::is.fd(fdobj1)) {
    stop("Only fd class allowed for fdobj1 input")
  }

  nrep1 <- dim(fdobj1$coefs)[2]
  coef1 <- fdobj1$coefs
  basisobj1 <- fdobj1$basis
  type1 <- basisobj1$type
  range1 <- basisobj1$rangeval

  if (!(fda::is.fd(fdobj2) | is.null(fdobj2))) {
    stop("fdobj2 input must be of class fd, or NULL")
  }

  if (fda::is.fd(fdobj2)) {
    type_calculated <- "inner_product"
    nrep2 <- dim(fdobj2$coefs)[2]
    if (nrep1 != nrep2) {
      stop("fdobj1 and fdobj2 must have the same number of observations")
    }
    coef2 <- fdobj2$coefs
    basisobj2 <- fdobj2$basis
    type2 <- basisobj2$type
    range2 <- basisobj2$rangeval
    if (!all(range1 == range2)) {
      stop("fdobj1 and fdobj2 must have the same domain")
    }
  }

  if (is.null(fdobj2)) {
    type_calculated <- "norm"
    nrep2 <- nrep1
    coef2 <- coef1
    basisobj2 <- basisobj1
    type2 <- type1
    range2 <- range1
  }

  iter <- 0
  rngvec <- range1
  if ((all(c(coef1) == 0) || all(c(coef2) == 0))) {
    return(numeric(nrep1))
  }

  JMAX <- 25
  JMIN <- 5
  EPS <- 1e-6

  inprodmat <- numeric(nrep1)
  nrng <- length(rngvec)
  for (irng in 2:nrng) {
    rngi <- c(rngvec[irng - 1], rngvec[irng])
    if (irng > 2)
      rngi[1] <- rngi[1] + 1e-10
    if (irng < nrng)
      rngi[2] <- rngi[2] - 1e-10
    iter <- 1
    width <- rngi[2] - rngi[1]
    JMAXP <- JMAX + 1
    h <- rep(1, JMAXP)
    h[2] <- 0.25
    s <- vector(mode = "list", length = JMAXP)
    fx1 <- fda::eval.fd(rngi, fdobj1)
    if (type_calculated == "inner_product") {
      fx2 <- fda::eval.fd(rngi, fdobj2)
      s[[1]] <- width * colSums(fx1 * fx2) / 2
    }
    if (type_calculated == "norm") {
      s[[1]] <- width * colSums(fx1^2) / 2
    }
    tnm <- 0.5
    for (iter in 2:JMAX) {
      tnm <- tnm * 2
      if (iter == 2) {
        x <- mean(rngi)
      } else {
        del <- width/tnm
        x <- seq(rngi[1] + del/2, rngi[2] - del/2, del)
      }
      fx1 <- fda::eval.fd(x, fdobj1)

      if (type_calculated == "inner_product") {
        fx2 <- fda::eval.fd(x, fdobj2)
        chs <- width * colSums(fx1 * fx2) / tnm
      }
      if (type_calculated == "norm") {
        chs <- width * colSums(fx1^2) / tnm
      }

      s[[iter]] <- (s[[iter - 1]] + chs) / 2

      if (iter >= 5) {
        ind <- (iter - 4):iter
        ya <- s[ind]
        xa <- h[ind]
        absxa <- abs(xa)
        absxamin <- min(absxa)
        ns <- min((seq_along(absxa))[absxa == absxamin])
        cs <- ya
        ds <- ya
        y <- ya[[ns]]
        ns <- ns - 1
        for (m in 1:4) {
          for (i in 1:(5 - m)) {
            ho <- xa[i]
            hp <- xa[i + m]
            w <- (cs[[i + 1]] - ds[[i]])/(ho - hp)
            ds[[i]] <- hp * w
            cs[[i]] <- ho * w
          }
          if (2 * ns < 5 - m) {
            dy <- cs[[ns + 1]]
          } else {
            dy <- ds[[ns]]
            ns <- ns - 1
          }
          y <- y + dy
        }
        ss <- y
        errval <- max(abs(dy))
        ssqval <- max(abs(ss))
        if (all(ssqval > 0)) {
          crit <- errval/ssqval
        } else {
          crit <- errval
        }
        if (crit < EPS && iter >= JMIN) break
      }
      s[[iter + 1]] <- s[[iter]]
      h[iter + 1] <- 0.25 * h[iter]
      if (iter == JMAX)
        warning("Failure to converge.")
    }
    inprodmat <- inprodmat + ss
  }
  names(inprodmat) <- NULL
  inprodmat
}

#' @noRd
#'
inprod_fd <- function (fdobj1,
                       fdobj2 = NULL,
                       Lfdobj1 = fda::int2Lfd(0),
                       Lfdobj2 = fda::int2Lfd(0),
                       rng = range1, wtfd = 0) {
  result1 <- fdchk(fdobj1)
  nrep1 <- result1[[1]]
  fdobj1 <- result1[[2]]
  coef1 <- fdobj1$coefs
  basisobj1 <- fdobj1$basis
  type1 <- basisobj1$type
  range1 <- basisobj1$rangeval
  if (is.null(fdobj2)) {
    tempfd <- fdobj1
    tempbasis <- tempfd$basis
    temptype <- tempbasis$type
    temprng <- tempbasis$rangeval
    if (temptype == "bspline") {
      basis2 <- fda::create.bspline.basis(temprng, 1, 1)
    }
    else {
      if (temptype == "fourier")
        basis2 <- fda::create.fourier.basis(temprng, 1)
      else basis2 <- fda::create.constant.basis(temprng)
    }
    fdobj2 <- fda::fd(1, basis2)
  }
  result2 <- fdchk(fdobj2)
  nrep2 <- result2[[1]]
  fdobj2 <- result2[[2]]
  coef2 <- fdobj2$coefs
  basisobj2 <- fdobj2$basis
  type2 <- basisobj2$type
  range2 <- basisobj2$rangeval
  if (rng[1] < range1[1] || rng[2] > range1[2])
    stop("Limits of integration are inadmissible.")
  if (fda::is.fd(fdobj1) && fda::is.fd(fdobj2) && type1 == "bspline" &&
      type2 == "bspline" && is.eqbasis(basisobj1, basisobj2) &&
      is.integer(Lfdobj1) && is.integer(Lfdobj2) &&
      length(basisobj1$dropind) ==
      0 && length(basisobj1$dropind) == 0 && wtfd == 0 &&
      all(rng == range1)) {
    inprodmat <- fda::inprod.bspline(fdobj1,
                                     fdobj2,
                                     Lfdobj1$nderiv,
                                     Lfdobj2$nderiv)
    return(inprodmat)
  }
  Lfdobj1 <- fda::int2Lfd(Lfdobj1)
  Lfdobj2 <- fda::int2Lfd(Lfdobj2)
  iter <- 0
  rngvec <- rng
  knotmult <- numeric(0)
  if (type1 == "bspline")
    knotmult <- knotmultchk(basisobj1, knotmult)
  if (type2 == "bspline")
    knotmult <- knotmultchk(basisobj2, knotmult)
  if (length(knotmult) > 0) {
    knotmult <- sort(unique(knotmult))
    knotmult <- knotmult[knotmult > rng[1] && knotmult <
                           rng[2]]
    rngvec <- c(rng[1], knotmult, rng[2])
  }
  if ((all(c(coef1) == 0) || all(c(coef2) == 0)))
    return(matrix(0, nrep1, nrep2))
  JMAX <- 25
  JMIN <- 5
  EPS <- 1e-06
  inprodmat <- matrix(0, nrep1, nrep2)
  nrng <- length(rngvec)
  for (irng in 2:nrng) {
    rngi <- c(rngvec[irng - 1], rngvec[irng])
    if (irng > 2)
      rngi[1] <- rngi[1] + 1e-10
    if (irng < nrng)
      rngi[2] <- rngi[2] - 1e-10
    iter <- 1
    width <- rngi[2] - rngi[1]
    JMAXP <- JMAX + 1
    h <- rep(1, JMAXP)
    h[2] <- 0.25
    s <- array(0, c(JMAXP, nrep1, nrep2))
    sdim <- length(dim(s))
    fx1 <- fda::eval.fd(rngi, fdobj1, Lfdobj1)
    fx2 <- fda::eval.fd(rngi, fdobj2, Lfdobj2)
    if (!is.numeric(wtfd)) {
      wtd <- fda::eval.fd(rngi, wtfd, 0)
      fx2 <- matrix(wtd, dim(wtd)[1], dim(fx2)[2]) * fx2
    }
    s[1, , ] <- width * matrix(crossprod(fx1, fx2), nrep1,
                               nrep2)/2
    tnm <- 0.5
    for (iter in 2:JMAX) {
      tnm <- tnm * 2
      if (iter == 2) {
        x <- mean(rngi)
      }
      else {
        del <- width/tnm
        x <- seq(rngi[1] + del/2, rngi[2] - del/2, del)
      }
      fx1 <- fda::eval.fd(x, fdobj1, Lfdobj1)
      fx2 <- fda::eval.fd(x, fdobj2, Lfdobj2)
      if (!is.numeric(wtfd)) {
        wtd <- fda::eval.fd(wtfd, x, 0)
        fx2 <- matrix(wtd, dim(wtd)[1], dim(fx2)[2]) *
          fx2
      }
      chs <- width * matrix(crossprod(fx1, fx2), nrep1,
                            nrep2)/tnm
      s[iter, , ] <- (s[iter - 1, , ] + chs)/2
      if (iter >= 5) {
        ind <- (iter - 4):iter
        ya <- s[ind, , ]
        ya <- array(ya, c(5, nrep1, nrep2))
        xa <- h[ind]
        absxa <- abs(xa)
        absxamin <- min(absxa)
        ns <- min((seq_along(absxa))[absxa == absxamin])
        cs <- ya
        ds <- ya
        y <- ya[ns, , ]
        ns <- ns - 1
        for (m in 1:4) {
          for (i in 1:(5 - m)) {
            ho <- xa[i]
            hp <- xa[i + m]
            w <- (cs[i + 1, , ] - ds[i, , ])/(ho - hp)
            ds[i, , ] <- hp * w
            cs[i, , ] <- ho * w
          }
          if (2 * ns < 5 - m) {
            dy <- cs[ns + 1, , ]
          }
          else {
            dy <- ds[ns, , ]
            ns <- ns - 1
          }
          y <- y + dy
        }
        ss <- y
        errval <- max(abs(dy))
        ssqval <- max(abs(ss))
        if (all(ssqval > 0)) {
          crit <- errval/ssqval
        }
        else {
          crit <- errval
        }
        if (crit < EPS && iter >= JMIN)
          break
      }
      s[iter + 1, , ] <- s[iter, , ]
      h[iter + 1] <- 0.25 * h[iter]
      if (iter == JMAX)
        warning("Failure to converge.")
    }
    inprodmat <- inprodmat + ss
  }
  if (length(dim(inprodmat) == 2)) {
    return(as.matrix(inprodmat))
  }
  else {
    return(inprodmat)
  }
}


#' @noRd
#'
fdchk <- function (fdobj) {
  if (inherits(fdobj, "fd")) {
    coef <- fdobj$coefs
  }
  else {
    if (inherits(fdobj, "basisfd")) {
      coef <- diag(rep(1, fdobj$nbasis - length(fdobj$dropind)))
      fdobj <- fda::fd(coef, fdobj)
    }
    else {
      stop("FDOBJ is not an FD object.")
    }
  }
  coefd <- dim(as.matrix(coef))
  if (length(coefd) > 2)
    stop("Functional data object must be univariate")
  nrep <- coefd[2]
  basisobj <- fdobj$basis
  return(list(nrep, fdobj))
}

#' @noRd
#'
knotmultchk <- function (basisobj, knotmult) {
  type <- basisobj$type
  if (type == "bspline") {
    params <- basisobj$params
    nparams <- length(params)
    norder <- basisobj$nbasis - nparams
    if (norder == 1) {
      knotmult <- c(knotmult, params)
    }
    else {
      if (nparams > 1) {
        for (i in 2:nparams) if (params[i] == params[i -
                                                     1])
          knotmult <- c(knotmult, params[i])
      }
    }
  }
  return(knotmult)
}

#' @noRd
#'
is.eqbasis <- function (basisobj1, basisobj2) {
  eqwrd <- TRUE
  if (basisobj1$type != basisobj2$type) {
    eqwrd <- FALSE
    return(eqwrd)
  }
  if (any(basisobj1$rangeval != basisobj2$rangeval)) {
    eqwrd <- FALSE
    return(eqwrd)
  }
  if (basisobj1$nbasis != basisobj2$nbasis) {
    eqwrd <- FALSE
    return(eqwrd)
  }
  if (any(basisobj1$params != basisobj2$params)) {
    eqwrd <- FALSE
    return(eqwrd)
  }
  if (any(basisobj1$dropind != basisobj2$dropind)) {
    eqwrd <- FALSE
    return(eqwrd)
  }
  return(eqwrd)
}


#' Get Multivariate Functional Data from a data frame
#'
#' @param dt
#' A \code{data.frame} containing the discrete data.
#' For each functional variable, a single column,
#' whose name is provided in the argument \code{variables},
#' contains discrete values of that variable for all functional observation.
#' The column indicated by the argument \code{id}
#' denotes which is the functional observation in each row.
#' The column indicated by the argument \code{arg}
#' gives the argument value at which
#' the discrete values of the functional variables are observed for each row.
#' @param domain
#' A numeric vector of length 2 defining
#' the interval over which the functional data object
#' can be evaluated.
#' @param arg
#' A character variable, which is the name of
#' the column of the data frame \code{dt}
#' giving the argument values at which the functional variables
#' are evaluated for each row.
#' @param id
#' A character variable indicating
#' which is the functional observation in each row.
#' @param variables
#' A vector of characters of the column names
#' of the data frame \code{dt}
#' indicating the functional variables.
#' @param n_basis
#' An integer variable specifying the number of basis functions;
#' default value is 30.
#' See details on basis functions.
#' @param n_order
#' An integer specifying the order of b-splines,
#' which is one higher than their degree.
#' The default of 4 gives cubic splines.
#' @param basisobj
#' An object of class \code{basisfd} defining
#' the basis function expansion.
#' Default is \code{NULL}, which means that
#' a \code{basisfd} object is created by doing
#' \code{create.bspline.basis(rangeval = domain,
#' nbasis = n_basis,  norder = n_order)}
#' @param Lfdobj
#' An object of class \code{Lfd} defining a
#' linear differential operator of order m.
#' It is used to specify a roughness penalty through \code{fdPar}.
#' Alternatively, a nonnegative integer
#' specifying the order m can be given and is
#' passed as \code{Lfdobj} argument to the function \code{fdPar},
#' which indicates that the derivative of order m is penalized.
#' Default value is 2, which means that the
#' integrated squared second derivative is penalized.
#' @param lambda
#' A non-negative real number.
#' If you want to use a single specified smoothing parameter
#' for all functional data objects in the dataset,
#' this argument is passed to the function \code{fda::fdPar}.
#' Default value is NULL, in this case the smoothing parameter is chosen
#' by minimizing the generalized cross-validation (GCV)
#' criterion over the grid of values given by the argument.
#' See details on how smoothing parameters work.
#' @param lambda_grid
#' A vector of non-negative real numbers.
#' If \code{lambda} is provided as a single number, this argument is ignored.
#' If \code{lambda} is NULL, then this provides the grid of values
#' over which the optimal smoothing parameter is
#' searched. Default value is \code{10^seq(-10,1,l=20)}.
#' @param ncores
#' If you want parallelization, give the number of cores/threads
#' to be used when doing GCV separately on all observations.
#'
#' @details
#' Basis functions are created with
#' \code{fda::create.bspline.basis(domain, n_basis)}, i.e.
#' B-spline basis functions of order 4 with equally spaced knots
#' are used to create \code{mfd} objects.
#'
#' The smoothing penalty lambda is provided as
#' \code{fda::fdPar(bs, 2, lambda)},
#' where bs is the basis object and 2 indicates
#' that the integrated squared second derivative is penalized.
#'
#' Rather than having a data frame with long format,
#' i.e. with all functional observations in a single column
#' for each functional variable,
#' if all functional observations are observed on a common equally spaced grid,
#' discrete data may be available in matrix form for each functional variable.
#' In this case, see \code{get_mfd_list}.
#'
#' @seealso \code{\link{get_mfd_list}}
#'
#' @return An object of class \code{mfd}.
#' See also \code{?mfd} for additional details on the
#' multivariate functional data class.
#' @export
#' @examples
#' library(funcharts)
#'
#' x <- seq(1, 10, length = 25)
#' y11 <- cos(x)
#' y21 <- cos(2 * x)
#' y12 <- sin(x)
#' y22 <- sin(2 * x)
#' df <- data.frame(id = factor(rep(1:2, each = length(x))),
#'                  x = rep(x, times = 2),
#'                  y1 = c(y11, y21),
#'                  y2 = c(y12, y22))
#'
#' mfdobj <- get_mfd_df(dt = df,
#'                      domain = c(1, 10),
#'                      arg = "x",
#'                      id = "id",
#'                      variables = c("y1", "y2"),
#'                      lambda = 1e-5)
#'
get_mfd_df <- function(dt,
                       domain,
                       arg,
                       id,
                       variables,
                       n_basis = 30,
                       n_order = 4,
                       basisobj = NULL,
                       Lfdobj = 2,
                       lambda = NULL,
                       lambda_grid = 10^seq(-10, 1, length.out = 10),
                       ncores = 1) {

  if (!(is.data.frame(dt))) {
    stop("dt must be a data.frame")
  }
  if (!(arg %in% names(dt))) {
    stop("domain_var must be the name of a column of dt")
  }
  if (!(id %in% names(dt))) {
    stop("id must be the name of a column of dt")
  }
  if (sum(variables %in% names(dt)) < length(variables)) {
    stop("variables must contain only names of columns of dt")
  }
  if (!is.numeric(domain) | length(domain) != 2) {
    stop("domain must be a vector with two numbers.")
  }
  if (!is.null(basisobj) & !fda::is.basis(basisobj)) {
    stop("basisobj must be NULL or a basisfd object")
  }
  if (!is.null(basisobj) & !all(basisobj$rangeval == domain)) {
    stop("if basisobj is provided, basisobj$rangeval must be equal to domain")
  }
  if (!is.null(Lfdobj) & !fda::is.Lfd(Lfdobj)) {
    if (is.numeric(Lfdobj)) {
      if (Lfdobj != abs(round(Lfdobj))) {
        stop("Lfdobj must be a positive integer or a Lfd object")
      }
    } else {
      stop("Lfdobj must be a positive integer or a Lfd object")
    }
  }

  if (fda::is.basis(basisobj)) {
    message("basisobj is provided, n_basis and n_order are ignored")
    n_basis <- basisobj$nbasis
  }
  if (is.null(basisobj)) {
    basisobj <- fda::create.bspline.basis(rangeval = domain,
                                          nbasis = n_basis,
                                          norder = n_order)
  }

  ids <- levels(factor(dt[[id]]))
  n_obs <- length(ids)
  n_var <- length(variables)

  lambda_search <- if (!is.null(lambda)) lambda else lambda_grid

  n_lam <- length(lambda_search)
  ncores <- min(ncores, n_obs)

  fun_gcv <- function(ii) {

    rows_i <- dt[[id]] == ids[ii] &
      dt[[arg]] >= domain[1] &
      dt[[arg]] <= domain[2]
    dt_i <- dt[rows_i, , drop = FALSE]

    x <- dt_i[[arg]]
    y <- data.matrix(dt_i[, variables])

    # Generalized cross validation to choose smoothing parameter
    coefList <- list()
    gcv <- matrix(data=NA,n_var,n_lam)
    rownames(gcv) <- variables
    colnames(gcv) <- lambda_search
    for (h in seq_len(n_lam)) {
      fdpenalty <- fda::fdPar(basisobj, Lfdobj, lambda_search[h])
      smoothObj <- fda::smooth.basis(x, y, fdpenalty)
      coefList[[h]] <- smoothObj$fd$coefs
      gcv[, h] <- smoothObj$gcv

    }

    # If only NA in a row,
    # consider as optimal smoothing parameter the first one in the sequence.
    gcvmin <- apply(gcv, 1, function(x) {
      if(sum(is.na(x)) < n_lam) which.min(x) else 1
    })
    opt_lam <- lambda_search[gcvmin]
    names(opt_lam) <- variables

    coefs <- vapply(seq_len(n_var),
                    function(jj) coefList[[gcvmin[jj]]][, jj],
                    numeric(n_basis))
    if (basisobj$nbasis == 1) coefs <- matrix(as.numeric(coefs), nrow = 1)
    colnames(coefs) <- variables

    coefs
  }

  # You need to perform gcv separately for each observation
  if (ncores == 1) {
    coefs_list <- lapply(seq_len(n_obs), fun_gcv)
  } else {
    if (.Platform$OS.type == "unix") {
      coefs_list <- parallel::mclapply(seq_len(n_obs), fun_gcv, mc.cores = ncores)
    } else {
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(cl,
                              c("dt",
                                "ids",
                                "domain",
                                "variables",
                                "n_var",
                                "n_lam",
                                "lambda_search"),
                              envir = environment())
      coefs_list <- parallel::parLapply(cl, seq_len(n_obs), fun_gcv)
      parallel::stopCluster(cl)
    }
  }

  names(coefs_list) <- ids

  coefs <- array(do.call(rbind, coefs_list),
                 dim = c(basisobj$nbasis, n_obs, n_var),
                 dimnames = list(basisobj$names, ids, variables))

  fdObj <- mfd(coefs, basisobj, list(arg, ids, variables),
               dt[, c(id, arg, variables)], id)
  fdObj

}

#' Get Multivariate Functional Data from a list of matrices
#'
#' @param data_list
#' A named list of matrices.
#' Names of the elements in the list denote the functional variable names.
#' Each matrix in the list corresponds to a functional variable.
#' All matrices must have the same dimension, where
#' the number of rows corresponds to replications, while
#' the number of columns corresponds to the argument values at which
#' functions are evaluated.
#' @param grid
#' A numeric vector, containing the argument values at which
#' functions are evaluated.
#' Its length must be equal to the number of columns
#' in each matrix in data_list.
#' Default is NULL, in this case a vector equally spaced numbers
#' between 0 and 1 is created,
#' with as many numbers as the number of columns in each matrix in data_list.
#' @param n_basis
#' An integer variable specifying the number of basis functions;
#' default value is 30.
#' See details on basis functions.
#' @param n_order
#' An integer specifying the order of B-splines,
#' which is one higher than their degree.
#' The default of 4 gives cubic splines.
#' @param basisobj
#' An object of class \code{basisfd} defining the
#' B-spline basis function expansion.
#' Default is \code{NULL}, which means that
#' a \code{basisfd} object is created by doing
#' \code{create.bspline.basis(rangeval = domain,
#' nbasis = n_basis,  norder = n_order)}
#' @param Lfdobj
#' An object of class \code{Lfd} defining a linear
#' differential operator of order m.
#' It is used to specify a roughness penalty through \code{fdPar}.
#' Alternatively, a nonnegative integer specifying
#' the order m can be given and is
#' passed as \code{Lfdobj} argument to the function \code{fdPar},
#' which indicates that the derivative of order m is penalized.
#' Default value is 2, which means that the integrated
#' squared second derivative is penalized.
#' @param lambda
#' A non-negative real number.
#' If you want to use a single specified smoothing parameter
#' for all functional data objects in the dataset,
#' this argument is passed to the function \code{fda::fdPar}.
#' Default value is NULL, in this case the smoothing parameter is chosen
#' by minimizing the generalized cross-validation (GCV) criterion
#' over the grid of values given by the argument.
#' See details on how smoothing parameters work.
#' @param lambda_grid
#' A vector of non-negative real numbers.
#' If \code{lambda} is provided as a single number, this argument is ignored.
#' If \code{lambda} is NULL, then this provides
#' the grid of values over which the optimal smoothing parameter is
#' searched. Default value is \code{10^seq(-10,1,l=20)}.
#' @param ncores
#' Deprecated.
#'
#'
#' @details
#' Basis functions are created with
#' \code{fda::create.bspline.basis(domain, n_basis)}, i.e.
#' B-spline basis functions of order 4 with equally spaced knots
#' are used to create \code{mfd} objects.
#'
#' The smoothing penalty lambda is provided as
#' \code{fda::fdPar(bs, 2, lambda)},
#' where bs is the basis object and 2 indicates that
#' the integrated squared second derivative is penalized.
#'
#' Rather than having a list of matrices,
#' you may have a data frame with long format,
#' i.e. with all functional observations in a single column
#' for each functional variable.
#' In this case, see \code{get_mfd_df}.
#'
#' @return
#' An object of class \code{mfd}.
#' See also \code{\link{mfd}} for additional details
#' on the multivariate functional data class.
#'
#' @seealso \code{\link{mfd}},
#' \code{\link{get_mfd_list}},
#' \code{\link{get_mfd_array}}
#'
#' @export
#'
#' @examples
#' library(funcharts)
#' data("air")
#' # Only take first 5 multivariate functional observations
#' # and only two variables from air
#' air_small <- lapply(air[c("NO2", "CO")], function(x) x[1:5, ])
#' mfdobj <- get_mfd_list(data_list = air_small)
#'
get_mfd_list <- function(data_list,
                         grid = NULL,
                         n_basis = 30,
                         n_order = 4,
                         basisobj = NULL,
                         Lfdobj = 2,
                         lambda = NULL,
                         lambda_grid = 10^seq(-10, 1, length.out = 10),
                         ncores = 1) {

  if (!missing(ncores)) {
    warning("argument ncores is deprecated.", call. = FALSE)
  }
  if (!(is.list(data_list))) {
    stop("data_list must be a list of matrices")
  }
  if (is.null(names(data_list))) {
    stop("data_list must be a named list")
  }
  if (length(unique(lapply(data_list, dim))) > 1) {
    stop("data_list must be a list of matrices all of the same dimensions")
  }
  n_args <- ncol(data_list[[1]])
  if (!is.null(grid) & (length(grid) != n_args)) {
    stop(paste0(
      "grid length, ", length(grid),
      " has not the same length as number of ",
      "observed data per functional observation, ",
      ncol(data_list[[1]])))
  }

  data_array <- simplify2array(data_list)

  get_mfd_array(data_array = aperm(data_array, c(2, 1, 3)),
                grid = grid,
                n_basis = n_basis,
                n_order = n_order,
                basisobj = basisobj,
                Lfdobj = Lfdobj,
                lambda = lambda,
                lambda_grid = lambda_grid)

}



#' Get Multivariate Functional Data from a three-dimensional array
#'
#'
#' @param data_array
#' A three-dimensional array.
#' The first dimension corresponds to argument values,
#' the second to replications,
#' and the third to variables within replications.
#' @param grid
#' See \code{\link{get_mfd_list}}.
#' @param n_basis
#' See \code{\link{get_mfd_list}}.
#' @param n_order
#' #' See \code{\link{get_mfd_list}}.
#' @param basisobj
#' #' See \code{\link{get_mfd_list}}.
#' @param Lfdobj
#' #' See \code{\link{get_mfd_list}}.
#' @param lambda
#' See \code{\link{get_mfd_list}}.
#' @param lambda_grid
#' See \code{\link{get_mfd_list}}.
#' @param ncores
#' Deprecated. See \code{\link{get_mfd_list}}.
#'
#' @return
#' An object of class \code{mfd}.
#' See also \code{?mfd} for additional details on the
#' multivariate functional data class.
#' @export
#' @seealso
#' \code{\link{get_mfd_list}}, \code{\link{get_mfd_df}}
#'
#' @examples
#' library(funcharts)
#' library(fda)
#' data("CanadianWeather")
#' mfdobj <- get_mfd_array(CanadianWeather$dailyAv[, 1:10, ],
#'                         lambda = 1e-5)
#' plot_mfd(mfdobj)
#'
get_mfd_array <- function(data_array,
                          grid = NULL,
                          n_basis = 30,
                          n_order = 4,
                          basisobj = NULL,
                          Lfdobj = 2,
                          lambda = NULL,
                          lambda_grid = 10^seq(- 10, 1, length.out = 10),
                          ncores = 1) {

  name <- value <- id <- NULL

  if (!missing(ncores)) {
    warning("argument ncores is deprecated.", call. = FALSE)
  }
  n_var <- dim(data_array)[3]
  n_args <- dim(data_array)[1]
  if (!is.null(grid) & (length(grid) != n_args)) {
    stop(paste0(
      "grid length, ", length(grid),
      " has not the same length as number of ",
      "observed data per functional observation, ",
      dim(data_array)[1]))
  }
  if (is.null(grid)) grid <- seq(0, 1, l = n_args)
  domain <- range(grid)

  if (!is.null(basisobj) & !fda::is.basis(basisobj)) {
    stop("basisobj must be NULL or a basisfd object")
  }
  if (!is.null(basisobj) & !all(basisobj$rangeval == domain)) {
    stop("if basisobj is provided, basisobj$rangeval must be equal to domain")
  }
  if (!is.null(Lfdobj) & !fda::is.Lfd(Lfdobj)) {
    if (is.numeric(Lfdobj)) {
      if (Lfdobj != abs(round(Lfdobj))) {
        stop("Lfdobj must be a positive integer or a Lfd object")
      }
    } else {
      stop("Lfdobj must be a positive integer or a Lfd object")
    }
  }

  if (fda::is.basis(basisobj)) {
    if (!(basisobj$type %in% c("bspline", "fourier", "const"))) {
      stop("basisobj supported types are only bspline and fourier and constant")
    }
    message("basisobj is provided, n_basis and n_order are ignored")
    n_basis <- basisobj$nbasis
  }

  if (is.null(basisobj)) {
    basisobj <- fda::create.bspline.basis(rangeval = domain,
                                          nbasis = n_basis,
                                          norder = n_order)
  }

  variables <- dimnames(data_array)[[3]]
  ids <- dimnames(data_array)[[2]]
  if (is.null(ids)) ids <- as.character(seq_len(dim(data_array)[[2]]))
  n_obs <- length(ids)

  lambda_search <- if (!is.null(lambda)) lambda else lambda_grid
  n_lam <- length(lambda_search)

  coefList <- list()
  gcv <- array(data = NA, dim = c(n_obs, n_var, n_lam))

  dimnames(gcv)[[1]] <- ids
  dimnames(gcv)[[2]] <- variables
  dimnames(gcv)[[3]] <- lambda_search
  for (h in seq_len(n_lam)) {
    fdpenalty <- fda::fdPar(basisobj, Lfdobj, lambda_search[h])
    smoothObj <- fda::smooth.basis(grid, data_array, fdpenalty)
    cc <- smoothObj$fd$coefs
    if (n_var == 1) {
      cc <- array(cc, dim = c(dim(cc)[1], dim(cc)[2], 1))
    }
    coefList[[h]] <- cc
    gcv[, , h] <- smoothObj$gcv
  }

  # If only NA in a row,
  # consider as optimal smoothing parameter the first one in the sequence.
  gcvmin <- apply(gcv, 1:2, function(x) {
    if(sum(is.na(x)) < n_lam) which.min(x) else 1
  })


  coef <- array(NA, dim = c(n_basis, n_obs, n_var))
  dimnames(coef)[[1]] <- as.character(basisobj$names)
  dimnames(coef)[[2]] <- as.character(ids)
  dimnames(coef)[[3]] <- as.character(variables)
  for (ii in seq_len(n_obs)) {
    for (jj in seq_len(n_var)) {
      coef[, ii, jj] <- coefList[[gcvmin[ii, jj]]][, ii, jj]
    }
  }

  df_raw <- dplyr::bind_cols(
    data.frame(id = rep(ids, each = n_args)),
    data.frame(t = rep(grid, n_obs)),
    lapply(seq_len(dim(data_array)[3]), function(ii) {
      data_array[, , ii] %>%
        as.data.frame() %>%
        stats::setNames(ids) %>%
        tidyr::pivot_longer(dplyr::everything()) %>%
        dplyr::mutate(name = factor(name, levels = ids)) %>%
        dplyr::arrange(name) %>%
        dplyr::select(value) %>%
        stats::setNames(variables[ii])
    }) %>%
      dplyr::bind_cols()
  ) %>%
    dplyr::mutate(id = factor(id, levels = ids))

  fdObj <- mfd(coef = coef,
               basisobj = basisobj,
               fdnames = list("t",
                              as.character(ids),
                              as.character(variables)),
               raw = df_raw,
               id_var = "id")
  fdObj

}



#' Convert a \code{fd} object into a Multivariate Functional Data object.
#'
#'
#' @param fdobj
#' An object of class fd.
#'
#' @return
#' An object of class \code{mfd}.
#' See also \code{?mfd} for additional details on the
#' multivariate functional data class.
#' @export
#' @seealso
#' \code{mfd}
#'
#' @examples
#' library(funcharts)
#' library(fda)
#' bs <- create.bspline.basis(nbasis = 10)
#' fdobj <- fd(coef = 1:10, basisobj = bs)
#' mfdobj <- get_mfd_fd(fdobj)
get_mfd_fd <- function(fdobj) {

  if (length(fdobj$fdnames[[1]]) > 1) fdobj$fdnames[[1]] <- "time"

  if (!fda::is.fd(fdobj)) {
    stop("fdobj must be an object of class fd.")
  }
  coefs <- fdobj$coefs
  if (length(dim(coefs)) == 2) {
    if (!(identical(colnames(coefs), fdobj$fdnames[[2]]) |
        identical(colnames(coefs), fdobj$fdnames[[3]]))) {
      stop(paste0("colnames(fdobj$coefs) must correspond either to ",
                  "fdobj$fdnames[[2]] (i.e. replication names) or to ",
                  "fdobj$fdnames[[3]] (i.e. variable names)."))
    }
    if (identical(colnames(coefs), fdobj$fdnames[[2]])) {
      coefs <- array(coefs, dim = c(nrow(coefs), ncol(coefs), 1))
      dimnames(coefs) <- list()
      dimnames(coefs)[[1]] <- dimnames(fdobj$coefs)[[1]]
      dimnames(coefs)[[2]] <- fdobj$fdnames[[2]]
      dimnames(coefs)[[3]] <- fdobj$fdnames[[3]]
    }
    if (identical(colnames(coefs), fdobj$fdnames[[3]])) {
      coefs <- array(coefs, dim = c(nrow(coefs), 1, ncol(coefs)))
      dimnames(coefs) <- list()
      dimnames(coefs)[[1]] <- dimnames(fdobj$coefs)[[1]]
      dimnames(coefs)[[2]] <- fdobj$fdnames[[2]]
      dimnames(coefs)[[3]] <- fdobj$fdnames[[3]]
    }
  }
  bs <- fdobj$basis
  mfd(coefs,
      fdobj$basis,
      fdobj$fdnames)
}

#' Number of observations in a multivariate functional data object
#'
#' @param object An object of class `mfd`.
#' @param ... Further arguments passed to methods (not used).
#' @return An integer: the number of observations.
#' @examples
#' # nobs(mfdobj)
#' @export
#' @importFrom stats nobs
nobs.mfd <- function(object, ...) {
  dim(object$coefs)[2]
}

#' Number of basis functions
#'
#' Generic function to extract the number of basis functions from an object.
#'
#' @param object An object from which to extract the number of basis functions.
#' @param ... Further arguments passed to methods (not used).
#' @export
nbasis <- function(object, ...) {
  UseMethod("nbasis")
}

#' @export
nbasis.mfd <- function(object, ...) {
  dim(object$coefs)[1]
}


#' Number of variables
#'
#' Generic function to extract the number of variables from an object.
#'
#' @param object An object from which to extract the number of variables.
#' @param ... Further arguments passed to methods (not used).
#' @export
nvar <- function(object, ...) {
  UseMethod("nvar")
}

#' @export
nvar.mfd <- function(object, ...) {
  dim(object$coefs)[3]
}




#' Standardize Multivariate Functional Data.
#'
#' Scale multivariate functional data contained
#' in an object of class \code{mfd}
#' by subtracting the mean function and dividing
#' by the standard deviation function.
#'
#' @param mfdobj
#' A multivariate functional data object of class \code{mfd}.
#' @param center
#' A logical value, or a \code{fd} object.
#' When providing a logical value, if TRUE, \code{mfdobj} is centered,
#' i.e. the functional mean function is calculated and subtracted
#' from all observations in \code{mfdobj},
#' if FALSE, \code{mfdobj} is not centered.
#' If \code{center} is a \code{fd} object, then this function
#' is used as functional mean for centering.
#' @param scale
#' A logical value, or a \code{fd} object.
#' When providing a logical value, if TRUE, \code{mfdobj}
#' is scaled after possible centering,
#' i.e. the functional standard deviation is calculated
#' from all functional observations in \code{mfdobj} and
#' then the observations are divided by this calculated standard deviation,
#' if FALSE, \code{mfdobj} is not scaled.
#' If \code{scale} is a \code{fd} object,
#' then this function is used as standard deviation function for scaling.
#'
#' @return
#' A standardized object of class \code{mfd}, with two attributes,
#' if calculated,
#' \code{center} and \code{scale}, storing the mean and
#' standard deviation functions used for standardization.
#'
#' @details
#' This function has been written to work similarly
#' as the function \code{\link{scale}} for matrices.
#' When calculated, attributes \code{center} and \code{scale}
#' are of class \code{fd}
#' and have the same structure you get
#' when you use \code{fda::\link[fda]{mean.fd}}
#' and \code{fda::\link[fda]{sd.fd}}.
#' @export
#' @examples
#' library(funcharts)
#' mfdobj <- data_sim_mfd()
#' mfdobj_scaled <- scale_mfd(mfdobj)
scale_mfd <- function(mfdobj, center = TRUE, scale = TRUE) {

  if (!is.mfd(mfdobj)) {
    stop("Only mfd class allowed for mfdobj input")
  }

  bs <- mfdobj$basis
  n_obs <- length(mfdobj$fdnames[[2]])

  if (n_obs == 1 & (!fda::is.fd(scale) | !fda::is.fd(center))) {
    stop("There is only one observation in the data set")
  }

  # Center
  if (!(is.logical(center) | fda::is.fd(center))) {
    stop("Only logical or fd classes allowed for center input")
  }

  mean_fd <- NULL
  if (is.logical(center) && center == FALSE) {
    cen_fd <- mfdobj
  } else {
    if (is.logical(center) && center == TRUE) {
      mean_fd <- fda::mean.fd(mfdobj)
    }
    if (fda::is.fd(center)) {
      mean_fd <- center
    }
    mean_fd_coefs <- array(mean_fd$coefs[, 1, ],
                           dim = c(dim(mean_fd$coefs)[c(1, 3)], n_obs))
    mean_fd_coefs <- aperm(mean_fd_coefs, c(1, 3, 2))
    mean_fd_rep <- fda::fd(mean_fd_coefs, bs, mfdobj$fdnames)
    cen_fd <- fda::minus.fd(mfdobj, mean_fd_rep)
  }

  # Scale
  if (!(is.logical(scale) | fda::is.fd(scale))) {
    stop("Only logical or fd classes allowed for scale input")
  }

  sd_fd <- NULL
  if (is.logical(scale) && scale == FALSE) {
    fd_std <- cen_fd
  } else {
    if (is.logical(scale) && scale == TRUE) {
      sd_fd <- fda::sd.fd(mfdobj)
    }
    if (fda::is.fd(scale)) {
      sd_fd <- scale
    }
    if (sd_fd$basis$nbasis < 15 | sd_fd$basis$type != "bspline") {
      domain <- mfdobj$basis$rangeval
      x_eval <- seq(domain[1], domain[2], length.out = 1000)
      sd_eval <- fda::eval.fd(evalarg = x_eval, sd_fd)

      bs_sd <- fda::create.bspline.basis(rangeval = domain, nbasis = 20)

      fdpar_more_basis <- fda::fdPar(bs_sd, 2, 0)
      sd_fd_more_basis <- fda::smooth.basis(x_eval, sd_eval,
                                            fdpar_more_basis)$fd
      sd_inv <- sd_fd_more_basis^(-1)
      sd_inv_eval <- fda::eval.fd(evalarg = x_eval, sd_inv)
      sd_inv <- fda::smooth.basis(x_eval, sd_inv_eval, fda::fdPar(bs, 2, 0))$fd

    } else {
      sd_inv <- sd_fd^(-1)
    }
    sd_inv_coefs <- array(sd_inv$coefs, dim = c(dim(sd_inv$coefs), n_obs))
    sd_inv_coefs <- aperm(sd_inv_coefs, c(1, 3, 2))
    sd_inv_rep <- fda::fd(sd_inv_coefs, bs, mfdobj$fdnames)
    fd_std <- fda::times.fd(cen_fd, sd_inv_rep, bs)
    fd_std$fdnames <- mfdobj$fdnames
    dimnames(fd_std$coefs) <- dimnames(mfdobj$coefs)
    dimnames(fd_std$fdnames) <- NULL
  }

  attr(fd_std, "scaled:center") <- mean_fd
  attr(fd_std, "scaled:scale") <- sd_fd

  fd_std$raw <- NULL
  fd_std$id_var <- mfdobj$id_var
  class(fd_std) <- c("mfd", "fd")

  fd_std
}


#' De-standardize a standardized Multivariate Functional Data object
#'
#' This function takes a scaled
#' multivariate functional data contained in an object of class \code{mfd},
#' multiplies it by function provided by the argument \code{scale}
#' and adds the function provided by the argument \code{center}.
#'
#' @param scaled_mfd
#' A scaled multivariate functional data object,
#' of class \code{mfd}.
#' @param center
#' A functional data object of class \code{fd},
#' having the same structure you get
#' when you use \code{fda::\link[fda]{mean.fd}}
#' over a \code{mfd} object.
#' @param scale
#' A functional data object of class \code{fd},
#' having the same structure you get
#' when you use \code{fda::\link[fda]{sd.fd}}
#' over a \code{mfd} object.
#'
#' @return
#' A de-standardized object of class \code{mfd},
#' obtained by multiplying \code{scaled_mfd} by \code{scale} and then
#' adding \code{center}.
#' @noRd
#'
descale_mfd <- function (scaled_mfd, center = FALSE, scale = FALSE) {

  if (!is.mfd(scaled_mfd)) stop("scaled_mfd must be from class mfd")

  basis <- scaled_mfd$basis
  nbasis <- basis$nbasis
  nobs <- length(scaled_mfd$fdnames[[2]])
  nvar <- length(scaled_mfd$fdnames[[3]])

  if (fda::is.fd(scale)) {

    coef_sd_list <- lapply(seq_len(nvar), function(jj) {
      matrix(scale$coefs[, jj], nrow = nbasis, ncol = nobs)
    })
    coef_sd <- simplify2array(coef_sd_list)
    sd_fd <- fda::fd(coef_sd, scaled_mfd$basis)
    centered <- fda::times.fd(scaled_mfd, sd_fd, basisobj = basis)

  } else {
    if (is.logical(scale) & !scale & length(scale) == 1) {
      centered <- scaled_mfd
    } else {
      stop("scale must be either an mfd object or FALSE")
    }
  }

  if (fda::is.fd(center)) {

    descaled_mean_list <- lapply(seq_len(nvar), function(jj) {
      out <- centered$coefs[, , jj, drop = FALSE] + as.numeric(center$coefs[, 1, jj])
      matrix(out, dim(out)[1], dim(out)[2])
    })
    descaled_coef <- simplify2array(descaled_mean_list)

  } else {
    if (is.logical(center) & !center & length(center) == 1) {
      descaled_coef <- centered$coef
    } else {
      stop("center must be either an mfd object or FALSE")
    }
  }

  dimnames(descaled_coef) <- dimnames(scaled_mfd$coefs)

  mfd(descaled_coef,
      scaled_mfd$basis,
      scaled_mfd$fdnames,
      B = scaled_mfd$basis$B)
}


#' Tensor product of two Multivariate Functional Data objects
#'
#' This function returns the tensor product of two
#' Multivariate Functional Data objects.
#' Each object must contain only one replication.
#'
#' @param mfdobj1
#' A multivariate functional data object, of class \code{mfd},
#' having only one functional observation.
#' @param mfdobj2
#' A multivariate functional data object, of class \code{mfd},
#' having only one functional observation.
#' If NULL, it is set equal to \code{mfdobj1}. Default is NULL.
#'
#' @return
#' An object of class \code{bifd}.
#' If we denote with x(s)=(x_1(s),\dots,x_p(s))
#' the vector of p functions represented by \code{mfdobj1} and
#' with y(t)=(y_1(t),\dots,y_q(t)) the vector of q functions
#' represented by \code{mfdobj2},
#' the output is the
#' vector of pq bivariate functions
#'
#' f(s,t)=(x_1(s)y_1(t),\dots,x_1(s)y_q(t),
#' \dots,x_p(s)y_1(t),\dots,x_p(s)y_q(t)).
#'
#'
#' @export
#' @examples
#' library(funcharts)
#' mfdobj1 <- data_sim_mfd(nobs = 1, nvar = 3)
#' mfdobj2 <- data_sim_mfd(nobs = 1, nvar = 2)
#' tensor_product_mfd(mfdobj1)
#' tensor_product_mfd(mfdobj1, mfdobj2)
#'
tensor_product_mfd <- function(mfdobj1, mfdobj2 = NULL) {

  if (!is.mfd(mfdobj1)) {
    stop("First argument must be a mfd object.")
  }
  if (is.null(mfdobj2)) mfdobj2 <- mfdobj1
  if (!is.mfd(mfdobj2)) {
    stop("First argument must be a mfd object.")
  }
  obs1 <- mfdobj1$fdnames[[2]]
  obs2 <- mfdobj2$fdnames[[2]]
  nobs1 <- length(obs1)
  nobs2 <- length(obs2)
  if (nobs1 > 1) {
    stop("mfdobj1 must have only 1 observation")
  }
  if (nobs2 > 1) {
    stop("mfdobj2 must have only 1 observation")
  }
  variables1 <- mfdobj1$fdnames[[3]]
  variables2 <- mfdobj2$fdnames[[3]]
  nvar1 <- length(variables1)
  nvar2 <- length(variables2)

  coef1 <- mfdobj1$coefs
  coef2 <- mfdobj2$coefs
  coef1 <- matrix(coef1, nrow = dim(coef1)[1], ncol = dim(coef1)[3])
  coef2 <- matrix(coef2, nrow = dim(coef2)[1], ncol = dim(coef2)[3])

  basis1 <- mfdobj1$basis
  basis2 <- mfdobj2$basis

  nbasis1 <- basis1$nbasis
  nbasis2 <- basis2$nbasis

  coef <- outer(coef1, coef2)
  coef <- aperm(coef, c(1, 3, 4, 2))
  coef <- array(coef, dim = c(nbasis1, nbasis2, 1, nvar1 * nvar2))

  dim_names <- expand.grid(variables1, variables2)
  variables <- paste(dim_names[, 1], dim_names[, 2])

  obs <- paste0("s.", obs1, " t.", obs2)

  fdnames <- list(basis1$names,
                  basis2$names,
                  obs,
                  variables)

  dimnames(coef) <- fdnames

  fda::bifd(coef, basis1, basis2, fdnames)

}


#' Bind variables of two Multivariate Functional Data Objects
#'
#' @param mfdobj1
#' An object of class mfd, with the same number of replications of mfdobj2
#' and different variable names with respect to mfdobj2.
#' @param mfdobj2
#' An object of class mfd, with the same number of replications of mfdobj1,
#' and different variable names with respect to mfdobj1.
#'
#' @return
#' An object of class mfd, whose replications are the same of mfdobj1 and
#' mfdobj2 and whose functional variables are the union of the functional
#' variables in mfdobj1 and mfdobj2.
#' @export
#'
#' @examples
#' library(funcharts)
#' mfdobj1 <- data_sim_mfd(nvar = 3)
#' mfdobj2 <- data_sim_mfd(nvar = 2)
#' dimnames(mfdobj2$coefs)[[3]] <- mfdobj2$fdnames[[3]] <- c("var10", "var11")
#'
#' plot_mfd(mfdobj1)
#' plot_mfd(mfdobj2)
#' mfdobj_cbind <- cbind_mfd(mfdobj1, mfdobj2)
#' plot_mfd(mfdobj_cbind)
#'
cbind_mfd <- function(mfdobj1, mfdobj2) {

  if (!is.null(mfdobj1) & !is.mfd(mfdobj1)) {
    stop("mfdobj1 must be an object of class mfd if not NULL")
  }
  if (!is.null(mfdobj2) & !is.mfd(mfdobj2)) {
    stop("mfdobj2 must be an object of class mfd if not NULL")
  }

  if (is.null(mfdobj2)) {
    return(mfdobj1)
  }
  if (is.null(mfdobj1)) {
    return(mfdobj2)
  }

  if (dim(mfdobj1$coefs)[2] != dim(mfdobj2$coefs)[2]) {
    stop("mfdobj1 and mfdobj2 must have the same number of replications.")
  }
  if (!identical(mfdobj1$basis, mfdobj2$basis)) {
    stop("mfdobj1 and mfdobj2 must have the same basis.")
  }
  mfd(coef = array(c(mfdobj1$coefs,
                     mfdobj2$coefs),
                   dim = c(dim(mfdobj1$coefs)[1],
                           dim(mfdobj1$coefs)[2],
                           dim(mfdobj1$coefs)[3] + dim(mfdobj2$coefs)[3])),
      basisobj = mfdobj1$basis,
      fdnames = list(mfdobj1$fdnames[[1]],
                     mfdobj1$fdnames[[2]],
                     c(mfdobj1$fdnames[[3]],
                       mfdobj2$fdnames[[3]])),
      B = mfdobj1$basis$B)
}


#' Bind replications of two Multivariate Functional Data Objects
#'
#' @param mfdobj1
#' An object of class mfd, with the same variables of mfdobj2
#' and different replication names with respect to mfdobj2.
#' @param mfdobj2
#' An object of class mfd, with the same variables of mfdobj1,
#' and different replication names with respect to mfdobj1.
#'
#' @return
#' An object of class mfd, whose variables are the same of mfdobj1 and
#' mfdobj2 and whose replications are the union of the replications
#' in mfdobj1 and mfdobj2.
#' @export
#'
#' @examples
#' library(funcharts)
#' mfdobj1 <- data_sim_mfd(nvar = 3, nobs = 4)
#' mfdobj2 <- data_sim_mfd(nvar = 3, nobs = 5)
#' dimnames(mfdobj2$coefs)[[2]] <-
#'   mfdobj2$fdnames[[2]] <-
#'   c("rep11", "rep12", "rep13", "rep14", "rep15")
#' mfdobj_rbind <- rbind_mfd(mfdobj1, mfdobj2)
#' plot_mfd(mfdobj_rbind)
#'
rbind_mfd <- function(mfdobj1, mfdobj2) {

  if (!is.null(mfdobj1) & !is.mfd(mfdobj1)) {
    stop("mfdobj1 must be an object of class mfd if not NULL")
  }
  if (!is.null(mfdobj2) & !is.mfd(mfdobj2)) {
    stop("mfdobj2 must be an object of class mfd if not NULL")
  }

  if (is.null(mfdobj2)) {
    return(mfdobj1)
  }
  if (is.null(mfdobj1)) {
    return(mfdobj2)
  }

  if (dim(mfdobj1$coefs)[3] != dim(mfdobj2$coefs)[3]) {
    stop("mfdobj1 and mfdobj2 must have the same number of variables")
  }
  if (!identical(mfdobj1$basis, mfdobj2$basis)) {
    stop("mfdobj1 and mfdobj2 must have the same basis.")
  }

  coef1_aperm <- aperm(mfdobj1$coefs, c(1, 3, 2))
  coef2_aperm <- aperm(mfdobj2$coefs, c(1, 3, 2))
  coef_aperm <- array(c(coef1_aperm, coef2_aperm),
                      dim = c(dim(mfdobj1$coefs)[1],
                              dim(mfdobj1$coefs)[3],
                              dim(mfdobj1$coefs)[2] + dim(mfdobj2$coefs)[2]))
  coef <- aperm(coef_aperm, c(1, 3, 2))

  mfd(coef = coef,
      basisobj = mfdobj1$basis,
      fdnames = list(mfdobj1$fdnames[[1]],
                     c(mfdobj1$fdnames[[2]],
                       mfdobj2$fdnames[[2]]),
                     mfdobj2$fdnames[[3]]),
      B = mfdobj1$basis$B)
}

# Plots -------------------------------------------------------------------

#' Convert a Multivariate Functional Data Object into a data frame
#'
#' This function extracts the argument \code{raw} of an
#' object of class \code{mfd}.
#'
#' @param mfdobj A multivariate functional data object of class mfd.
#'
#' @return
#' A \code{data.frame} in the long format,
#' which has been used to create the object \code{mfdobj}
#' using the function \code{\link{get_mfd_df}},
#' or that has been created when creating \code{mfdobj}
#' using the function \code{\link{get_mfd_list}}.
#'
#' @seealso \code{\link{get_mfd_df}}, \code{\link{get_mfd_list}}
#' @noRd
mfd_to_df_raw <- function(mfdobj) {

  id <- NULL

  if (!(is.mfd(mfdobj))) {
    stop("Input must be a multivariate functional data object.")
  }

  dt <- mfdobj$raw
  id_var <- mfdobj$id_var

  arg_var <- mfdobj$fdnames[[1]]
  obs <- mfdobj$fdnames[[2]]
  variables <- mfdobj$fdnames[[3]]
  vars_to_select <- c(variables, id_var, arg_var)
  dt %>%
    dplyr::select(dplyr::all_of(vars_to_select)) %>%
    dplyr::rename(id = !!id_var) %>%
    dplyr::filter(id %in% !!obs) %>%
    tidyr::pivot_longer(dplyr::all_of(variables), names_to = "var") %>%
    dplyr::arrange("id", "var", !!arg_var) %>%
    tidyr::drop_na()
}


#' Discretize a Multivariate Functional Data Object
#'
#' This function discretizes an object of class \code{mfd}
#' and stores it into a \code{data.frame} object in the long format.
#'
#' @param mfdobj A multivariate functional data object of class mfd.
#'
#' @return
#' A \code{data.frame} in the long format,
#' obtained by discretizing the functional values using
#' \code{fda::\link[fda]{eval.fd}}
#' on a grid of 200 equally spaced argument values
#' covering the functional domain.
#' @noRd
mfd_to_df <- function(mfdobj) {

  pos <- NULL

  n <- var <- NULL

  if (!(is.mfd(mfdobj))) {
    stop("Input must be a multivariate functional data object.")
  }

  n_obs <- length(mfdobj$fdnames[[2]])
  arg_var <- mfdobj$fdnames[[1]]
  range <- mfdobj$basis$rangeval
  evalarg <- seq(range[1], range[2], l = 200)
  X <- fda::eval.fd(evalarg, mfdobj)
  id <- mfdobj$fdnames[[2]]

  id <- data.frame(id = id) %>%
    dplyr::mutate(pos = seq_len(dplyr::n())) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::mutate(id = ifelse(n == 1,
                              id,
                              paste0(id, " rep", seq_len(dplyr::n())))) %>%
    dplyr::arrange(pos) %>%
    dplyr::pull(id)

  variables <- mfdobj$fdnames[[3]]
  lapply(seq_along(variables), function(jj) {
    variable <- variables[jj]

    .df <- as.data.frame(X[, , jj, drop = FALSE]) %>%
      stats::setNames(id)
    .df[[arg_var]] <- evalarg
    .df$var <- variable
    tidyr::pivot_longer(.df, -c(!!arg_var, var), names_to = "id")
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(var = factor(var, levels = !!variables),
                  id = factor(id, levels = !!id))
}


#' Plot a Multivariate Functional Data Object.
#'
#' Plot an object of class \code{mfd} using \code{ggplot2}
#' and \code{patchwork}.
#'
#' @param mfdobj
#' A multivariate functional data object of class mfd.
#' @param data
#' A \code{data.frame} providing columns
#' to create additional aesthetic mappings.
#' It must contain a factor column "id" with the replication values
#' as in \code{mfdobj$fdnames[[2]]}.
#' If it contains a column "var", this must contain
#' the functional variables as in \code{mfdobj$fdnames[[3]]}.
#' @param mapping
#' Set of aesthetic mappings additional
#' to \code{x} and \code{y} as passed to the function
#' \code{ggplot2::geom:line}.
#' @param stat
#' See \code{ggplot2::\link[ggplot2]{geom_line}}.
#' @param position
#' See \code{ggplot2::\link[ggplot2]{geom_line}}.
#' @param na.rm
#' See \code{ggplot2::\link[ggplot2]{geom_line}}.
#' @param orientation
#' See \code{ggplot2::\link[ggplot2]{geom_line}}.
#' @param show.legend
#' See \code{ggplot2::\link[ggplot2]{geom_line}}.
#' @param inherit.aes
#' See \code{ggplot2::\link[ggplot2]{geom_line}}.
#' @param type_mfd
#' A character value equal to "mfd" or "raw".
#' If "mfd", the smoothed functional data are plotted, if "raw",
#' the original discrete data are plotted.
#' @param y_lim_equal
#' A logical value. If \code{TRUE}, the limits of the y-axis
#' are the same for all functional variables.
#' If \code{FALSE}, limits are different for each variable.
#' Default value is \code{FALSE}.
#' @param ...
#' See \code{ggplot2::\link[ggplot2]{geom_line}}.
#'
#' @return
#' A plot of the multivariate functional data object.
#'
#' @export
#' @examples
#' library(funcharts)
#' library(ggplot2)
#' mfdobj <- data_sim_mfd()
#' ids <- mfdobj$fdnames[[2]]
#' df <- data.frame(id = ids, first_two_obs = ids %in% c("rep1", "rep2"))
#' plot_mfd(mapping = aes(colour = first_two_obs),
#'          data = df,
#'          mfdobj = mfdobj)
#'
plot_mfd <- function(mfdobj,
                     mapping = NULL,
                     data = NULL,
                     stat = "identity",
                     position = "identity",
                     na.rm = TRUE,
                     orientation = NA,
                     show.legend = NA,
                     inherit.aes = TRUE,
                     type_mfd = "mfd",
                     y_lim_equal = FALSE,
                     ...) {

  var <- id <- value <- NULL

  if (!(is.mfd(mfdobj))) {
    stop("First argument must be a multivariate functional data object.")
  }

  if (!(type_mfd %in% c("mfd", "raw"))) {
    stop("type_mfd not 'mfd' or 'raw'")
  }
  if (type_mfd == "mfd") df <- mfd_to_df(mfdobj)
  if (type_mfd == "raw") df <- mfd_to_df_raw(mfdobj)

  if (!is.null(data)) {
    join_vars <- "id"
    if ("var" %in% names(data)) join_vars <- c(join_vars, "var")
    df <- dplyr::inner_join(df, data, by = join_vars)
  }
  variables <- mfdobj$fdnames[[3]]
  df$var <- factor(as.character(df$var), levels = variables)
  arg_var <- mfdobj$fdnames[[1]]
  if (grepl(" ", arg_var)) {
    mapping1 <- ggplot2::aes(!!dplyr::sym(paste0("`", arg_var, "`")), y = value, group = id)
  } else {
    mapping1 <- ggplot2::aes(!!dplyr::sym(arg_var), y = value, group = id)
  }
  mapping_tot <- c(mapping1, mapping)
  class(mapping_tot) <- "uneval"

  ylim_common <- range(df$value)

  for (kk in seq_along(mapping_tot)) {

    column <- as.character(mapping_tot[kk])
    column <- substr(column, 2, nchar(column))
    mapping_type <- names(mapping_tot)[kk]


    if (!(column %in% names(df))) {

      val <- eval(parse(text = column))
      ids <- unique(df$id)
      if (length(val) != length(ids)) {
        stop("The aesthetic ", column,
             " must have length equal to the number of observations nobs(mfdobj).")
      }
      df_map <- data.frame(id = ids, tmp = val)
      names(df_map)[names(df_map) == "tmp"] <- column
      df <- dplyr::inner_join(df, df_map, by = "id")
    }
  }

  plot_list <- list()
  for (jj in seq_along(variables)) {
    dat <- df %>%
      dplyr::filter(var == variables[jj])

    if (grepl(" ", arg_var)) {
      mapping1 <- ggplot2::aes(!!dplyr::sym(paste0("`", arg_var, "`")), value, group = id)
    } else {
      mapping1 <- ggplot2::aes(!!dplyr::sym(arg_var), value, group = id)
    }

    mapping_tot <- c(mapping1, mapping)
    class(mapping_tot) <- "uneval"


    geom_line_obj <- ggplot2::geom_line(mapping = mapping_tot,
                                        data = dat,
                                        stat = stat,
                                        position = position,
                                        na.rm = na.rm,
                                        orientation = orientation,
                                        show.legend = show.legend,
                                        inherit.aes = inherit.aes,
                                        ...)
    if (is.null(geom_line_obj$aes_params$linewidth) & is.null(geom_line_obj$mapping$linewidth)) {
      geom_line_obj$aes_params$linewidth <- 0.25
    }

    p <- ggplot2::ggplot() +
      geom_line_obj +
      ggplot2::ylab(variables[jj]) +
      ggplot2::xlab(arg_var) +
      ggplot2::theme_bw() +
      ggplot2::scale_linetype_discrete(drop = FALSE) +
      ggplot2::scale_colour_discrete(drop = FALSE)



    if (!is.null(p$layers[[1]]$data[["colour"]])) {
      if (is.numeric(p$layers[[1]]$data$colour)) {
        p <- p + ggplot2::scale_colour_continuous(limits = range(dat$colour))
      } else {
        p <- p + ggplot2::scale_colour_discrete(drop = FALSE)
      }
    }
    if (!is.null(p$layers[[1]]$data[["color"]])) {
      if (is.numeric(p$layers[[1]]$data$color)) {
        p <- p + ggplot2::scale_color_continuous(limits = range(dat$color))
      } else {
        p <- p + ggplot2::scale_color_discrete(drop = FALSE)
      }
    }

    if (y_lim_equal) {
      p <- p + ggplot2::ylim(ylim_common)
    }

    plot_list[[jj]] <- p
  }

  patchwork::wrap_plots(plot_list) #+ patchwork::plot_layout(guides = "collect")

}





#' Add the plot of a new multivariate functional data object to an existing
#' plot.
#'
#'
#' @param plot_mfd_obj
#' A plot produced by \code{link{plot_mfd}}
#' @param mfdobj_new
#' A new multivariate functional data object of class mfd to be plotted.
#' @param data
#' See \code{\link{plot_mfd}}.
#' @param mapping
#' See \code{\link{plot_mfd}}.
#' @param stat
#' See \code{\link{plot_mfd}}.
#' @param position
#' See \code{\link{plot_mfd}}.
#' @param na.rm
#' See \code{\link{plot_mfd}}.
#' @param orientation
#' See \code{\link{plot_mfd}}.
#' @param show.legend
#' See \code{\link{plot_mfd}}.
#' @param inherit.aes
#' See \code{\link{plot_mfd}}.
#' @param type_mfd
#' See \code{\link{plot_mfd}}.
#' @param y_lim_equal
#' See \code{\link{plot_mfd}}.
#' @param ...
#' See \code{\link{plot_mfd}}.
#'
#' @return
#' A plot of the multivariate functional data object added to the existing
#' one.
#'
#' @export
#' @examples
#' library(funcharts)
#' library(ggplot2)
#' mfdobj1 <- data_sim_mfd()
#' mfdobj2 <- data_sim_mfd()
#' p <- plot_mfd(mfdobj1)
#' lines_mfd(p, mfdobj_new = mfdobj2)
#'
lines_mfd <- function(plot_mfd_obj,
                      mfdobj_new,
                      mapping = NULL,
                      data = NULL,
                      stat = "identity",
                      position = "identity",
                      na.rm = TRUE,
                      orientation = NA,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      type_mfd = "mfd",
                      y_lim_equal = FALSE,
                      ...) {
  nvars <- length(plot_mfd_obj$patches$plots) + 1

  p2 <- plot_mfd(mfdobj = mfdobj_new,
                 mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 na.rm = na.rm,
                 orientation = orientation,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 type_mfd = type_mfd,
                 y_lim_equal = y_lim_equal,
                 ... = ...)

  # check obs names
  obs_names <- list()
  for (jj in seq_len(nvars)) {
    obs_names[[jj]] <-
      unique(as.character(plot_mfd_obj[[1]]$layers[[1]]$data$id))
  }
  obs_names <- unique(unlist(obs_names))
  if (any(mfdobj_new$fdnames[[2]] %in% obs_names)) {
    mfdobj_new$fdnames[[2]] <- paste0(mfdobj_new$fdnames[[2]], "_2")
    dimnames(mfdobj_new$coefs)[[2]] <- paste0(mfdobj_new$fdnames[[2]], "_2")
  }

  if (!y_lim_equal) {
    plot_mfd_obj$scales$scales[[2]] <- NULL
  } else {
    ylim_common2 <- p2$scales$scales[[2]]$limits
    if (length(plot_mfd_obj$scales$scales) == 2) {
      ylim_common <- plot_mfd_obj$scales$scales[[2]]$limits
      plot_mfd_obj$scales$scales[[2]] <- NULL
    } else {
      ylim_common <- range(vapply(seq_len(nvars), function(jj) {
        range(plot_mfd_obj[[jj]]$layers[[1]]$data$value)
      }, c(0, 0)))
    }
    ylim_common_new <- range(c(ylim_common, ylim_common2))
  }

  p <- plot_mfd_obj
  for (jj in seq_len(nvars)) {
    p[[jj]] <- plot_mfd_obj[[jj]] +
      p2[[jj]]$layers[[1]]
    if (y_lim_equal) {
      p[[jj]]$scales$scales[[2]] <- NULL
      p[[jj]] <- p[[jj]] + ggplot2::ylim(ylim_common_new)
    }

  }
  p

}




#' Plot a Bivariate Functional Data Object.
#'
#' Plot an object of class \code{bifd} using
#' \code{ggplot2} and \code{geom_tile}.
#' The object must contain only one single functional replication.
#'
#' @param bifd_obj A bivariate functional data object of class bifd,
#' containing one single replication.
#' @param type_plot a character value
#' If "raster", it plots the bivariate functional data object
#' as a raster image.
#' If "contour", it produces a contour plot.
#' If "perspective", it produces a perspective plot.
#' Default value is "raster".
#' @param phi
#' If \code{type_plot=="perspective"}, it is the \code{phi} argument
#' of the function \code{plot3D::persp3D}.
#' @param theta
#' If \code{type_plot=="perspective"}, it is the \code{theta} argument
#' of the function \code{plot3D::persp3D}.
#'
#' @return
#' A ggplot with a geom_tile layer providing a plot of the
#' bivariate functional data object as a heat map.
#' @export
#'
#' @examples
#' library(funcharts)
#' mfdobj <- data_sim_mfd(nobs = 1)
#' tp <- tensor_product_mfd(mfdobj)
#' plot_bifd(tp)
#'
plot_bifd <- function(bifd_obj,
                      type_plot = "raster",
                      phi = 40,
                      theta = 40) {

  s <- t <- value <- NULL

  if (!inherits(bifd_obj, "bifd")) {
    stop("bifd_obj must be an object of class bifd")
  }
  if (length(dim(bifd_obj$coef)) != 4) {
    stop("length of bifd_obj$coef must be 4")
  }
  if (dim(bifd_obj$coef)[3] != 1) {
    stop("third dimension of bifd_obj$coef must be 1")
  }
  if (!(type_plot %in% c("raster", "contour", "perspective"))) {
    stop("type_plot must be one of \"raster\", \"contour\", \"perspective\"")
  }


  s_eval <- seq(bifd_obj$sbasis$rangeval[1],
                bifd_obj$sbasis$rangeval[2],
                l = 100)
  t_eval <- seq(bifd_obj$tbasis$rangeval[1],
                bifd_obj$tbasis$rangeval[2],
                l = 100)
  X_eval <- fda::eval.bifd(s_eval, t_eval, bifd_obj)

  variables <- bifd_obj$bifdnames[[4]]
  nvar <- length(variables)
  zlim <- c(- max(abs(X_eval)), max(abs(X_eval)))

  if (type_plot == "perspective") {
    phi <- 40
    theta <- 40
    nr <- ceiling(sqrt(nvar))
    graphics::par(mfrow = c(nr, nr))
    for (ii in seq_len(nvar)) {
      graphics::persp(s_eval,
                      t_eval,
                      X_eval[,,1,ii],
                      phi = phi,
                      theta = theta,
                      main = variables[ii],
                      zlim = zlim,
                      xlab = "s",
                      ylab = "t",
                      zlab = "value")
    }
    graphics::par(mfrow = c(1, 1))
  } else {
    plot_list <- list()
    for (ii in seq_along(variables)) {
      p <- X_eval[, , , ii] %>%
        data.frame() %>%
        stats::setNames(t_eval) %>%
        dplyr::mutate(s = s_eval) %>%
        tidyr::pivot_longer(-s, names_to = "t", values_to = "value") %>%
        dplyr::mutate(t = as.numeric(t),
                      variable = bifd_obj$bifdnames[[4]][ii]) %>%
        ggplot2::ggplot() +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       strip.background = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(colour = "black")) +
        ggplot2::ggtitle(variables[ii])

      if (type_plot == "raster") {
        p <- p +
          ggplot2::geom_tile(ggplot2::aes(s, t, fill = value)) +
          ggplot2::scale_fill_gradientn(
            colours = c("blue", "white", "red"),
            limits = zlim)
      }

      if (type_plot == "contour") {
        p <- p +
          ggplot2::geom_contour(ggplot2::aes(
            s,
            t,
            z = value,
            colour = ggplot2::after_stat(get("level")))) +
          ggplot2::scale_color_gradientn(
            colours = c("blue", "white", "red"),
            limits = zlim) +
          ggplot2::labs(colour = 'level')
      }
      plot_list[[ii]] <- p +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                          size = 9))
    }
    patchwork::wrap_plots(plot_list) +
      patchwork::plot_layout(guides = "collect")

  }

}

#' Plot multivariate functional data
#'
#' @method plot mfd
#' @param x An `mfd` object.
#' @param y Ignored.
#' @param add Logical; if TRUE, add curves to an existing `mfd` plot
#'   (using \code{matlines}) instead of creating a new one.
#' @param common_ylim Logical; if TRUE, all panels share the same y-limits,
#'   otherwise each panel adapts its own scale.
#' @param ... Graphical arguments passed to \code{matplot} (if
#'   \code{add=FALSE}) or to \code{matlines} (if \code{add=TRUE}).
#' @export
plot.mfd <- function(x, y, add = FALSE, common_ylim = TRUE, ...) {
  args <- list(...)
  domain  <- x$basis$rangeval
  evalarg <- seq(domain[1], domain[2], length.out = 200)
  X <- fda::eval.fd(evalarg, x)
  y_range <- range(X)
  nvar     <- dim(X)[3]
  varnames <- x$fdnames[[3]]
  tvar     <- x$fdnames[[1]]

  ncol <- ceiling(sqrt(nvar))
  nrow <- ceiling(nvar / ncol)

  if (is.null(args$col)) args$col <- "black"
  if (is.null(args$lty)) args$lty <- 1
  if (!is.null(args$ylim)) {
    y_range <- args$ylim
    common_ylim <- TRUE
    args$ylim <- NULL
  }

  if (!add) {
    if (is.null(args$mar)) args$mar <- c(4, 4, 0.5, 0.5)

    oldpar <- graphics::par(c("mar", "mgp"))
    on.exit(graphics::par(oldpar), add = TRUE)

    graphics::par(mfrow = c(nrow, ncol), mar = args$mar, mgp = c(2.5, 1, 0))

    per_var_ylims <- vector("list", nvar)
    per_var_usrs  <- vector("list", nvar)

    for (ii in 1:nvar) {
      ylim_ii <- if (common_ylim) y_range else range(X[,,ii])

      graphics::plot.new()
      graphics::plot.window(xlim = range(evalarg), ylim = ylim_ii)

      do.call(graphics::matlines,
              c(list(x = evalarg, y = X[,,ii]), args))

      graphics::axis(1); graphics::axis(2)
      graphics::title(xlab = tvar, ylab = varnames[ii])
      graphics::box()

      per_var_ylims[[ii]] <- ylim_ii
      per_var_usrs[[ii]]  <- graphics::par("usr")  # save panel usr
    }

    options(
      last_mfd_nvar  = nvar,
      last_mfd_xlim  = range(evalarg),
      last_mfd_ylims = per_var_ylims,
      last_mfd_common_ylim = common_ylim
    )

  } else {
    nvar_layout <- getOption("last_mfd_nvar")
    ylims       <- getOption("last_mfd_ylims")
    if (is.null(nvar_layout) || is.null(ylims)) {
      stop("No existing mfd plot found. Call plot.mfd(..., add=FALSE) first.")
    }

    ii_row <- 1
    ii_col <- 1
    ylims <- options()$last_mfd_ylims
    xlim <- options()$last_mfd_xlim
    for (ii in 1:nvar) {
      if (ii_col > ncol) {
        ii_col <- 1
        ii_row <- ii_row + 1
      }
      graphics::par(mfg = c(ii_row, ii_col))
      if (is.null(args$mar)) args$mar <- c(4, 4, 0.5, 0.5)
      graphics::par(mar = args$mar, mgp = c(2.5, 1, 0))

      graphics::plot.window(xlim = xlim, ylim = ylims[[ii]])

      do.call(graphics::matlines,
              c(list(x = evalarg, y = X[,,ii]), args))
      ii_col <- ii_col + 1
    }
  }
  invisible(NULL)
}


#' Add curves to an existing multivariate functional data plot
#'
#' Adds new curves from an `mfd` object to an existing plot created by
#' \code{\link{plot.mfd}}. Each variable is added to its corresponding panel,
#' using the same scales and layout as the original plot.
#'
#' @param x An `mfd` object.
#' @param ... Graphical arguments passed to \code{\link{plot.mfd}} with
#'   \code{add=TRUE}.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @seealso \code{\link{plot.mfd}}, \code{\link{abline_mfd}}
#'
#'
#' @method lines mfd
#' @export
lines.mfd <- function(x, ...) {
  plot.mfd(x, add = TRUE, ...)
}



#' Add reference lines to all panels of the current multi-panel plot
#'
#' Calls \code{\link[graphics]{abline}} in every panel actually used by the most
#' recent call to \code{\link{plot.mfd}}.
#'
#' @inheritParams graphics::abline
#' @param ... Further graphical parameters (e.g., \code{col}, \code{lty}, \code{lwd}).
#'
#' @details
#' The function relies on \code{plot.mfd} having stored the number of variables
#' in \code{options("last_mfd_nvar")}. It then loops over exactly that many
#' panels in the current layout.
#'
#' Calls \code{\link[graphics]{abline}} in every panel actually used by the most
#' recent call to \code{\link{plot.mfd}}. Vertical and horizontal lines span
#' the full x- or y-range of each panel, even when scales differ.
#'
#' @inheritParams graphics::abline
#' @param ... Further graphical parameters (e.g., \code{col}, \code{lty}, \code{lwd}).
#'
#' @export
abline_mfd <- function(a = NULL, b = NULL, h = NULL, v = NULL, ...) {
  mf <- graphics::par("mfrow")
  ncol <- mf[2]

  nvar  <- getOption("last_mfd_nvar")
  ylims <- getOption("last_mfd_ylims")
  xlim  <- getOption("last_mfd_xlim")
  if (is.null(nvar) || is.null(ylims) || is.null(xlim)) {
    stop("No stored panel limits found. Call plot.mfd(..., add=FALSE) first.")
  }

  oldpar <- graphics::par(c("mar", "mgp", "mfg"))
  on.exit(graphics::par(oldpar), add = TRUE)

  ii_row <- 1
  ii_col <- 1
  for (ii in 1:nvar) {
    if (ii_col > ncol) {
      ii_col <- 1
      ii_row <- ii_row + 1
    }
    graphics::par(mfg = c(ii_row, ii_col))

    usr <- c(xlim[1], xlim[2], ylims[[ii]][1], ylims[[ii]][2])
    graphics::plot.window(xlim = usr[1:2], ylim = usr[3:4])

    # horizontal lines
    if (!is.null(h)) {
      for (y in h) graphics::segments(usr[1], y, usr[2], y, ...)
    }
    # vertical lines
    if (!is.null(v)) {
      for (x in v) graphics::segments(x, usr[3], x, usr[4], ...)
    }
    # slope/intercept lines
    if (!is.null(a) || !is.null(b)) {
      graphics::abline(a = a, b = b, ...)
    }

    ii_col <- ii_col + 1
  }
  invisible(NULL)
}






#' Mean Function for Multivariate Functional Data
#'
#' Computes the mean function for an object of class \code{mfd}.
#'
#' @method mean mfd
#' @param x An object of class \code{mfd}, containing \eqn{N} observations of a
#'   \eqn{p}-dimensional multivariate functional variable.
#' @param ... Further arguments are ignored (required for S3 consistency).
#'
#' @return An object of class \code{mfd} representing the mean function. The
#'   output contains a single observation, corresponding to the mean function
#'   for each variable.
#'
#' @details
#' The method averages the coefficient array across the observation dimension,
#' resulting in a new \code{mfd} object with one observation (the sample mean).
#'
#' @examples
#' \donttest{
#' library(funcharts)
#' data(air)
#' mfdobj <- get_mfd_list(air)
#' mean_result <- mean(mfdobj)
#' plot(mean_result)
#' }
#'
#' @seealso \code{\link{mfd}}, \code{\link{plot.mfd}}
#'
#' @export
mean.mfd <- function(x, ...) {
  coef_mean <- apply(x$coefs, c(1, 3), mean)
  coef_mean <- array(coef_mean, dim = c(nrow(coef_mean), 1, ncol(coef_mean)))
  fdnames <- list(x$fdnames[[1]], "sample mean", x$fdnames[[3]])
  mfd(coef_mean, x$basis, fdnames)
}



#' Covariance Function for Multivariate Functional Data
#'
#' Computes the covariance function for two multivariate functional data objects of class `mfd`.
#'
#' @param mfdobj1 An object of class `mfd` representing the first multivariate functional data set.
#'   It contains \eqn{N} observations of a \eqn{p}-dimensional multivariate functional variable.
#' @param mfdobj2 An object of class `mfd` representing the second multivariate functional data set.
#'   Defaults to `mfdobj1`. If provided, it must also contain \eqn{N} observations of a \eqn{p}-dimensional
#'   multivariate functional variable.
#'
#' @return A bifd object representing the covariance function of the two input objects. The output
#'   is a collection of \eqn{p^2} functional surfaces, each corresponding to the covariance between
#'   two components of the multivariate functional data.
#'
#' @details
#' The function calculates the covariance between all pairs of dimensions from the two multivariate
#' functional data objects. Each covariance is represented as a functional surface in the resulting
#' bifd object. The covariance function is useful for analyzing relationships between functional variables.
#'
#' @examples
#' \donttest{
#' library(funcharts)
#' data("air")
#' x <- get_mfd_list(air[1:3])
#' cov_result <- cov_mfd(x)
#' plot_bifd(cov_result)
#' }
#'
#' @export
cov_mfd <- function(mfdobj1, mfdobj2 = mfdobj1) {
  fdobj1 <- mfdobj1
  fdobj2 <- mfdobj2
  coefx <- fdobj1$coefs
  coefy <- fdobj2$coefs
  coefdobj1 <- dim(coefx)
  coefdobj2 <- dim(coefy)
  basisx <- fdobj1$basis
  basisy <- fdobj2$basis
  nbasisx <- basisx$nbasis
  nbasisy <- basisy$nbasis

  nvar <- coefdobj1[3]
  coefvar <- array(0, c(nbasisx, nbasisx, 1, nvar^2))
  varnames <- fdobj1$fdnames[[3]]
  m <- 0
  bivarnames <- vector("character", nvar^2)
  for (i in 1:nvar) for (j in 1:nvar) {
    m <- m + 1
    coefvar[, , 1, m] <- stats::var(t(coefx[, , i]), t(coefx[, , j]))
    bivarnames[m] <- paste(varnames[i], "vs", varnames[j])
  }
  bifdnames <- list()
  bifdnames[[1]] <- fdobj1$names
  bifdnames[[2]] <- fdobj2$names
  bifdnames[[3]] <- "covariance"
  bifdnames[[4]] <- bivarnames

  varbifd <- fda::bifd(coefvar, basisx, basisx, bifdnames)
  return(varbifd)
}

#' Correlation Function for Multivariate Functional Data
#'
#' Computes the correlation function for two multivariate functional data objects of class `mfd`.
#'
#' @param mfdobj1 An object of class `mfd` representing the first multivariate functional data set.
#'   It contains \eqn{N} observations of a \eqn{p}-dimensional multivariate functional variable.
#' @param mfdobj2 An object of class `mfd` representing the second multivariate functional data set.
#'   Defaults to `mfdobj1`. If provided, it must also contain \eqn{N} observations of a \eqn{p}-dimensional
#'   multivariate functional variable.
#'
#' @return A bifd object representing the correlation function of the two input objects. The output
#'   is a collection of \eqn{p^2} functional surfaces, each corresponding to the correlation between
#'   two components of the multivariate functional data.
#'
#' @details
#' The function calculates the correlation between all pairs of dimensions from the two multivariate
#' functional data objects. The data is first scaled using \code{\link{scale_mfd}}, and the correlation
#' is then computed as the covariance of the scaled data using \code{\link{cov_mfd}}.
#'
#' @examples
#' \donttest{
#' library(funcharts)
#' data("air")
#' x <- get_mfd_list(air[1:3])
#' cor_result <- cor_mfd(x)
#' plot_bifd(cor_result)
#' }
#'
#' @export
cor_mfd <- function(mfdobj1, mfdobj2 = mfdobj1) {
  mfdobj1_scaled <- scale_mfd(mfdobj1)
  mfdobj2_scaled <- scale_mfd(mfdobj2)
  cor_result <- cov_mfd(mfdobj1_scaled, mfdobj2_scaled)
  cor_result$bifdnames[[3]] <- "correlation"
  return(cor_result)
}


#' @noRd
#'
# project_mfd <- function(X, basisobj) {
#
#   fdnames <- NULL
#   if (is.mfd(X)) {
#     p <- dim(X$coef)[3]
#     bs <- X$basis
#     rb <- bs$rangeval
#     xseq <- seq(rb[1], rb[2], l = 300)
#     Xeval <- fda::eval.fd(xseq, X)
#     Xeval <- aperm(Xeval, c(2, 1, 3))
#     fdnames <- X$fdnames
#   }
#   if (is.array(X)) {
#     rb <- c(0, 1)
#     xseq <- seq(rb[1], rb[2], l = dim(X)[2])
#     Xeval <- X
#     Xeval <- aperm(Xeval, c(2, 1, 3))
#   }
#   if (is.list(X) & !is.mfd(X)) {
#     rb <- c(0, 1)
#     xseq <- seq(rb[1], rb[2], l = ncol(X[[1]]))
#     Xeval <- simplify2array(X)
#     Xeval <- aperm(Xeval, c(2, 1, 3))
#   }
#
#   coefList <- fda::project.basis(Xeval, argvals = xseq, basisobj = basisobj)
#   X_mfd <- mfd(coefList, basisobj = basisobj, fdnames = fdnames)
#   X_mfd
#
# }
