#' Class for the Manifold of Symmetric Positive Definite Matrices
#'
#' @description Class for the manifold of symmetric positive definite (SPD)
#'   matrices.
#'
#' @param tangent_vec A numeric array of shape \eqn{[\dots \times n \times n]}
#'   specifying one or more symmetric matrices at corresponding base points.
#' @param base_point A numeric array of shape \eqn{[\dots \times n \times n]}
#'   specifying one or more SPD matrices specifying base points for the input
#'   tangent vectors.
#'
#' @author Yann Thanwerdas
#'
#' @family symmetric positive definite matrix classes
#' @keywords internal
SPDMatrices <- R6::R6Class(
  classname = "SPDMatrices",
  inherit = OpenSet,
  public = list(
    #' @field n An integer value specifying the number of rows and columns of the
    #'   matrices.
    n = NULL,

    #' @description The [`SPDMatrices`] class constructor.
    #'
    #' @param n An integer value specifying the number of rows and columns of the
    #'   matrices.
    #' @param ... Extra arguments to be passed to parent class constructors. See
    #'   [`OpenSet`] and [`Manifold`] classes.
    #' @param py_cls A Python object of class `SPDMatrices`. Defaults to `NULL`
    #'   in which case it is instantiated on the fly using the other input
    #'   arguments.
    #'
    #' @return An object of class [`SPDMatrices`].
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   spd3
    #' }
    initialize = function(n, ..., py_cls = NULL) {
      if (is.null(py_cls)) {
        dots <- capture_extra_params(...)
        dots$n <- as.integer(n)
        if ("shape" %in% names(dots)) {
          dots$shape <- dots$shape |>
            purrr::map(as.integer) |>
            reticulate::tuple()
        }
        if ("metric" %in% names(dots))
          dots$metric <- dots$metric$get_python_class()
        py_cls <- do.call(gs$geometry$spd_matrices$SPDMatrices, dots)
      }
      super$set_python_class(py_cls)
      private$set_fields()
    },

    #' @description Computes Cholesky factor for a symmetric positive definite
    #'   matrix.
    #'
    #' @param mat A numeric array of shape \eqn{[\dots \times n \times n]}
    #'   specifying one or more SPD matrices.
    #'
    #' @return A numeric array of the same shape storing the corresponding
    #'   Cholesky factors.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$cholesky_factor(A)
    #' }
    cholesky_factor = function(mat) {
      super$get_python_class()$cholesky_factor(mat = mat)
    },

    #' @description Computes the differential of the Cholesky factor map.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times n \times n]} storing
    #'   the differentials of the corresponding Cholesky factor maps.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$differential_cholesky_factor(diag(1, 3), A)
    #' }
    differential_cholesky_factor = function(tangent_vec, base_point) {
      super$get_python_class()$differential_cholesky_factor(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Computes the matrix exponential for a symmetric matrix.
    #'
    #' @param mat A numeric array of shape \eqn{[\dots \times n \times n]}
    #'   specifying one or more symmetric matrices.
    #'
    #' @return A numeric array of the same shape storing the corresponding
    #'   matrix exponentials.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   spd3$expm(diag(-1, 3))
    #' }
    expm = function(mat) {
      super$get_python_class()$expm(mat = mat)
    },

    #' @description Computes the differential of the matrix exponential.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times n \times]} storing
    #'   the differentials of matrix exponential at corresponding base points
    #'   applied to corresponding tangent vectors.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$differential_exp(diag(1, 3), A)
    #' }
    differential_exp = function(tangent_vec, base_point) {
      super$get_python_class()$differential_exp(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Computes the inverse of the differential of the matrix
    #'   exponential.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times n \times]} storing
    #'   the inverse of the differentials of matrix exponential at corresponding
    #'   base points applied to corresponding tangent vectors.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$inverse_differential_exp(diag(1, 3), A)
    #' }
    inverse_differential_exp = function(tangent_vec, base_point) {
      super$get_python_class()$inverse_differential_exp(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Computes the matrix logarithm of an SPD matrix.
    #'
    #' @param mat A numeric array of shape \eqn{[\dots \times n \times n]}
    #'   specifying one or more SPD matrices.
    #'
    #' @return A numeric array of the same shape storing the logarithms of the
    #'   input SPD matrices.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   spd3$logm(diag(1, 3))
    #' }
    logm = function(mat) {
      super$get_python_class()$logm(mat = mat)
    },

    #' @description Computes the differential of the matrix logarithm.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times n \times]} storing
    #'   the differentials of matrix logarithm at corresponding base points
    #'   applied to corresponding tangent vectors.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$differential_log(diag(1, 3), A)
    #' }
    differential_log = function(tangent_vec, base_point) {
      super$get_python_class()$differential_log(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Computes the inverse of the differential of the matrix
    #'   logarithm.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times n \times]} storing
    #'   the inverse of the differentials of matrix logarithm at corresponding
    #'   base points applied to corresponding tangent vectors.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$inverse_differential_log(diag(1, 3), A)
    #' }
    inverse_differential_log = function(tangent_vec, base_point) {
      super$get_python_class()$inverse_differential_log(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Computes the matrix power of an SPD matrix.
    #'
    #' @param mat A numeric array of shape \eqn{[\dots \times n \times n]}
    #'   specifying one or more SPD matrices.
    #' @param power A numeric value or vector specifying the desired power(s).
    #'
    #' @return A numeric array of the same shape as `mat` storing the
    #'   corresponding matrix powers computed as: \deqn{A^p = \exp(p \log(A)).}
    #'   If `power` is a vector, a list of such arrays is returned.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   spd3$powerm(diag(1, 3), 2)
    #' }
    powerm = function(mat, power) {
      super$get_python_class()$powerm(
        mat = mat,
        power = power
      )
    },

    #' @description Computes the differential of the matrix power function.
    #'
    #' @param power An integer value specifying the desired power.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times n \times]} storing
    #'   the differential of the power function \deqn{A^p = \exp(p \log(A))} at
    #'   corresponding base points applied to corresponding tangent vectors.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$differential_power(2, diag(1, 3), A)
    #' }
    differential_power = function(power, tangent_vec, base_point) {
      super$get_python_class()$differential_power(
        power = as.integer(power),
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Computes the inverse of the differential of the matrix power
    #'   function.
    #'
    #' @param power An integer value specifying the desired power.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times n \times]} storing
    #'   the inverse of the differential of the power function \deqn{A^p =
    #'   \exp(p \log(A))} at corresponding base points applied to corresponding
    #'   tangent vectors.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   V <- cbind(
    #'     c(sqrt(2) / 2, -sqrt(2) / 2, 0),
    #'     c(sqrt(2) / 2, sqrt(2) / 2, 0),
    #'     c(0, 0, 1)
    #'   )
    #'   A <- V %*% diag(1:3) %*% t(V)
    #'   spd3$inverse_differential_power(2, diag(1, 3), A)
    #' }
    inverse_differential_power = function(power, tangent_vec, base_point) {
      super$get_python_class()$inverse_differential_power(
        power = as.integer(power),
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
      self$n <- super$get_python_class()$n
    }
  )
)

#' Class for the Manifold of Symmetric Positive Definite Matrices
#'
#' This function generates an instance of the class for the manifold of
#' symmetric positive definite matrices \eqn{\mathrm{SPD}(n)}.
#'
#' @author Yann Thanwerdas
#'
#' @param n An integer value specifying the number of rows and columns of the
#'   matrices.
#' @param ... Extra arguments to be passed to parent class constructors. See
#'   [`OpenSet`] and [`Manifold`] classes.
#'
#' @return An object of class [`SPDMatrices`].
#'
#' @family symmetric positive definite matrix classes
#' @export
#' @examples
#' if (reticulate::py_module_available("geomstats")) {
#'   spd3 <- SPDMatrix(n = 3)
#'   spd3
#' }
SPDMatrix <- function(n, ...) {
  SPDMatrices$new(n = n, ...)
}
