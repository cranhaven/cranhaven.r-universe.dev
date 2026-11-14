#' Abstract Class for Matrix Lie Algebras
#'
#' @description There are two main forms of representation for elements of a
#'   matrix Lie algebra implemented here. The first one is as a matrix, as
#'   elements of \eqn{R^{n \times n}}. The second is by choosing a basis and
#'   remembering the coefficients of an element in that basis. This basis will
#'   be provided in child classes (e.g. `SkewSymmetricMatrices`).
#'
#' @author Stefan Heyder
#'
#' @keywords internal
MatrixLieAlgebra <- R6::R6Class(
  classname = "MatrixLieAlgebra",
  inherit = VectorSpace,
  public = list(
    #' @field n An integer value representing the number of rows and columns in
    #'   the matrix representation of the Lie algebra.
    n = NULL,

    #' @description The [`MatrixLieAlgebra`] class constructor.
    #'
    #' @param dim An integer value specifying the dimension of the Lie algebra
    #'   as a real vector space.
    #' @param n An integer value representing the number of rows and columns in
    #'   the matrix representation of the Lie algebra.
    #' @param ... Extra arguments to be passed to parent class constructors. See
    #'   [`VectorSpace`] and  [`Manifold`] classes.
    #' @param py_cls A Python object of class `MatrixLieAlgebra`. Defaults to
    #'   `NULL` in which case it is instantiated on the fly using the other
    #'   input arguments.
    #'
    #' @return An object of class [`MatrixLieAlgebra`].
    initialize = function(dim, n, ..., py_cls = NULL) {
      if (is.null(py_cls)) {
        dots <- capture_extra_params(...)
        dots$dim <- as.integer(dim)
        dots$n <- as.integer(n)
        py_cls <- do.call(gs$geometry$lie_algebra$MatrixLieAlgebra, dots)
      }
      super$set_python_class(py_cls)
      private$set_fields()
    },

    #' @description Calculates the Baker-Campbell-Hausdorff approximation of
    #'   given order.
    #'
    #' @details The implementation is based on
    #'   \insertCite{casas2009efficient;textual}{rgeomstats} with the
    #'   pre-computed constants taken from
    #'   \insertCite{casas2009data;textual}{rgeomstats}. Our coefficients are
    #'   truncated to enable us to calculate BCH up to order \eqn{15}. This
    #'   represents \deqn{Z = \log \left( \exp(X) \exp(Y) \right)} as an
    #'   infinite linear combination of the form \deqn{Z = \sum_i z_i e_i} where
    #'   \eqn{z_i} are rational numbers and \eqn{e_i} are iterated Lie brackets
    #'   starting with \eqn{e_1 = X}, \eqn{e_2 = Y}, each \eqn{e_i} is given by
    #'   some \eqn{(i^\prime,i^{\prime\prime})} such that \eqn{e_i =
    #'   [e_i^\prime, e_i^{\prime\prime}]}.
    #'
    #' @param matrix_a A numeric array of shape \eqn{... \times n \times n}
    #'   specifying a matrix or a sample of matrices.
    #' @param matrix_b A numeric array of shape \eqn{... \times n \times n}
    #'   specifying a matrix or a sample of matrices.
    #' @param order An integer value specifying the order to which the
    #'   approximation is calculated. Note that this is NOT the same as using
    #'   only \eqn{e_i} with \eqn{i < \mathrm{order}}. Defaults to `2L`.
    #'
    #' @return A numeric array of shape \eqn{... \times n \times n} storing a
    #'   matrix or a sample of matrices corresponding to the BCH
    #'   approximation(s) between input matrices.
    baker_campbell_hausdorff = function(matrix_a, matrix_b, order = 2) {
      super$get_python_class()$baker_campbell_hausdorff(
        matrix_a = matrix_a,
        matrix_b = matrix_b,
        order = as.integer(order)
      )
    },

    #' @description Computes the coefficients of matrices in the given basis.
    #'
    #' @param matrix_representation A numeric array of shape \eqn{... \times n
    #'   \times n} specifying a matrix or a sample of matrices in its matrix
    #'   representation.
    #'
    #' @return A numeric array of shape \eqn{... \times \mathrm{dim}} storing a
    #'   matrix or a sample of matrices in its basis representation.
    basis_representation = function(matrix_representation) {
      super$get_python_class()$basis_representation(
        matrix_representation = matrix_representation
      )
    },

    #' @description Compute the matrix representation for the given basis
    #'   coefficients.
    #'
    #' @details Sums the basis elements according to the coefficients given in
    #'   basis representation.
    #'
    #' @param basis_representation A numeric array of shape \eqn{... \times
    #'   \mathrm{dim}} storing a matrix or a sample of matrices in its basis
    #'   representation.
    #'
    #' @return A numeric array of shape \eqn{... \times n \times n} specifying a
    #'   matrix or a sample of matrices in its matrix representation.
    matrix_representation = function(basis_representation) {
      super$get_python_class()$matrix_representation(
        basis_representation = basis_representation
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
