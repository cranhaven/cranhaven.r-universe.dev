#' Class for the Bures-Wasserstein Metric on the Manifold of Symmetric Positive
#' Definite Matrices
#'
#' @description An [R6::R6Class] object implementing the
#'   [`SPDMetricBuresWasserstein`] class. This is the class for the
#'   Bures-Wasserstein metric on the SPD manifold
#'   \insertCite{bhatia2019bures,malago2018wasserstein}{rgeomstats}.
#'
#' @references
#' \insertAllCited{}
#'
#' @author Yann Thanwerdas
#'
#' @export
SPDMetricBuresWasserstein <- R6::R6Class(
  classname = "SPDMetricBuresWasserstein",
  inherit = RiemannianMetric,
  public = list(
    #' @field n An integer value specifying the shape of the matrices: \eqn{n
    #'   \times n}.
    n = NULL,

    #' @description The [`SPDMetricBuresWasserstein`] class constructor.
    #'
    #' @param n An integer value specifying the shape of the matrices: \eqn{n
    #'   \times n}.
    #' @param py_cls A Python object of class `SPDMetricBuresWasserstein`.
    #'   Defaults to `NULL` in which case it is instantiated on the fly using
    #'   the other input arguments.
    #'
    #' @return An object of class [`SPDMetricBuresWasserstein`].
    initialize = function(n, py_cls = NULL) {
      if (is.null(py_cls)) {
        n <- as.integer(n)
        py_cls <- gs$geometry$spd_matrices$SPDMetricBuresWasserstein(n = n)
      }
      super$set_python_class(py_cls)
      private$set_fields()
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
      self$n <- super$get_python_class()$n
    }
  )
)
