#' Class for the log-Euclidean Metric on the Manifold of Symmetric Positive
#' Definite Matrices
#'
#' @description An [R6::R6Class] object implementing the
#'   [`SPDMetricLogEuclidean`] class. This is the class for the
#'   log-Euclidean metric on the SPD manifold.
#'
#' @author Yann Thanwerdas
#'
#' @export
SPDMetricLogEuclidean <- R6::R6Class(
  classname = "SPDMetricLogEuclidean",
  inherit = RiemannianMetric,
  public = list(
    #' @field n An integer value specifying the shape of the matrices: \eqn{n
    #'   \times n}.
    n = NULL,

    #' @description The [`SPDMetricLogEuclidean`] class constructor.
    #'
    #' @param n An integer value specifying the shape of the matrices: \eqn{n
    #'   \times n}.
    #' @param py_cls A Python object of class `SPDMetricLogEuclidean`. Defaults
    #'   to `NULL` in which case it is instantiated on the fly using the other
    #'   input arguments.
    #'
    #' @return An object of class [`SPDMetricLogEuclidean`].
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   mt
    #' }
    initialize = function(n, py_cls = NULL) {
      if (is.null(py_cls)) {
        n <- as.integer(n)
        py_cls <- gs$geometry$spd_matrices$SPDMetricLogEuclidean(n = n)
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
