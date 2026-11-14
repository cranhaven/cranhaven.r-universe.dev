#' Class for the Euclidean Metric on the Manifold of Symmetric Positive Definite
#' Matrices
#'
#' @description An [R6::R6Class] object implementing the [`SPDMetricEuclidean`]
#'   class. This is the class for the Euclidean metric on the SPD manifold.
#'
#' @author Yann Thanwerdas
#'
#' @export
SPDMetricEuclidean <- R6::R6Class(
  classname = "SPDMetricEuclidean",
  inherit = RiemannianMetric,
  public = list(
    #' @field n An integer value specifying the shape of the matrices: \eqn{n
    #'   \times n}.
    n = NULL,

    #' @field power_euclidean An integer value specifying the power
    #'   transformation of the classical SPD metric.
    power_euclidean = NULL,

    #' @description The [`SPDMetricEuclidean`] class constructor.
    #'
    #' @param n An integer value specifying the shape of the matrices: \eqn{n
    #'   \times n}.
    #' @param power_euclidean An integer value specifying the power
    #'   transformation of the classical SPD metric. Defaults to `1L`.
    #' @param py_cls A Python object of class `SPDMetricEuclidean`. Defaults to
    #'   `NULL` in which case it is instantiated on the fly using the other
    #'   input arguments.
    #'
    #' @return An object of class [`SPDMetricEuclidean`].
    initialize = function(n, power_euclidean = 1, py_cls = NULL) {
      if (is.null(py_cls)) {
        n <- as.integer(n)
        power_euclidean <- as.integer(power_euclidean)
        py_cls <- gs$geometry$spd_matrices$SPDMetricEuclidean(
          n = n,
          power_euclidean = power_euclidean
        )
      }
      super$set_python_class(py_cls)
      private$set_fields()
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
      self$n <- super$get_python_class()$n
      self$power_euclidean <- super$get_python_class()$power_euclidean
    }
  )
)
