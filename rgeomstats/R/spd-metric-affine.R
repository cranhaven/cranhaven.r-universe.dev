#' Class for the Affine Metric on the Manifold of Symmetric Positive Definite
#' Matrices
#'
#' @description An [R6::R6Class] object implementing the [`SPDMetricAffine`]
#'   class. This is the class for the affine-invariant metric on the SPD
#'   manifold \insertCite{thanwerdas2019affine}{rgeomstats}.
#'
#' @references
#' \insertAllCited{}
#'
#' @author Yann Thanwerdas
#'
#' @export
SPDMetricAffine <- R6::R6Class(
  classname = "SPDMetricAffine",
  inherit = RiemannianMetric,
  public = list(
    #' @field n An integer value specifying the shape of the matrices: \eqn{n
    #'   \times n}.
    n = NULL,

    #' @field power_affine An integer value specifying the power transformation
    #'   of the classical SPD metric.
    power_affine = NULL,

    #' @description The [`SPDMetricAffine`] class constructor.
    #'
    #' @param n An integer value specifying the shape of the matrices: \eqn{n
    #'   \times n}.
    #' @param power_affine An integer value specifying the power transformation
    #'   of the classical SPD metric. Defaults to `1L`.
    #' @param py_cls A Python object of class `SPDMetricAffine`. Defaults to
    #'   `NULL` in which case it is instantiated on the fly using the other
    #'   input arguments.
    #'
    #' @return An object of class [`SPDMetricAffine`].
    initialize = function(n, power_affine = 1, py_cls = NULL) {
      if (is.null(py_cls)) {
        n <- as.integer(n)
        power_affine <- as.integer(power_affine)
        py_cls <- gs$geometry$spd_matrices$SPDMetricAffine(
          n = n,
          power_affine = power_affine
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
      self$power_affine <- super$get_python_class()$power_affine
    }
  )
)
