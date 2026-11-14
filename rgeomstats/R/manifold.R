#' Abstract Class for Manifolds
#'
#' @description An [R6::R6Class] object implementing the base [`Manifold`]
#'   class. In other words, a topological space that locally resembles Euclidean
#'   space near each point.
#'
#' @author Nina Miolane
#'
#' @keywords internal
Manifold <- R6::R6Class(
  classname = "Manifold",
  inherit = PythonClass,
  public = list(
    #' @field dim An integer value specifying the dimension of the manifold.
    dim = NULL,

    #' @field shape An integer vector specifying the shape of one element of the
    #'   manifold. Defaults to `NULL`.
    shape = NULL,

    #' @field metric A [RiemannianMetric] object specifying the metric to use on
    #'   the manifold. Defaults to `NULL`.
    metric = NULL,

    #' @field default_coords_type A string specifying the coordinate type.
    #'   Choices are `extrensic` or `intrinsic`. Dedaults to `intrinsic`.
    default_coords_type = NULL,

    #' @field default_point_type A string specifying the point type. Choices are
    #'   `vector` or `matrix`. It is automatically determined depending on the
    #'   manifold.
    default_point_type = NULL,

    #' @description The [`Manifold`] class constructor.
    #'
    #' @param dim An integer value specifying the dimension of the manifold.
    #' @param shape An integer vector specifying the shape of one element of the
    #'   manifold. Defaults to `NULL`.
    #' @param metric A [`RiemannianMetric`] object specifying the metric to use
    #'   on the manifold. Defaults to `NULL`.
    #' @param default_coords_type A string specifying the coordinate type.
    #'   Choices are `extrinsic` or `intrinsic`. Defaults to `intrinsic`.
    #' @param py_cls A Python object of class `Manifold`. Defaults to `NULL` in
    #'   which case it is instantiated on the fly using the other input
    #'   arguments.
    #'
    #' @return An object of class [`Manifold`].
    initialize = function(dim, shape = NULL, metric = NULL, default_coords_type = "intrinsic", py_cls = NULL) { # nocov start
      if (is.null(py_cls)) {
        dim <- as.integer(dim)
        if (!is.null(shape)) {
          shape <- shape |>
            purrr::map(as.integer) |>
            reticulate::tuple()
        }
        if (!is.null(metric))
          metric <- metric$get_python_class()
        default_coords_type <- match.arg(default_coords_type, c("intrinsic", "extrinsic"))
        py_cls <- gs$geometry$manifold$Manifold(
          dim = dim,
          shape = shape,
          metric = metric,
          default_coords_type = default_coords_type
        )
      }
      super$set_python_class(py_cls)
      private$set_fields()
    }, # nocov end

    #' @description Evaluates if a point belongs to the manifold.
    #'
    #' @param point A numeric array of shape \eqn{[\dots \times
    #'   \{\mathrm{dim}\}]} specifying one or more points to be checked.
    #' @param atol A numeric value specifying the absolute tolerance for
    #'   checking. Defaults to `gs$backend$atol`.
    #'
    #' @return A boolean value or vector storing whether the corresponding
    #'   points belong to the manifold.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   A <- diag(1, 3)
    #'   spd3$belongs(diag(1, 3))
    #' }
    belongs = function(point, atol = gs$backend$atol) {
      super$get_python_class()$belongs(point, atol = atol)
    },

    #' @description Checks whether a vector is tangent at a base point.
    #'
    #' @param vector A numeric array of shape \eqn{[\dots \times
    #'   [\mathrm{dim}]]} specifying one or more vectors to be checked.
    #' @param base_point A numeric array of shape \eqn{[\dots \times
    #'   [\mathrm{dim}]]} specifying one or more base points on the manifold.
    #'   Defaults to `NULL` in which case the identity is used.
    #' @param atol A numeric value specifying the absolute tolerance for
    #'   checking. Defaults to `gs$backend$atol`.
    #'
    #' @return A boolean value or vector storing whether the corresponding
    #'   points are tangent to the manifold at corresponding base points.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   A <- diag(1, 3)
    #'   spd3$is_tangent(diag(1, 3))
    #' }
    is_tangent = function(vector, base_point = NULL, atol = gs$backend$atol) {
      super$get_python_class()$is_tangent(
        vector = vector,
        base_point = base_point,
        atol = atol
      )
    },

    #' @description Projects a vector to a tangent space of the manifold.
    #'
    #' @param vector A numeric array of shape \eqn{[\dots \times
    #'   [\mathrm{dim}]]} specifying one or more vectors to project on the
    #'   manifold.
    #' @param base_point A numeric array of shape \eqn{[\dots \times
    #'   [\mathrm{dim}]]} specifying one or more base points on the manifold.
    #'   Defaults to `NULL` in which case the identity is used.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{\mathrm{dim}\}]}
    #'   storing the corresponding projections onto the manifold at
    #'   corresponding base points.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   A <- diag(1, 3)
    #'   spd3$to_tangent(diag(1, 3))
    #' }
    to_tangent = function(vector, base_point = NULL) {
      super$get_python_class()$to_tangent(
        vector = vector,
        base_point = base_point
      )
    },

    #' @description Samples random points on the manifold.
    #'
    #' @details If the manifold is compact, a uniform distribution is used.
    #'
    #' @param n_samples An integer value specifying the number of samples to be
    #'   drawn. Defaults to `1L`.
    #' @param bound A numeric value specifying the bound of the interval in
    #'   which to sample for non-compact manifolds. Defaults to `1L`.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{\mathrm{dim}\}]}
    #'   storing a sample of points on the manifold.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   # spd3$random_point(10) # TO DO: uncomment when bug fixed in gs
    #' }
    random_point = function(n_samples = 1, bound = 1.0) {
      super$get_python_class()$random_point(
        n_samples = as.integer(n_samples),
        bound = bound
      )
    },

    #' @description Regularizes a point to the canonical representation for the
    #'   manifold.
    #'
    #' @param point A numeric array of shape \eqn{[\dots \times
    #'   [\mathrm{dim}]]} specifying one or more points on the manifold.
    #'
    #' @return A numeric array of the same shape storing the corresponding
    #'   regularized points.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   A <- diag(1, 3)
    #'   spd3$regularize(diag(1, 3))
    #' }
    regularize = function(point) {
      super$get_python_class()$regularize(
        point = point
      )
    },

    #' @description Sets the Riemannian Metric associated to the manifold.
    #'
    #' @param metric An object of class [`RiemannianMetric`].
    #'
    #' @return The [Manifold] class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   spd3$metric
    #'   spd3$set_metric(SPDMetricBuresWasserstein$new(n = 3))
    #'   spd3$metric
    #' }
    set_metric = function(metric) {
      pc <- super$get_python_class()
      pc$metric <- metric$get_python_class()
      private$set_fields()
      invisible(self)
    },

    #' @description Generates a random tangent vector.
    #'
    #' @param base_point A numeric array of shape \eqn{[\dots \times
    #'   \{\mathrm{dim}\}]} specifying one or more base points on the manifold.
    #' @param n_samples An integer value specifying the number of samples to be
    #'   drawn. Defaults to `1L`.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{\mathrm{dim}\}]}
    #'   storing a sample of vectors that are tangent to the manifold at
    #'   corresponding base points.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   spd3$random_tangent_vec(diag(1, 3), 10)
    #' }
    random_tangent_vec = function(base_point, n_samples = 1) {
      n_samples <- as.integer(n_samples)
      super$get_python_class()$random_tangent_vec(
        base_point = base_point,
        n_samples = n_samples
      )
    }
  ),
  private = list(
    set_fields = function() {
      self$dim <- super$get_python_class()$dim
      self$shape <- super$get_python_class()$shape
      self$metric <- super$get_python_class()$metric
      self$default_coords_type <- super$get_python_class()$default_coords_type
      self$default_point_type <- super$get_python_class()$default_point_type
    }
  )
)
