#' Abstract Class for Riemannian Metrics
#'
#' @description An [R6::R6Class] object implementing the base
#'   [`RiemannianMetric`] class. This is an abstract class for Riemannian and
#'   pseudo-Riemannian metrics which are the associated Levi-Civita connection
#'   on the tangent bundle.
#'
#' @param base_point A numeric array of shape `dim` specifying a point on
#'   the manifold. Defaults to `NULL`.
#' @param vector A numeric array of shape `dim` specifying a vector.
#'
#' @author Nina Miolane
#'
#' @keywords internal
RiemannianMetric <- R6::R6Class(
  classname = "RiemannianMetric",
  inherit = Connection,
  public = list(
    #' @field signature An integer vector specifying the signature of the
    #'   metric.
    signature = NULL,

    #' @description The [`RiemannianMetric`] class constructor.
    #'
    #' @param dim An integer value specifying the dimension of the manifold.
    #' @param shape An integer vector specifying the shape of one element of the
    #'   manifold. Defaults to `NULL`.
    #' @param signature An integer vector specifying the signature of the
    #'   metric. Defaults to `c(dim, 0L)`.
    #' @param default_coords_type A string specifying the coordinate type.
    #'   Choices are `extrensic` or `intrinsic`. Defaults to `intrinsic`.
    #' @param py_cls A Python object of class `RiemannianMetric`. Defaults to
    #'   `NULL` in which case it is instantiated on the fly using the other
    #'   input arguments.
    #'
    #' @return An object of class [`RiemannianMetric`].
    initialize = function(dim, shape = NULL, signature = NULL,
                          default_coords_type = "intrinsic", py_cls = NULL) {
      if (is.null(py_cls)) {
        dim <- as.integer(dim)
        if (!is.null(shape)) shape <- dim
        if (!is.null(signature)) signature <- as.integer(signature)
        py_cls <- gs$geometry$riemannian_metric$RiemannianMetric(
          dim = dim,
          shape = shape,
          signature = signature,
          default_coords_type = default_coords_type
        )
      }
      super$set_python_class(py_cls)
      private$set_fields()
    },

    #' @description Metric matrix at the tangent space at a base point.
    #'
    #' @return A numeric array of shape `dim x dim` storing the inner-product
    #'   matrix.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   # mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   # mt$metric_matrix()
    #' }
    metric_matrix = function(base_point = NULL) {
      super$get_python_class()$metric_matrix(base_point)
    },

    #' @description Inner co-product matrix at the cotangent space at a base
    #'   point. This represents the cometric matrix, i.e. the inverse of the
    #'   metric matrix.
    #'
    #' @return A numeric array of shape `dim x dim` storing the inverse of the
    #'   inner-product matrix.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   # mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   # mt$cometric_matrix()
    #' }
    cometric_matrix = function(base_point = NULL) {
      super$get_python_class()$cometric_matrix(base_point)
    },

    #' @description Compute derivative of the inner prod matrix at base point.
    #'
    #' @return A numeric array of shape `dim x dim` storing the derivative of
    #'   the inverse of the inner-product matrix.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   # mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   # mt$inner_product_derivative_matrix()
    #' }
    inner_product_derivative_matrix = function(base_point) {
      super$get_python_class()$inner_product_derivative_matrix(base_point)
    },

    #' @description Inner product between two tangent vectors at a base point.
    #'
    #' @param tangent_vec_a A numeric array of shape `dim` specifying a tangent
    #'   vector at base point.
    #' @param tangent_vec_b A numeric array of shape `dim` specifying a tangent
    #'   vector at base point.
    #'
    #' @return A scalar value representing the inner product between the two
    #'   input tangent vectors at the input base point.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   mt$inner_product(diag(0, 3), diag(1, 3), base_point = diag(1, 3))
    #' }
    inner_product = function(tangent_vec_a, tangent_vec_b, base_point) {
      super$get_python_class()$inner_product(
        tangent_vec_a = tangent_vec_a,
        tangent_vec_b = tangent_vec_b,
        base_point = base_point
      )
    },

    #' @description Computes inner coproduct between two cotangent vectors at
    #'   base point. This is the inner product associated to the cometric
    #'   matrix.
    #'
    #' @param cotangent_vec_a A numeric array of shape `dim` specifying a
    #'   cotangent vector at base point.
    #' @param cotangent_vec_b A numeric array of shape `dim` specifying a
    #'   cotangent vector at base point.
    #'
    #' @return A scalar value representing the inner coproduct between the two
    #'   input cotangent vectors at the input base point.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   # mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   # mt$inner_coproduct(diag(0, 3), diag(1, 3), base_point = diag(1, 3))
    #' }
    inner_coproduct = function(cotangent_vec_a, cotangent_vec_b, base_point = NULL) {
      super$get_python_class()$inner_coproduct(
        cotangent_vec_a = cotangent_vec_a,
        cotangent_vec_b = cotangent_vec_b,
        base_point = base_point
      )
    },

    #' @description Computes the Hamiltonian energy associated to the cometric.
    #'   The Hamiltonian at state \eqn{(q, p)} is defined by \deqn{H(q, p) =
    #'   \frac{1}{2} \langle p, p \rangle_q,} where \eqn{\langle \cdot, \cdot
    #'   \rangle_q} is the cometric at \eqn{q}.
    #'
    #' @param state A list with two components: (i) a numeric array of shape
    #'   `dim` specifying the *position* which is a point on the manifold and
    #'   (ii) a numeric array of shape `dim` specifying the *momentum* which is
    #'   a cotangent vector.
    #'
    #' @return A numeric value representing the Hamiltonian energy at `state`.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   # mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   # mt$hamiltonian()
    #' }
    hamiltonian = function(state) {
      super$get_python_class()$hamiltonian(state)
    },

    #' @description Computes the square of the norm of a vector. Squared norm of
    #'   a vector associated to the inner product at the tangent space at a base
    #'   point.
    #'
    #' @return A numeric value representing the squared norm of the input
    #'   vector.
    #'
    #' if (reticulate::py_module_available("geomstats")) {
    #'   mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   mt$squared_norm(diag(0, 3), diag(1, 3))
    #' }
    squared_norm = function(vector, base_point = NULL) {
      super$get_python_class()$squared_norm(
        vector = vector,
        base_point = base_point
      )
    },

    #' @description Computes the norm of a vector associated to the inner
    #'   product at the tangent space at a base point.
    #'
    #' @details This only works for positive-definite Riemannian metrics and
    #'   inner products.
    #'
    #' @return A numeric value representing the norm of the input vector.
    #'
    #' if (reticulate::py_module_available("geomstats")) {
    #'   mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   mt$norm(diag(0, 3), diag(1, 3))
    #' }
    norm = function(vector, base_point = NULL) {
      super$get_python_class()$norm(
        vector = vector,
        base_point = base_point
      )
    },

    #' @description Normalizes a tangent vector at a given point.
    #'
    #' @return A numeric array of shape `dim` storing the normalized version of
    #'   the input tangent vector.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   # mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   # mt$normalize(diag(2, 3), diag(1, 3))
    #' }
    normalize = function(vector, base_point = NULL) {
      super$get_python_class()$normalize(
        vector = vector,
        base_point = base_point
      )
    },

    #' @description Generates a random unit tangent vector at a given point.
    #'
    #' @param n_vectors An integer value specifying the number of vectors to be
    #'   generated at `base_point`. For vectorization purposes, `n_vectors` can
    #'   be greater than 1 *iff* `base_point` corresponds to a single point.
    #'   Defaults to `1L`.
    #'
    #' @return A numeric array of shape `c(n_vectors, dim)` storing random unit
    #'   tangent vectors at `base_point`.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   # mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   # mt$random_unit_tangent_vec(diag(1, 3))
    #' }
    random_unit_tangent_vec = function(base_point = NULL, n_vectors = 1) {
      n_vectors <- as.integer(n_vectors)
      super$get_python_class()$random_unit_tangent_vec(
        base_point = base_point,
        n_vectors = n_vectors
      )
    },

    #' @description Squared geodesic distance between two points.
    #'
    #' @param point_a A numeric array of shape `dim` on the manifold.
    #' @param point_b A numeric array of shape `dim` on the manifold.
    #' @param ... Extra parameters to be passed to the `$log()` method of the
    #'   parent [`Connection`] class.
    #'
    #' @return A numeric value storing the squared geodesic distance between the
    #'   two input points.
    #'
    #' if (reticulate::py_module_available("geomstats")) {
    #'   mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   mt$squared_dist(diag(1, 3), diag(1, 3))
    #' }
    squared_dist = function(point_a, point_b, ...) {
      dots <- capture_extra_params(...)
      dots$point_a <- point_a
      dots$point_b <- point_b
      do.call(super$get_python_class()$squared_dist, dots)
    },

    #' @description Geodesic distance between two points.
    #'
    #' @details It only works for positive definite Riemannian metrics.
    #'
    #' @param point_a A numeric array of shape `dim` on the manifold.
    #' @param point_b A numeric array of shape `dim` on the manifold.
    #' @param ... Extra parameters to be passed to the `$log()` method of the
    #'   parent [`Connection`] class.
    #'
    #' @return A numeric value storing the geodesic distance between the two
    #'   input points.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   mt$dist(diag(1, 3), diag(1, 3))
    #' }
    dist = function(point_a, point_b, ...) {
      dots <- capture_extra_params(...)
      dots$point_a <- point_a
      dots$point_b <- point_b
      do.call(super$get_python_class()$dist, dots)
    },

    #' @description Computes the geodesic distance between points.
    #'
    #' @details If `n_samples_a == n_samples_b` then `dist` is the element-wise
    #'   distance result of a point in `points_a` with the point from `points_b`
    #'   of the same index. If `n_samples_a != n_samples_b` then `dist` is the
    #'   result of applying geodesic distance for each point from `points_a` to
    #'   all points from `points_b`.
    #'
    #' @param points_a A numeric array of shape `c(n_samples_a, dim)` specifying
    #'   a set of points on the manifold.
    #' @param points_b A numeric array of shape `c(n_samples_b, dim)` specifying
    #'   a set of points on the manifold.
    #'
    #' @return A numeric array of shape `c(n_samples_a, dim)` if `n_samples_a ==
    #'   n_samples_b` or of shape `c(n_samples_a, n_samples_b, dim)` if
    #'   `n_samples_a != n_samples_b` storing the geodesic distance between
    #'   points in set A and points in set B.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   mt <- SPDMetricLogEuclidean$new(n = 3)
    #'   mt$dist(diag(1, 3), diag(1, 3))
    #' }
    dist_broadcast = function(points_a, points_b) {
      super$get_python_class()$dist_broadcast(
        point_a = points_a,
        point_b = points_b
      )
    },

    #' @description Computes the pairwise distance between points.
    #'
    #' @param points A numeric array of shape `c(n_samples, dim)` specifying a
    #'   set of points on the manifold.
    #' @param n_jobs An integer value specifying the number of cores for
    #'   parallel computation. Defaults to `1L`.
    #' @param ... Extra parameters to be passed tothe  `joblib.Parallel` Python
    #'   class. See [joblib
    #'   documentation](https://joblib.readthedocs.io/en/latest/) for details.
    #'
    #' @return A numeric matrix of shape `c(n_samples, n_samples)` storing the
    #'   pairwise geodesic distances between all the points.
    dist_pairwise = function(points, n_jobs = 1, ...) {
      dots <- capture_extra_params(...)
      dots$points <- points
      dots$n_jobs <- as.integer(n_jobs)
      do.call(super$get_python_class()$dist_pairwise, dots)
    },

    #' @description Computes the diameter of set of points on a manifold.
    #'
    #' @details The diameter is the maximum over all pairwise distances.
    #'
    #' @param points A numeric array of shape `c(n_samples, dim)` specifying a
    #'   set of points on the manifold.
    #'
    #' @return A numeric value representing the largest distance between any two
    #'   points in the input set.
    diameter = function(points) {
      super$get_python_class()$diameter(points)
    },

    #' @description Finds the closest neighbor to a point among a set of
    #'   neighbors.
    #'
    #' @param point A numeric array of shape `dim` specifying a point on the
    #'   manifold.
    #' @param neighbors A numeric array of shape `c(n_neighbors, dim)`
    #'   specifying a set of neighboring points for the input `point`.
    #'
    #' @return An integer value representing the index of the neighbor in
    #'   `neighbors` that is closest to `point`.
    closest_neighbor_index = function(point, neighbors) {
      super$get_python_class()$closest_neighbor_index(
        point = point,
        neighbors = neighbors
      )
    },

    #' @description Normalizes the basis with respect to the metric. This
    #'   corresponds to a renormalization of each basis vector.
    #'
    #' @param basis A numeric array of shape `c(dim, dim)` specifying a basis.
    #'
    #' @return A numeric array of shape `c(dim, n, n)` storing the normal basis.
    normal_basis = function(basis, base_point = NULL) {
      super$get_python_class()$normal_basis(
        basis = basis,
        base_point = base_point
      )
    },

    #' @description Computes the sectional curvature.
    #'
    #' @details For two orthonormal tangent vectors \eqn{x} and \eqn{y} at a
    #'   base point, the sectional curvature is defined by \deqn{\langle R(x,
    #'   y)x, y \rangle = \langle R_x(y), y \rangle.} For non-orthonormal
    #'   vectors, it is \deqn{\langle R(x, y)x, y \rangle / \\|x \wedge y\\|^2.}
    #'   See also https://en.wikipedia.org/wiki/Sectional_curvature.
    #'
    #' @param tangent_vec_a A numeric array of shape `c(n, n)` specifying a
    #'   tangent vector at `base_point`.
    #' @param tangent_vec_b A numeric array of shape `c(n, n)` specifying a
    #'   tangent vector at `base_point`.
    #'
    #' @return A numeric value representing the sectional curvature at
    #'   `base_point`.
    sectional_curvature = function(tangent_vec_a, tangent_vec_b, base_point = NULL) {
      super$get_python_class()$sectional_curvature(
        tangent_vec_a = tangent_vec_a,
        tangent_vec_b = tangent_vec_b,
        base_point = base_point
      )
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
      self$signature <- super$get_python_class()$signature
    }
  )
)
