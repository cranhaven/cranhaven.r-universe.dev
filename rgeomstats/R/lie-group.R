#' Abstract Class for Matrix Lie Groups
#'
#' @description Class for matrix Lie groups.
#'
#' @param base_point A numeric array of shape \eqn{[\dots \times n \times n]}
#'   specifying one or more base points on the manifold. Defaults to identity if
#'   `NULL`.
#'
#' @author Nina Miolane
#'
#' @keywords internal
MatrixLieGroup <- R6::R6Class(
  classname = "MatrixLieGroup",
  inherit = Manifold,
  public = list(
    #' @field lie_algebra An object of class [`MatrixLieAlgebra`] or `NULL`
    #'   representing the tangent space at the identity.
    lie_algebra = NULL,

    #' @field n The size of the \eqn{n \times n} matrix elements.
    n = NULL,

    #' @field left_canonical_metric An object of class `InvariantMetric`
    #'   representing the left invariant metric that corresponds to the
    #'   Euclidean inner product at the identity.
    left_canonical_metric = NULL,

    #' @field right_canonical_metric An object of class `InvariantMetric`
    #'   representing the left invariant metric that corresponds to the
    #'   Euclidean inner product at the identity.
    right_canonical_metric = NULL,

    #' @description The [`MatrixLieGroup`] class constructor.
    #'
    #' @param dim An integer value specifying the dimension of the manifold.
    #' @param n The size of the \eqn{n \times n} matrix elements.
    #' @param lie_algebra An object of class [`MatrixLieAlgebra`] or `NULL`
    #'   representing the tangent space at the identity.
    #' @param ... Extra arguments to be passed to parent class constructors. See
    #'   [`Manifold`] class.
    #' @param py_cls A Python object of class `MatrixLieGroup`. Defaults to
    #'   `NULL` in which case it is instantiated on the fly using the other
    #'   input arguments.
    #'
    #' @return An object of class [`MatrixLieGroup`].
    initialize = function(dim,
                          n,
                          lie_algebra = NULL,
                          ...,
                          py_cls = NULL) { # nocov start
      if (is.null(py_cls)) {
        dots <- capture_extra_params(...)
        dots$dim <- as.integer(dim)
        dots$n <- as.integer(n)
        if (!is.null(lie_algebra))
          dots$lie_algebra <- lie_algebra$get_python_class()
        py_cls <- do.call(gs$geometry$lie_group$MatrixLieGroup, dots)
      }
      super$set_python_class(py_cls)
      private$set_fields()
    }, # nocov end

    #' @description Exponentiates a left-invariant vector field from a base
    #'   point.
    #'
    #' @details The vector input is not an element of the Lie algebra, but of
    #'   the tangent space at `base_point`: if \eqn{g} denotes `base_point`,
    #'   \eqn{v} the tangent vector, and \eqn{V = g^{-1} v} the associated Lie
    #'   algebra vector, then \deqn{\exp(v, g) = \mathrm{mul}(g, \exp(V))}.
    #'   Therefore, the Lie exponential is obtained when `base_point` is `NULL`,
    #'   or the identity.
    #'
    #' @param tangent_vec A numeric array of shape \eqn{[\dots \times n \times
    #'   n]} specifying one or more tangent vectors at corresponding base
    #'   points.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times n \times n]} storing
    #'   the left multiplication of the Lie exponential of the input tangent
    #'   vectors with the corresponding base points.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3)
    #'   # so3$exp(diag(1, 3)) # TO DO: fix in gs
    #' }
    exp = function(tangent_vec, base_point = NULL) {
      super$get_python_class()$exp(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Computes a left-invariant vector field bringing `base_point`
    #'   to `point`.
    #'
    #' @details The output is a vector of the tangent space at `base_point`, so
    #'   not a Lie algebra element if `base_point` is not the identity.
    #'   Furthermore, denoting `point` by \eqn{g} and `base_point` by \eqn{h},
    #'   the output satisfies \deqn{g = \exp(\log(g, h), h)}.
    #'
    #' @param point A numeric array of shape \eqn{[\dots \times n \times n]}
    #'   specifying one or more points.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times n \times n]} such
    #'   that its Lie exponential at corresponding base points matches
    #'   corresponding points.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3)
    #'   so3$log(diag(1, 3))
    #' }
    log = function(point, base_point = NULL) {
      super$get_python_class()$log(
        point = point,
        base_point = base_point
      )
    },

    #' @description Gets the identity of the group.
    #'
    #' @return A numeric array of shape \eqn{n \times n} storing the identity
    #'   of the Lie group.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3)
    #'   so3$get_identity()
    #' }
    get_identity = function() {
      super$get_python_class()$identity
    },

    #' @description Computes the lie bracket of two tangent vectors.
    #'
    #' @details For matrix Lie groups with tangent vectors \eqn{A} and \eqn{B}
    #'   at the same base point \eqn{P}, this is given by (translate to
    #'   identity, compute commutator, go back): \deqn{[A,B] = A_P^{-1}B -
    #'   B_P^{-1}A}.
    #'
    #' @param tangent_vector_a A numeric array of shape \eqn{[\dots \times n
    #'   \times n]} specifying one or more tangent vectors at corresponding base
    #'   points.
    #' @param tangent_vector_b A numeric array of shape \eqn{[\dots \times n
    #'   \times n]} specifying one or more tangent vectors at corresponding base
    #'   points.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times n \times n]} storing
    #'   the Lie bracket of the two input tangent vectors.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3)
    #'   so3$lie_bracket(diag(0, 3), diag(1, 3))
    #' }
    lie_bracket = function(tangent_vector_a, tangent_vector_b, base_point = NULL) {
      super$get_python_class()$lie_bracket(
        tangent_vector_a = tangent_vector_a,
        tangent_vector_b = tangent_vector_b,
        base_point = base_point
      )
    },

    #' @description Computes the push-forward map by the left/right translation.
    #'
    #' @details Computes the push-forward map of the left/right translation by
    #'   the point. It corresponds to the tangent map, or differential of the
    #'   group multiplication by the point or its inverse. For groups with a
    #'   vector representation, it is only implemented at identity, but it can
    #'   be used at other points with `inverse = TRUE`. This method wraps the
    #'   Jacobian translation which actually computes the matrix representation
    #'   of the map.
    #'
    #' @param point A numeric array of shape \eqn{[\dots \times \{ \mathrm{dim},
    #'   [n \times n] \} ]} specifying one or more points at which to compute
    #'   the map.
    #' @param left_or_right A character string specifying whether to compute the
    #'   map for the left or right translation. Choices are `"left"` or
    #'   `"right`. Defaults to `"left"`.
    #' @param inverse A boolean specifying whether to inverse the Jacobian
    #'   matrix. If set to `TRUE`, the push forward by the translation by the
    #'   inverse of the point is returned. Defaults to `FALSE`.
    #'
    #' @return A function taking as argument a numeric array `tangent_vec` of
    #'   shape \eqn{[\dots \times \{ \mathrm{dim}, [n \times n] \} ]} specifying
    #'   one or more tangent vectors and returning a numeric array of shape
    #'   \eqn{[\dots \times \{ \mathrm{dim}, [n \times n] \} ]} storing the
    #'   result of the tangent mapping of the left/right translation of input
    #'   tangent points by corresponding base points.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3)
    #'   tangent_map <- so3$tangent_translation_map(diag(1, 3))
    #'   tangent_map(diag(1, 3))
    #' }
    tangent_translation_map = function(point, left_or_right = "left", inverse = FALSE) {
      tm <- super$get_python_class()$tangent_translation_map(
        point = point,
        left_or_right = left_or_right,
        inverse = inverse
      )
      function(tangent_vec) {
        tm |>
          reticulate::py_call(tangent_vec) |>
          reticulate::py_to_r()
      }
    },

    #' @description Performs function composition corresponding to the Lie
    #'   group.
    #'
    #' @param point_a A numeric array of shape \eqn{[\dots \times \{
    #'   \mathrm{dim}, n \times n \}]} specifying one or more left factors in
    #'   the product.
    #' @param point_b A numeric array of shape \eqn{[\dots \times \{
    #'   \mathrm{dim}, n \times n \}]} specifying one or more right factors in
    #'   the product.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{ \mathrm{dim}, n
    #'   \times n \}]} storing the product of `point_a` and `point_b` along the
    #'   first dimension.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3)
    #'   so3$compose(diag(1, 3), diag(1, 3))
    #' }
    compose = function(point_a, point_b) {
      super$get_python_class()$compose(
        point_a = point_a,
        point_b = point_b
      )
    },

    #' @description Computes the inverse law of the Lie group.
    #'
    #' @param point A numeric array of shape \eqn{[\dots \times \{ \mathrm{dim},
    #'   n \times n \}]} specifying one or more points to be inverted.
    #'
    #' @return A numeric array of the same shape storing the inverted points.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3)
    #'   so3$inverse(diag(1, 3))
    #' }
    inverse = function(point) {
      super$get_python_class()$inverse(point = point)
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
      self$lie_algebra            <- super$get_python_class()$lie_algebra
      self$n                      <- super$get_python_class()$n
      self$left_canonical_metric  <- super$get_python_class()$left_canonical_metric
      self$right_canonical_metric <- super$get_python_class()$right_canonical_metric
    }
  )
)

#' Abstract Class for Lie Groups
#'
#' @description Class for Lie groups. In this class, `point_type` (`'vector'` or
#'   `'matrix'`) will be used to describe the format of the points on the Lie
#'   group. If `point_type` is `'vector'`, the format of the inputs is
#'   `dimension`, where `dimension` is the dimension of the Lie group. If
#'   `point_type` is `'matrix'`, the format of the inputs is `c(n, n)` where `n`
#'   is the parameter of \eqn{\mathrm{GL}(n)} e.g. the amount of rows and
#'   columns of the matrix.
#'
#' @param base_point A numeric array of shape \eqn{[\dots \times \{
#'   \mathrm{dim}, [n \times n] \}]} specifying one or more base points on the
#'   manifold. Defaults to identity if `NULL`.
#' @param point A numeric array of shape \eqn{[\dots \times \{
#'   \mathrm{dim}, [n \times n] \}]} specifying one or more points on the
#'   manifold.
#' @param tangent_vec A numeric array of shape \eqn{[\dots \times \{
#'   \mathrm{dim}, [n \times n] \}]} specifying one or more tangent vectors at
#'   corresponding base points.
#' @param left_or_right A character string specifying whether to compute the map
#'   for the left or right translation. Choices are `"left"` or `"right`.
#'   Defaults to `"left"`.
#'
#' @author Nina Miolane
#'
#' @keywords internal
LieGroup <- R6::R6Class(
  classname = "LieGroup",
  inherit = Manifold,
  public = list(
    #' @field lie_algebra An object of class [`MatrixLieAlgebra`] or `NULL`
    #'   representing the tangent space at the identity.
    lie_algebra = NULL,

    #' @field left_canonical_metric An object of class `InvariantMetric`
    #'   representing the left invariant metric that corresponds to the
    #'   Euclidean inner product at the identity.
    left_canonical_metric = NULL,

    #' @field right_canonical_metric An object of class `InvariantMetric`
    #'   representing the left invariant metric that corresponds to the
    #'   Euclidean inner product at the identity.
    right_canonical_metric = NULL,

    #' @field metrics A list of objects of class [`RiemannianMetric`].
    metrics = NULL,

    #' @description The [`LieGroup`] class constructor.
    #'
    #' @param dim An integer value specifying the dimension of the manifold.
    #' @param shape An integer vector specifying the shape of one element of the
    #'   Lie group.
    #' @param lie_algebra An object of class [`MatrixLieAlgebra`] or `NULL`
    #'   specifying the tangent space at the identity.
    #' @param ... Extra arguments to be passed to parent class constructors. See
    #'   [`Manifold`] class.
    #' @param py_cls A Python object of class `LieGroup`. Defaults to `NULL` in
    #'   which case it is instantiated on the fly using the other input
    #'   arguments.
    #'
    #' @return An object of class [`LieGroup`].
    initialize = function(dim,
                          shape,
                          lie_algebra = NULL,
                          ...,
                          py_cls = NULL) { # nocov start
      if (is.null(py_cls)) {
        dots <- capture_extra_params(...)
        dots$dim <- as.integer(dim)
        dots$shape <- shape |>
          purrr::map(as.integer) |>
          reticulate::tuple()
        if (!is.null(lie_algebra))
          dots$lie_algebra <- lie_algebra$get_python_class()
        py_cls <- do.call(gs$geometry$lie_group$LieGroup, dots)
      }
      super$set_python_class(py_cls)
      private$set_fields()
    }, # nocov end

    #' @description Exponentiates a left-invariant vector field from a base
    #'   point.
    #'
    #' @details The vector input is not an element of the Lie algebra, but of
    #'   the tangent space at `base_point`: if \eqn{g} denotes `base_point`,
    #'   \eqn{v} the tangent vector, and \eqn{V = g^{-1} v} the associated Lie
    #'   algebra vector, then \deqn{\exp(v, g) = \mathrm{mul}(g, \exp(V))}.
    #'   Therefore, the Lie exponential is obtained when `base_point` is `NULL`,
    #'   or the identity.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{ \mathrm{dim}, [n
    #'   \times n] \}]} storing the group exponential of the input tangent
    #'   vector(s).
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$exp(rep(0, 3))
    #' }
    exp = function(tangent_vec, base_point = NULL) {
      super$get_python_class()$exp(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Compute the group exponential of tangent vector from the
    #'   identity.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{ \mathrm{dim}, [n
    #'   \times n] \}]} storing the group exponential of the input tangent
    #'   vector(s).
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$exp_from_identity(rep(0, 3))
    #' }
    exp_from_identity = function(tangent_vec) {
      super$get_python_class()$exp_from_identity(tangent_vec = tangent_vec)
    },

    #' @description Calculate the group exponential at `base_point`.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{ \mathrm{dim}, [n
    #'   \times n] \}]} storing the group exponential of the input tangent
    #'   vector(s).
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$exp_not_from_identity(rep(0, 3), rep(0, 3))
    #' }
    exp_not_from_identity = function(tangent_vec, base_point) {
      super$get_python_class()$exp_not_from_identity(
        tangent_vec = tangent_vec,
        base_point = base_point
      )
    },

    #' @description Computes a left-invariant vector field bringing `base_point`
    #'   to `point`.
    #'
    #' @details The output is a vector of the tangent space at `base_point`, so
    #'   not a Lie algebra element if `base_point` is not the identity.
    #'   Furthermore, denoting `point` by \eqn{g} and `base_point` by \eqn{h},
    #'   the output satisfies \deqn{g = \exp(\log(g, h), h)}.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{ \mathrm{dim}, [n
    #'   \times n] \}]} storing the group logarithm of the input point(s).
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$log(rep(0, 3))
    #' }
    log = function(point, base_point = NULL) {
      super$get_python_class()$log(
        point = point,
        base_point = base_point
      )
    },

    #' @description Computes the group logarithm of `point` from the identity.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{ \mathrm{dim}, [n
    #'   \times n] \}]} storing the group logarithm of the input point(s).
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$log_from_identity(rep(0, 3))
    #' }
    log_from_identity = function(point) {
      super$get_python_class()$log_from_identity(point = point)
    },

    #' @description Computes the group logarithm at `base_point`.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{ \mathrm{dim}, [n
    #'   \times n] \}]} storing the group logarithm of the input point(s).
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$log_not_from_identity(rep(0, 3), rep(0, 3))
    #' }
    log_not_from_identity = function(point, base_point) {
      super$get_python_class()$log_not_from_identity(
        point = point,
        base_point = base_point
      )
    },

    #' @description Gets the identity of the group.
    #'
    #' @return A numeric array of shape \eqn{\{ \mathrm{dim}, [n \times n] \}}
    #'   storing the identity of the Lie group.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$get_identity()
    #' }
    get_identity = function() {
      super$get_python_class()$identity
    },

    #' @description Computes the lie bracket of two tangent vectors.
    #'
    #' @details For matrix Lie groups with tangent vectors \eqn{A} and \eqn{B}
    #'   at the same base point \eqn{P}, this is given by (translate to
    #'   identity, compute commutator, go back): \deqn{[A,B] = A_P^{-1}B -
    #'   B_P^{-1}A}.
    #'
    #' @param tangent_vector_a A numeric array of shape \eqn{[\dots \times n
    #'   \times n]} specifying one or more tangent vectors at corresponding base
    #'   points.
    #' @param tangent_vector_b A numeric array of shape \eqn{[\dots \times n
    #'   \times n]} specifying one or more tangent vectors at corresponding base
    #'   points.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times n \times n]} storing
    #'   the Lie bracket of the two input tangent vectors.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$lie_bracket(diag(0, 3), diag(0, 3))
    #' }
    lie_bracket = function(tangent_vector_a, tangent_vector_b, base_point = NULL) {
      super$get_python_class()$lie_bracket(
        tangent_vector_a = tangent_vector_a,
        tangent_vector_b = tangent_vector_b,
        base_point = base_point
      )
    },

    #' @description Computes the push-forward map by the left/right translation.
    #'
    #' @details Computes the push-forward map of the left/right translation by
    #'   the point. It corresponds to the tangent map, or differential of the
    #'   group multiplication by the point or its inverse. For groups with a
    #'   vector representation, it is only implemented at identity, but it can
    #'   be used at other points with `inverse = TRUE`. This method wraps the
    #'   Jacobian translation which actually computes the matrix representation
    #'   of the map.
    #'
    #' @param inverse A boolean specifying whether to inverse the Jacobian
    #'   matrix. If set to `TRUE`, the push forward by the translation by the
    #'   inverse of the point is returned. Defaults to `FALSE`.
    #'
    #' @return A function computing the tangent map of the left/right
    #'   translation by `point`. It can be applied to tangent vectors.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$tangent_translation_map(rep(0, 3))
    #' }
    tangent_translation_map = function(point, left_or_right = "left", inverse = FALSE) {
      tm <- super$get_python_class()$tangent_translation_map(
        point = point,
        left_or_right = left_or_right,
        inverse = inverse
      )
      function(tangent_vec) {
        tm |>
          reticulate::py_call(tangent_vec) |>
          reticulate::py_to_r()
      }
    },

    #' @description Performs function composition corresponding to the Lie
    #'   group.
    #'
    #' @param point_a A numeric array of shape \eqn{[\dots \times \{
    #'   \mathrm{dim}, [n \times n] \}]} specifying one or more left factors in
    #'   the product.
    #' @param point_b A numeric array of shape \eqn{[\dots \times \{
    #'   \mathrm{dim}, [n \times n] \}]} specifying one or more right factors in
    #'   the product.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{ \mathrm{dim}, [n
    #'   \times n] \}]} storing the product of `point_a` and `point_b` along the
    #'   first dimension.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$compose(rep(0, 3), rep(0, 3))
    #' }
    compose = function(point_a, point_b) {
      super$get_python_class()$compose(
        point_a = point_a,
        point_b = point_b
      )
    },

    #' @description Computes the Jacobian of left/right translation by a point.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \mathrm{dim} \times
    #'   \mathrm{dim}]} storing the Jacobian of the left/right translation by
    #'   `point`.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$jacobian_translation(rep(0, 3))
    #' }
    jacobian_translation = function(point, left_or_right = "left") {
      super$get_python_class()$jacobian_translation(
        point = point,
        left_or_right = left_or_right
      )
    },

    #' @description Computes the inverse law of the Lie group.
    #'
    #' @param point A numeric array of shape \eqn{[\dots \times \{ \mathrm{dim},
    #'   [n \times n] \}]} specifying one or more points to be inverted.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{ \mathrm{dim}, [n
    #'   \times n] \}]} storing the inverted points.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$inverse(rep(0, 3))
    #' }
    inverse = function(point) {
      super$get_python_class()$inverse(point = point)
    },

    #' @description Adds a metric to the class `$metrics` attribute.
    #'
    #' @param metric An object of class [`RiemannianMetric`].
    #'
    #' @return The class itself invisibly.
    add_metric = function(metric) {
      super$get_python_class()$add_metric(metric$get_python_class())
      private$set_fields()
      invisible(self)
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
      self$lie_algebra            <- super$get_python_class()$lie_algebra
      self$left_canonical_metric  <- super$get_python_class()$left_canonical_metric
      self$right_canonical_metric <- super$get_python_class()$right_canonical_metric
      self$metrics                <- super$get_python_class()$metrics
    }
  )
)
