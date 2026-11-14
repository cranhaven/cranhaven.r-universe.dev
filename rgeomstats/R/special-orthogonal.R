#' Abstract Class for Special Orthogonal Groups in Matrix Representation
#'
#' @description Class for special orthogonal groups in matrix representation.
#'
#' @author Nicolas Guigui and Nina Miolane
#'
#' @family special orthogonal classes
#' @keywords internal
SpecialOrthogonalMatrices <- R6::R6Class(
  classname = "SpecialOrthogonalMatrices",
  inherit = MatrixLieGroup,
  public = list(
    #' @field bi_invariant_metric An object of class `BiInvariantMetric`
    #'   specifying the metric to equip the manifold with.
    bi_invariant_metric = NULL,

    #' @description The [`SpecialOrthogonalMatrices`] class constructor.
    #'
    #' @param n An integer value specifying the number of rows and columns of
    #'   the matrices.
    #' @param ... Extra arguments to be passed to parent class constructors. See
    #'   [`MatrixLieAlgebra`], [`LevelSet`] and [`Manifold`] classes.
    #' @param py_cls A Python object of class `SpecialOrthogonalMatrices`.
    #'   Defaults to `NULL` in which case it is instantiated on the fly using
    #'   the other input arguments.
    #'
    #' @return An object of class [`SpecialOrthogonalMatrices`].
    initialize = function(n, ..., py_cls = NULL) {
      if (is.null(py_cls)) {
        dots <- capture_extra_params(...)
        dots$n <- as.integer(n)
        py_cls <- do.call(gs$geometry$special_orthogonal$`_SpecialOrthogonalMatrices`, dots)
      }
      super$set_python_class(py_cls)

      # Set up the second parent class here
      private$second_inheritance <- .SpecialOrthogonalMatrices$new(n, ..., py_cls = py_cls) # TO DO: not sure fields are inherited properly here

      private$set_fields()
    },

    #' @description Evaluates if a point belongs to the manifold.
    #'
    #' @param point A numeric array of shape \eqn{[\dots \times
    #'   \{\mathrm{dim}\}]} specifying one or more points to be checked.
    #' @param atol A numeric value specifying the absolute tolerance for
    #'   checking. Defaults to `gs$backend$atol`.
    #'
    #' @return A boolean value or vector storing whether the input points belong
    #'   to the manifold.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3)
    #'   so3$belongs(diag(1, 3))
    #' }
    belongs = function(point, atol = gs$backend$atol) {
      private$second_inheritance$belongs(point, atol = atol)
    },

    #' @description Converts from intrinsic to extrinsic coordinates.
    #'
    #' @param point_intrinsic A numeric array of shape \eqn{[\dots \times \{
    #'   \mathrm{dim} \}]} specifying one or more points on the embedded
    #'   manifold in intrinsic coordinates.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{
    #'   \mathrm{dim_embedding} \}]} storing the same points on the embedded
    #'   manifold in extrinsic coordinates.
    intrinsic_to_extrinsic_coords = function(point_intrinsic) {
      private$second_inheritance$intrinsic_to_extrinsic_coords(
        point_intrinsic = point_intrinsic
      )
    },

    #' @description Converts from extrinsic to intrinsic coordinates.
    #'
    #' @param point_extrinsic A numeric array of shape \eqn{[\dots \times \{
    #'   \mathrm{dim_embedding} \}]} specifying one or more points on the
    #'   embedded manifold in extrinsic coordinates.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \{ \mathrm{dim} \}]}
    #'   storing the same points on the embedded manifold in intrinsic
    #'   coordinates.
    extrinsic_to_intrinsic_coords = function(point_extrinsic) {
      private$second_inheritance$extrinsic_to_intrinsic_coords(
        point_extrinsic = point_extrinsic
      )
    },

    #' @description Project a matrix on \eqn{\mathrm{SO}(n)} by minimizing the
    #'   Frobenius norm.
    #'
    #' @param point A numeric array of shape \eqn{[\dots \times n \times n]}
    #'   specifying one or more matrices.
    #'
    #' @return A numeric array of the same shape storing the projected matrices.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(3)
    #'   so3$projection(diag(1, 3))
    #' }
    projection = function(point) {
      private$second_inheritance$get_python_class()$projection(point = point)
    }
  ),
  private = list(
    second_inheritance = NULL,
    set_fields = function() {
      super$set_fields()
      self$n                   <- super$get_python_class()$n
      self$bi_invariant_metric <- super$get_python_class()$bi_invariant_metric
    }
  )
)

.SpecialOrthogonalMatrices <- R6::R6Class(
  classname = ".SpecialOrthogonalMatrices",
  inherit = LevelSet,
  public = list(
    initialize = function(n, ..., py_cls = NULL) {
      if (is.null(py_cls)) {
        dots <- capture_extra_params(...)
        dots$n <- as.integer(n)
        py_cls <- do.call(gs$geometry$special_orthogonal$`_SpecialOrthogonalMatrices`, dots)
      }
      super$set_python_class(py_cls)
      private$set_fields()
    }
  ), private = list(
    set_fields = function() {
      super$set_fields()
    }
  )
)

#' Abstract Class for Special Orthogonal Groups in Vector Representation
#'
#' @description Class for the special orthogonal groups
#'   \eqn{\mathrm{SO}(\{2,3\})} in vector form, i.e. the Lie groups of planar
#'   and 3D rotations. This class is specific to the vector representation of
#'   rotations. For the matrix representation, use the [`SpecialOrthogonal`]
#'   class and set `n = 2` or `n = 3`.
#'
#' @author Nicolas Guigui and Nina Miolane
#'
#' @keywords internal
SpecialOrthogonalVectors <- R6::R6Class(
  classname = "SpecialOrthogonalVectors",
  inherit = LieGroup,
  public = list(
    #' @field n An integer value specifying the number of rows and columns of
    #'   the matrices.
    n = NULL,

    #' @field epsilon A numeric value specifying the precision to use for
    #'   calculations involving potential divison by 0 in rotations.
    epsilon = NULL,

    #' @description The [`SpecialOrthogonalVectors`] class constructor.
    #'
    #' @param n An integer value specifying the number of rows and columns of
    #'   the matrices.
    #' @param epsilon A numeric value specifying the precision to use for
    #'   calculations involving potential divison by 0 in rotations. Defaults to
    #'   `0`.
    #' @param py_cls A Python object of class `SpecialOrthogonalVectors`.
    #'   Defaults to `NULL` in which case it is instantiated on the fly using
    #'   the other input arguments.
    #'
    #' @return An object of class [`SpecialOrthogonalVectors`].
    initialize = function(n, epsilon = 0.0, py_cls = NULL) {
      if (is.null(py_cls)) {
        py_cls <- gs$geometry$special_orthogonal$`_SpecialOrthogonalVectors`(
          n = n,
          epsilon = epsilon
        )
      }
      super$set_python_class(py_cls)
      private$set_fields()
    },

    #' @description Projects a matrix on \eqn{\mathrm{SO}(2)} or
    #'   \eqn{\mathrm{SO}(3)} using the Frobenius norm.
    #'
    #' @param point A numeric array of shape \eqn{[\dots \times n \times n]}
    #'   specifying one or more matrices to be projected.
    #'
    #' @return A numeric array of the same shape as the input point storing the
    #'   projected matrices.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
    #'   so2$projection(diag(1, 2))
    #' }
    projection = function(point) {
      super$get_python_class()$projection(point = point)
    },

    #' @description Gets the skew-symmetric matrix derived from the vector. In
    #'   3D, computes the skew-symmetric matrix, known as the cross-product of a
    #'   vector, associated to the vector `vec`.
    #'
    #' @param vec A numeric array of shape \eqn{[\dots \times \mathrm{dim}]}
    #'   specifying one or more vectors from which to compute corresponding skew
    #'   matrix representations.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times n \times n]} storing
    #'   the corresponding skew matrix representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
    #'   so2$skew_matrix_from_vector(array(0))
    #' }
    skew_matrix_from_vector = function(vec) {
      super$get_python_class()$skew_matrix_from_vector(vec = vec)
    },

    #' @description Derives a vector from the skew-symmetric matrix. In 3D,
    #'   computes the vector defining the cross-product associated to a
    #'   skew-symmetric matrix.
    #'
    #' @param skew_mat A numeric array of shape \eqn{[\dots \times n \times n]}
    #'   specifying skew matrices.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times \mathrm{dim}]}
    #'   storing the corresponding vector representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
    #'   so2$vector_from_skew_matrix(diag(0, 2))
    #' }
    vector_from_skew_matrix = function(skew_mat) {
      super$get_python_class()$vector_from_skew_matrix(skew_mat = skew_mat)
    },

    #' @description Regularizes a tangent vector at the identity. In 2D,
    #'   regularizes a tangent vector by getting its norm at the identity to be
    #'   less than \eqn{\pi}.
    #'
    #' @param tangent_vec A numeric array of shape \eqn{[\dots \times 1]}
    #'   specifying one or more tangent vectors at base point.
    #' @param metric An object of class [`RiemannianMetric`] specifying the
    #'   metric to compute the norm of the tangent vector or `NULL`. If it is
    #'   set to `NULL`, it defaults to using the Euclidean metric.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 1]} storing the
    #'   regularized tangent vector(s).
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
    #'   so2$regularize_tangent_vec_at_identity(array(0))
    #' }
    regularize_tangent_vec_at_identity = function(tangent_vec, metric = NULL) {
      if (!is.null(metric)) metric <- metric$get_python_class()
      super$get_python_class()$regularize_tangent_vec_at_identity(
        tangent_vec = tangent_vec,
        metric = metric
      )
    },

    #' @description Regularizes a tangent vector at a base point. In 2D,
    #'   regularizes a tangent vector by getting the norm of its parallel
    #'   transport to the identity, determined by the metric, to be less than
    #'   \eqn{\pi}.
    #'
    #' @param tangent_vec A numeric array of shape \eqn{[\dots \times 1]}
    #'   specifying one or more tangent vectors at corresponding base points.
    #' @param base_point A numeric array of shape \eqn{[\dots \times 1]}
    #'   specifying one or more points on the manifold.
    #' @param metric An object of class [`RiemannianMetric`] specifying the
    #'   metric to compute the norm of the tangent vector or `NULL`. If it is
    #'   set to `NULL`, it defaults to using the Euclidean metric.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 1]} storing the
    #'   regularized tangent vector(s).
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
    #'   so2$regularize_tangent_vec(array(0), array(1))
    #' }
    regularize_tangent_vec = function(tangent_vec, base_point, metric = NULL) {
      if (!is.null(metric)) metric <- metric$get_python_class()
      super$get_python_class()$regularize_tangent_vec(
        tangent_vec = tangent_vec,
        base_point = base_point,
        metric = metric
      )
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
      self$n       <- super$get_python_class()$n
      self$epsilon <- super$get_python_class()$epsilon
    }
  )
)

#' Abstract Class for the 2D Special Orthogonal Group in Vector Representation
#'
#' @description Class for the special orthogonal group \eqn{\mathrm{SO}(2)} in
#'   vector form, i.e. the Lie group of planar rotations. This class is
#'   specific to the vector representation of rotations. For the matrix
#'   representation, use the [`SpecialOrthogonal`] class and set `n = 2`.
#'
#' @author Nicolas Guigui and Nina Miolane
#'
#' @family special orthogonal classes
#' @keywords internal
SpecialOrthogonal2Vectors <- R6::R6Class(
  classname = "SpecialOrthogonal2Vectors",
  inherit = SpecialOrthogonalVectors,
  public = list(
    #' @description The [`SpecialOrthogonal2Vectors`] class constructor.
    #'
    #' @param epsilon A numeric value specifying the precision to use for
    #'   calculations involving potential division by 0 in rotations. Defaults to
    #'   `0`.
    #' @param py_cls A Python object of class `SpecialOrthogonal2Vectors`.
    #'   Defaults to `NULL` in which case it is instantiated on the fly using
    #'   the other input arguments.
    #'
    #' @return An object of class [`SpecialOrthogonal2Vectors`].
    initialize = function(epsilon = 0.0, py_cls = NULL) {
      if (is.null(py_cls)) {
        py_cls <- gs$geometry$special_orthogonal$`_SpecialOrthogonal2Vectors`(
          epsilon = epsilon
        )
      }
      super$set_python_class(py_cls)
      private$set_fields()
    },

    #' @description Converts rotation matrix (in 2D) to rotation vector
    #'   (axis-angle) getting the angle through the `atan2()` function.
    #'
    #' @param rot_mat A numeric array of shape \eqn{[\dots \times 2 \times 2]}
    #'   specifying one or more 2D rotation matrices.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 1]} storing the
    #'   corresponding axis-angle representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
    #'   so2$rotation_vector_from_matrix(diag(1, 2))
    #' }
    rotation_vector_from_matrix = function(rot_mat) {
      super$get_python_class()$rotation_vector_from_matrix(rot_mat = rot_mat)
    },

    #' @description Convert a 2D rotation from vector to matrix representation.
    #'
    #' @param rot_vec A numeric array of shape \eqn{... \times 1} specifying one
    #'   or more 2D rotations in vector representation.
    #'
    #' @return A numeric array of shape \eqn{... \times 2 \times 2} storing the
    #'   corresponding 2D rotation matrices.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
    #'   so2$matrix_from_rotation_vector(array(0))
    #' }
    matrix_from_rotation_vector = function(rot_vec) {
      super$get_python_class()$matrix_from_rotation_vector(rot_vec = rot_vec)
    },

    #' @description Samples in \eqn{\mathrm{SO}(2)} from a uniform distribution.
    #'
    #' @param n_samples An integer value specifying the sample size. Defaults to
    #'   `1L`.
    #'
    #' @return A numeric array of shape \eqn{... \times 1} storing a sample of
    #'   2D rotations in axis-angle representation uniformly sampled in
    #'   \eqn{\mathrm{SO}(2)}.
    random_uniform = function(n_samples = 1) {
      super$get_python_class()$random_uniform(n_samples = as.integer(n_samples))
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
    }
  )
)

#' Abstract Class for the 3D Special Orthogonal Group in Vector Representation
#'
#' @description Class for the special orthogonal group \eqn{\mathrm{SO}(3)} in
#'   vector form, i.e. the Lie group of 3D rotations. This class is specific to
#'   the vector representation of rotations. For the matrix representation, use
#'   the [`SpecialOrthogonal`] class and set `n = 3`.
#'
#' @author Nicolas Guigui and Nina Miolane
#'
#' @family special orthogonal classes
#' @keywords internal
SpecialOrthogonal3Vectors <- R6::R6Class(
  classname = "SpecialOrthogonal3Vectors",
  inherit = SpecialOrthogonalVectors,
  public = list(
    #' @field bi_invariant_metric An object of class `BiInvariantMetric`
    #'   specifying the metric to equip the manifold with.
    bi_invariant_metric = NULL,

    #' @description The [`SpecialOrthogonal3Vectors`] class constructor.
    #'
    #' @param epsilon A numeric value specifying the precision to use for
    #'   calculations involving potential division by 0 in rotations. Defaults to
    #'   `0`.
    #' @param py_cls A Python object of class `SpecialOrthogonal3Vectors`.
    #'   Defaults to `NULL` in which case it is instantiated on the fly using
    #'   the other input arguments.
    #'
    #' @return An object of class [`SpecialOrthogonal3Vectors`].
    initialize = function(epsilon = 0.0, py_cls = NULL) {
      if (is.null(py_cls)) {
        py_cls <- gs$geometry$special_orthogonal$`_SpecialOrthogonal3Vectors`(
          epsilon = epsilon
        )
      }
      super$set_python_class(py_cls)
      private$set_fields()
    },

    #' @description Converts a 3D rotation from matrix to axis-angle
    #'   representation.
    #'
    #' @details Gets the angle \eqn{\theta} through the trace of the rotation
    #'   matrix. The eigenvalues are: \deqn{\{ 1, \cos \theta + i \sin \theta,
    #'   \cos \theta - i \sin \theta \}} so that \deqn{\mathrm{trace} = 1 + 2
    #'   \cos \theta, \{ -1 \leq \mathrm{trace} \leq 3 \}.}
    #'
    #' The rotation vector is the vector associated to the skew-symmetric matrix
    #' \deqn{S_r = \frac{\theta}{(2 \sin \theta) (R - R^T)}.}
    #'
    #' For the edge case where the angle is close to \eqn{\pi}, the rotation
    #' vector (up to sign) is derived by using the following equality (see the
    #' axis-angle representation on Wikipedia): \deqn{\mathrm{outer}(r, r) =
    #' \frac{1}{2} (R + I_3).}
    #'
    #' In nD, the rotation vector stores the \eqn{n(n-1)/2} values of the
    #' skew-symmetric matrix representing the rotation.
    #'
    #' @param rot_mat A numeric array of shape \eqn{[\dots \times 3 \times 3]}
    #'   specifying one or more 3D rotation matrices.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 3]} storing the
    #'   corresponding axis-angle representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$rotation_vector_from_matrix(diag(1, 3))
    #' }
    rotation_vector_from_matrix = function(rot_mat) {
      super$get_python_class()$rotation_vector_from_matrix(rot_mat = rot_mat)
    },

    #' @description Converts a 3D rotation from axis-angle to matrix
    #'   representation.
    #'
    #' @param rot_vec A numeric array of shape \eqn{[\dots \times 3]} specifying
    #'   one or more 3D rotations in axis-angle representation.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 3 \times 3]} storing
    #'   the corresponding matrix representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$matrix_from_rotation_vector(rep(0, 3))
    #' }
    matrix_from_rotation_vector = function(rot_vec) {
      super$get_python_class()$matrix_from_rotation_vector(rot_vec = rot_vec)
    },

    #' @description Converts a 3D rotation from matrix to unit quaternion
    #'   representation.
    #'
    #' @param rot_mat A numeric array of shape \eqn{[\dots \times 3 \times 3]}
    #'   specifying one or more 3D rotations in matrix representation.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 4]} storing the
    #'   corresponding unit quaternion representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$quaternion_from_matrix(diag(1, 3))
    #' }
    quaternion_from_matrix = function(rot_mat) {
      super$get_python_class()$quaternion_from_matrix(rot_mat = rot_mat)
    },

    #' @description Converts a 3D rotation from axis-angle to unit quaternion
    #'   representation.
    #'
    #' @param rot_vec A numeric array of shape \eqn{[\dots \times 3]} specifying
    #'   one or more 3D rotations in axis-angle representation.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 4]} storing the
    #'   corresponding unit quaternion representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$quaternion_from_rotation_vector(rep(0, 3))
    #' }
    quaternion_from_rotation_vector = function(rot_vec) {
      super$get_python_class()$quaternion_from_rotation_vector(rot_vec = rot_vec)
    },

    #' @description Converts a 3D rotation from unit quaternion to axis-angle
    #'   representation.
    #'
    #' @param quaternion A numeric array of shape \eqn{[\dots \times 4]}
    #'   specifying one or more 3D rotations in unit quaternion representation.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 3]} storing the
    #'   corresponding axis-angle representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$rotation_vector_from_quaternion(array(c(1, rep(0, 3))))
    #' }
    rotation_vector_from_quaternion = function(quaternion) {
      super$get_python_class()$rotation_vector_from_quaternion(quaternion = quaternion)
    },

    #' @description Converts a 3D rotation from unit quaternion to matrix
    #'   representation.
    #'
    #' @param quaternion A numeric array of shape \eqn{[\dots \times 4]}
    #'   specifying one or more 3D rotations in unit quaternion representation.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 3 \times 3]} storing
    #'   the corresponding matrix representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$matrix_from_quaternion(c(1, rep(0, 3)))
    #' }
    matrix_from_quaternion = function(quaternion) {
      super$get_python_class()$matrix_from_quaternion(quaternion = quaternion)
    },

    #' @description Converts a 3D rotation from Tait-Bryan angle to matrix
    #'   representation.
    #'
    #' @details Converts a rotation given in terms of the Tait-Bryan angles
    #'   `[angle_1, angle_2, angle_3]` in extrinsic (fixed) or intrinsic
    #'   (moving) coordinate frame in the corresponding matrix representation.
    #'   If the order is `zyx`, into the rotation matrix `rot_mat = X(angle_1)
    #'   Y(angle_2) Z(angle_3)` where:
    #' - `X(angle_1)` is a rotation of angle `angle_1` around axis `x`;
    #' - `Y(angle_2)` is a rotation of angle `angle_2` around axis `y`;
    #' - `Z(angle_3)` is a rotation of angle `angle_3` around axis `z`.
    #'
    #' Exchanging `'extrinsic'` and `'intrinsic'` amounts to exchanging the
    #' order.
    #'
    #' @param tait_bryan_angles A numeric array of shape \eqn{[\dots \times 3]}
    #'   specifying one or more 3D rotations in Tait-Bryan angle representation.
    #' @param extrinsic_or_intrinsic A character string specifying the
    #'   coordinate frame in which the Tait-Bryan angles are expressed. Choices
    #'   are either `"extrinsic"` (fixed frame) or `"intrinsic"` (moving frame).
    #'   Defaults to `"extrinsic"`.
    #' @param order A character string specifying the order of the rotation
    #'   composition around the three axes of the chosen coordinate frame.
    #'   Choices are either `"xyz"` or `"zyx"`. Defaults to `"zyx"`.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 3 \times 3]} storing
    #'   the corresponding matrix representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$matrix_from_tait_bryan_angles(rep(0, 3))
    #' }
    matrix_from_tait_bryan_angles = function(tait_bryan_angles,
                                             extrinsic_or_intrinsic = "extrinsic",
                                             order = "zyx") {
      super$get_python_class()$matrix_from_tait_bryan_angles(
        tait_bryan_angles = tait_bryan_angles,
        extrinsic_or_intrinsic = extrinsic_or_intrinsic,
        order = order
      )
    },

    #' @description Converts a 3D rotation from matrix to Tait-Bryan angle
    #'   representation.
    #'
    #' @details Converts a rotation given in matrix representation into its
    #'   Tait-Bryan angle representation `[angle_1, angle_2, angle_3]` in
    #'   extrinsic (fixed) or intrinsic (moving) coordinate frame in the
    #'   corresponding matrix representation. If the order is `zyx`, into the
    #'   rotation matrix `rot_mat = X(angle_1) Y(angle_2) Z(angle_3)` where:
    #' - `X(angle_1)` is a rotation of angle `angle_1` around axis `x`;
    #' - `Y(angle_2)` is a rotation of angle `angle_2` around axis `y`;
    #' - `Z(angle_3)` is a rotation of angle `angle_3` around axis `z`.
    #'
    #' Exchanging `'extrinsic'` and `'intrinsic'` amounts to exchanging the
    #' order.
    #'
    #' @param rot_mat A numeric array of shape \eqn{[\dots \times 3 \times 3]}
    #'   specifying one or more 3D rotations in matrix representation.
    #' @param extrinsic_or_intrinsic A character string specifying the
    #'   coordinate frame in which the Tait-Bryan angles are expressed. Choices
    #'   are either `"extrinsic"` (fixed frame) or `"intrinsic"` (moving frame).
    #'   Defaults to `"extrinsic"`.
    #' @param order A character string specifying the order of the rotation
    #'   composition around the three axes of the chosen coordinate frame.
    #'   Choices are either `"xyz"` or `"zyx"`. Defaults to `"zyx"`.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 3]} storing the
    #'   corresponding Tait-Bryan angle representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$tait_bryan_angles_from_matrix(diag(1, 3))
    #' }
    tait_bryan_angles_from_matrix = function(rot_mat,
                                             extrinsic_or_intrinsic = "extrinsic",
                                             order = "zyx") {
      super$get_python_class()$tait_bryan_angles_from_matrix(
        rot_mat = rot_mat,
        extrinsic_or_intrinsic = extrinsic_or_intrinsic,
        order = order
      )
    },

    #' @description Converts a 3D rotation from Tait-Bryan angle to unit
    #'   quaternion representation.
    #'
    #' @param tait_bryan_angles A numeric array of shape \eqn{[\dots \times 3]}
    #'   specifying one or more 3D rotations in Tait-Bryan angle representation.
    #' @param extrinsic_or_intrinsic A character string specifying the
    #'   coordinate frame in which the Tait-Bryan angles are expressed. Choices
    #'   are either `"extrinsic"` (fixed frame) or `"intrinsic"` (moving frame).
    #'   Defaults to `"extrinsic"`.
    #' @param order A character string specifying the order of the rotation
    #'   composition around the three axes of the chosen coordinate frame.
    #'   Choices are either `"xyz"` or `"zyx"`. Defaults to `"zyx"`.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 4]} storing the
    #'   corresponding unit quaternion representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$quaternion_from_tait_bryan_angles(rep(0, 3))
    #' }
    quaternion_from_tait_bryan_angles = function(tait_bryan_angles,
                                                 extrinsic_or_intrinsic = "extrinsic",
                                                 order = "zyx") {
      super$get_python_class()$quaternion_from_tait_bryan_angles(
        tait_bryan_angles = tait_bryan_angles,
        extrinsic_or_intrinsic = extrinsic_or_intrinsic,
        order = order
      )
    },

    #' @description Converts a 3D rotation from Tait-Bryan angle to axis-angle
    #'   representation.
    #'
    #' @param tait_bryan_angles A numeric array of shape \eqn{[\dots \times 3]}
    #'   specifying one or more 3D rotations in Tait-Bryan angle representation.
    #' @param extrinsic_or_intrinsic A character string specifying the
    #'   coordinate frame in which the Tait-Bryan angles are expressed. Choices
    #'   are either `"extrinsic"` (fixed frame) or `"intrinsic"` (moving frame).
    #'   Defaults to `"extrinsic"`.
    #' @param order A character string specifying the order of the rotation
    #'   composition around the three axes of the chosen coordinate frame.
    #'   Choices are either `"xyz"` or `"zyx"`. Defaults to `"zyx"`.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 3]} storing the
    #'   corresponding axis-angle representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$rotation_vector_from_tait_bryan_angles(rep(0, 3))
    #' }
    rotation_vector_from_tait_bryan_angles = function(tait_bryan_angles,
                                                      extrinsic_or_intrinsic = "extrinsic",
                                                      order = "zyx") {
      super$get_python_class()$rotation_vector_from_tait_bryan_angles(
        tait_bryan_angles = tait_bryan_angles,
        extrinsic_or_intrinsic = extrinsic_or_intrinsic,
        order = order
      )
    },

    #' @description Converts a 3D rotation from matrix to Tait-Bryan angle
    #'   representation.
    #'
    #' @param quaternion A numeric array of shape \eqn{[\dots \times 4]}
    #'   specifying one or more 3D rotations in unit quaternion representation.
    #' @param extrinsic_or_intrinsic A character string specifying the
    #'   coordinate frame in which the Tait-Bryan angles are expressed. Choices
    #'   are either `"extrinsic"` (fixed frame) or `"intrinsic"` (moving frame).
    #'   Defaults to `"extrinsic"`.
    #' @param order A character string specifying the order of the rotation
    #'   composition around the three axes of the chosen coordinate frame.
    #'   Choices are either `"xyz"` or `"zyx"`. Defaults to `"zyx"`.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 3]} storing the
    #'   corresponding Tait-Bryan angle representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$tait_bryan_angles_from_quaternion(c(1, rep(0, 3)))
    #' }
    tait_bryan_angles_from_quaternion = function(quaternion,
                                                 extrinsic_or_intrinsic = "extrinsic",
                                                 order = "zyx") {
      super$get_python_class()$tait_bryan_angles_from_quaternion(
        quaternion = quaternion,
        extrinsic_or_intrinsic = extrinsic_or_intrinsic,
        order = order
      )
    },

    #' @description Converts a 3D rotation from axis-angle to Tait-Bryan angle
    #'   representation.
    #'
    #' @param rot_vec A numeric array of shape \eqn{[\dots \times 3]} specifying
    #'   one or more 3D rotations in axis-angle representation.
    #' @param extrinsic_or_intrinsic A character string specifying the
    #'   coordinate frame in which the Tait-Bryan angles are expressed. Choices
    #'   are either `"extrinsic"` (fixed frame) or `"intrinsic"` (moving frame).
    #'   Defaults to `"extrinsic"`.
    #' @param order A character string specifying the order of the rotation
    #'   composition around the three axes of the chosen coordinate frame.
    #'   Choices are either `"xyz"` or `"zyx"`. Defaults to `"zyx"`.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 3]} storing the
    #'   corresponding Tait-Bryan angle representations.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$tait_bryan_angles_from_rotation_vector(rep(0, 3))
    #' }
    tait_bryan_angles_from_rotation_vector = function(rot_vec,
                                                      extrinsic_or_intrinsic = "extrinsic",
                                                      order = "zyx") {
      super$get_python_class()$tait_bryan_angles_from_rotation_vector(
        rot_vec = rot_vec,
        extrinsic_or_intrinsic = extrinsic_or_intrinsic,
        order = order
      )
    },

    #' @description Samples in \eqn{\mathrm{SO}(3)} from a uniform distribution.
    #'
    #' @param n_samples An integer value specifying the sample size. Defaults to
    #'   `1L`.
    #'
    #' @return A numeric array of shape \eqn{[\dots \times 3]} storing a sample
    #'   of 3D rotations in axis-angle representation uniformly sampled in
    #'   \eqn{\mathrm{SO}(3)}.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
    #'   so3$random_uniform()
    #' }
    random_uniform = function(n_samples = 1) {
      super$get_python_class()$random_uniform(n_samples = as.integer(n_samples))
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
      self$bi_invariant_metric <- super$get_python_class()$bi_invariant_metric
    }
  )
)

#' Class for the Special Orthogonal Group
#'
#' This function generates an instance of the class for the special orthogonal
#' group \eqn{\mathrm{SO}(n)}.
#'
#' @author Nicolas Guigui and Nina Miolane
#'
#' @param n An integer value representing the shape of the `n x n` matrices.
#' @param point_type A character string specifying how elements of the group
#'   should be represented. Choices are either `"vector"` or `"matrix"`.
#'   Defaults to `"matrix"`.
#' @param epsilon A numeric value specifying the precision to use for
#'   calculations involving potential division by 0 in rotations. Defaults to
#'   `0.0`.
#' @param ... Extra arguments to be passed to parent class constructors. See
#'   [`LieGroup`], [`MatrixLieAlgebra`], [`LevelSet`] and [`Manifold`] classes.
#' @param py_cls A Python object of class `SpecialOrthogonal`. Defaults to
#'   `NULL` in which case it is instantiated on the fly using the other input
#'   arguments.
#'
#' @return An object of class [`SpecialOrthogonal`] which is an instance of one
#'   of three different [`R6::R6Class`] depending on the values of the input
#'   arguments. Specifically:
#' - if `n == 2` and `point_type == "vector"`, then the user wants to
#' instantiate the space of 2D rotations in vector representations and thus the
#' output is an instance of the [`SpecialOrthogonal2Vectors`] class;
#' - if `n == 3` and `point_type == "vector"`, then the user wants to
#' instantiate the space of 3D rotations in vector representations and thus the
#' output is an instance of the [`SpecialOrthogonal3Vectors`] class;
#' - in all other cases, either the user is dealing with rotations in matrix
#' representation or with rotations in dimension greater than 3 and thus the
#' output is an instance of the [`SpecialOrthogonalMatrices`] class.
#'
#' @family special orthogonal classes
#' @export
#' @examples
#' if (reticulate::py_module_available("geomstats")) {
#'   so3 <- SpecialOrthogonal(n = 3)
#'   so3
#' }
SpecialOrthogonal <- function(n, point_type = "matrix", epsilon = 0.0, ..., py_cls = NULL) {
  if (n == 2 && point_type == "vector") {
    cls <- SpecialOrthogonal2Vectors$new(epsilon, py_cls = py_cls)
    class(cls) <- c("SpecialOrthogonal", class(cls))
    return(cls)
  }
  if (n == 3 && point_type == "vector") {
    cls <- SpecialOrthogonal3Vectors$new(epsilon, py_cls = py_cls)
    class(cls) <- c("SpecialOrthogonal", class(cls))
    return(cls)
  }
  if (point_type == "vector")
    cli::cli_abort("SO(n) is only implemented in vector representation for n = 2 or 3.")
  cls <- SpecialOrthogonalMatrices$new(n, ..., py_cls = py_cls)
  class(cls) <- c("SpecialOrthogonal", class(cls))
  cls
}
