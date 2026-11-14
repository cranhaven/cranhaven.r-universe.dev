#' Abstract Class for Connections
#'
#' @description An [R6::R6Class] object implementing the base [`Connection`]
#'   class for affine connections.
#'
#' @param tangent_vec A numeric array of shape `dim` specifying a tangent vector
#'   at `base_point`.
#' @param tangent_vec_a Tangent vector at `base_point`.
#' @param tangent_vec_b Tangent vector at `base_point`.
#' @param tangent_vec_c Tangent vector at `base_point`.
#' @param base_point A numeric array of shape `dim` specifying a base point on
#'   the manifold.
#' @param n_steps An integer value specifying the number of discrete time steps
#'   to take in the integration. Defaults to `100L`.
#' @param step A string specifying which numerical scheme to use for
#'   integration. Choices are `euler` or `rk4`. Defaults to `euler`.
#'
#' @author Nicolas Guigui
#'
#' @keywords internal
Connection <- R6::R6Class(
  classname = "Connection",
  inherit = PythonClass,
  public = list(
    #' @field dim An integer value specifying the dimension of the underlying
    #'   manifold.
    dim = NULL,

    #' @field shape An integer vector specifying the shape of one element of the
    #'   manifold. Defaults to `NULL`.
    shape = NULL,

    #' @field default_coords_type A string specifying the coordinate type.
    #'   Choices are `extrensic` or `intrinsic`. Dedaults to `intrinsic`.
    default_coords_type = NULL,

    #' @field default_point_type A string specifying the point type. Choices are
    #'   `vector` or `matrix`. It is automatically determined depending on the
    #'   manifold.
    default_point_type = NULL,

    #' @description The [`Connection`] class constructor.
    #'
    #' @param dim An integer value specifying the dimension of the manifold.
    #' @param shape An integer vector specifying the shape of one element of the
    #'   manifold. Defaults to `NULL`.
    #' @param metric A [`RiemannianMetric`] object specifying the metric to use
    #'   on the manifold. Defaults to `NULL`.
    #' @param default_coords_type A string specifying the coordinate type.
    #'   Choices are `extrensic` or `intrinsic`. Defaults to `intrinsic`.
    #' @param py_cls A Python object of class `Connection`. Defaults to `NULL`
    #'   in which case it is instantiated on the fly using the other input
    #'   arguments.
    #'
    #' @return An object of class [`Connection`].
    initialize = function(dim, shape = NULL, default_coords_type = "intrinsic", py_cls = NULL) {
      if (is.null(py_cls)) {
        dim <- as.integer(dim)
        if (!is.null(shape)) shape <- dim
        py_cls <- gs$geometry$connection$Connection(
          dim = dim,
          shape = shape,
          default_coords_type = default_coords_type
        )
      }
      super$set_python_class(py_cls)
      private$set_fields()
    },

    #' @description Christoffel symbols associated with the connection.
    #'
    #' @return A numeric array of shape `c(dim, dim, dim)` storing the
    #'   Christoffel symbols, with the contravariant index on the first
    #'   dimension.
    christoffels = function(base_point) {
      super$get_python_class()$christoffels(base_point)
    },

    #' @description Computes the geodesic ODE associated with the connection.
    #'
    #' @param state A numeric array of shape `dim` specifying a tangent vector
    #'   to the manifold at the position specified by `.time`.
    #' @param .time A numeric array of shape `dim` specifying a point on the
    #'   manifold at which to compute the geodesic ODE.
    #'
    #' @return A numeric array of shape `dim` storing the value of the vector
    #'   field to be integrated at position.
    geodesic_equation = function(state, .time) {
      super$get_python_class()$geodesic_equation(state, .time)
    },

    #' @description Exponential map associated to the affine connection.
    #'
    #' @details Exponential map at base_point of tangent_vec computed by
    #'   integration of the geodesic equation (initial value problem), using the
    #'   christoffel symbols.
    #'
    #' @return A numeric array of shape `dim` storing the exponential of the
    #'   input tangent vector, which lies on on the manifold.
    exp = function(tangent_vec, base_point, n_steps = 100, step = "euler") {
      n_steps <- as.integer(n_steps)
      step <- match.arg(step, c("euler", "rk4"))
      super$get_python_class()$exp(
        tangent_vec = tangent_vec,
        base_point = base_point,
        n_steps = n_steps,
        step = step
      )
    },

    #' @description Logarithm map associated to the affine connection.
    #'
    #' @details Solves the boundary value problem associated to the geodesic
    #'   equation using the Christoffel symbols and conjugate gradient descent.
    #'
    #' @param point A numeric array of shape `dim` specifying a point on the
    #'   manifold.
    #' @param max_iter An integer value specifying the number of iterations.
    #'   Defaults to `25L`.
    #' @param verbose A boolean specifying whether the optimizer should display
    #'   intermediate messages pertaining to its convergence. Defaults to
    #'   `FALSE`.
    #' @param tol A numeric value specifying the absolute tolerance for
    #'   optimization convergence. Defaults to `gs$backend$atol`.
    #'
    #' @return A numeric array of shape `dim` storing the exponential of the
    #'   input tangent vector, which lies on on the manifold.
    log = function(point, base_point, n_steps = 100, step = "euler",
                   max_iter = 25, verbose = FALSE, tol = gs$backend$atol) {
      n_steps <- as.integer(n_steps)
      step <- match.arg(step, c("euler", "rk4"))
      max_iter <- as.integer(max_iter)
      super$get_python_class()$log(
        point = point,
        base_point = base_point,
        n_steps = n_steps,
        step = step,
        max_iter = max_iter,
        verbose = verbose,
        tol = tol
      )
    },

    #' @description Approximate parallel transport using the pole ladder scheme.
    #'
    #' @details Approximate parallel transport using either the pole ladder or
    #'   the Schild's ladder scheme
    #'   \insertCite{lorenzi2014efficient}{rgeomstats}. Pole ladder is exact in
    #'   symmetric spaces and of order two in general while Schild's ladder is a
    #'   first order approximation \insertCite{guigui2022numerical}{rgeomstats}.
    #'   Both schemes are available on any affine connection manifolds whose
    #'   exponential and logarithm maps are implemented. `tangent_vec` is
    #'   transported along the geodesic starting at the `base_point` with
    #'   initial tangent vector `direction`.
    #'
    #' ## References
    #'
    #' \insertCited{}
    #'
    #' @param direction Tangent vector at base point specifying the initial
    #'   speed of the geodesic along which to transport.
    #' @param n_rungs A scalar integer specifying the Number of steps of the
    #'   ladder. Defaults to `1L`.
    #' @param scheme A string specifying the scheme to use for the construction
    #'   of the ladder at each step. Choices are either `pole` or `schild`.
    #'   Defaults to `pole`.
    #' @param alpha A numeric value specifying the exponent for the scaling of
    #'   the vector to transport. Must be greater or equal to 1 and
    #'   \insertCite{guigui2022numerical;textual}{rgeomstats} proved that `alpha
    #'   = 2` is optimal. Defaults to `2`.
    #' @param ... Extra arguments to be passed to calls to `$exp()` and `$log()`
    #'   in auxiliary single ladder step functions.
    #'
    #' @return A named list with 3 components:
    #' - `transported_tangent_vector`: Approximation of the parallel transport
    #' of the input tangent vector.
    #' - `trajectory` : A list of length `n_steps` storing the geodesics of the
    #' construction, only if `return_geodesics = TRUE` in the step function. The
    #' geodesics are methods of the class connection.
    #' - `end_point`:
    ladder_parallel_transport = function(tangent_vec, base_point, direction,
                                         n_rungs = 1, scheme = "pole",
                                         alpha = 1, ...) {
      dots <- capture_extra_params(...)
      dots$tangent_vec <- tangent_vec
      dots$base_point <- base_point
      dots$direction <- direction
      dots$n_rungs <- as.integer(n_rungs)
      dots$scheme <- match.arg(scheme, c("pole", "schild"))
      dots$alpha <- alpha
      do.call(super$get_python_class()$ladder_parallel_transport, dots)
    },

    #' @description Computes the curvature.
    #'
    #' @details For three vector fields \eqn{X|_P = \mathrm{tangent\_vec\_a}},
    #'   \eqn{Y|_P = \mathrm{tangent\_vec\_b}}, \eqn{Z|_P =
    #'   \mathrm{tangent\_vec\_c}} with tangent vector specified in argument at
    #'   the base point \eqn{P}, the curvature is defined by \deqn{R(X,Y)Z =
    #'   \nabla_{[X,Y]}Z - \nabla_X\nabla_Y Z + \nabla_Y\nabla_X Z.}
    #'
    #' @return Tangent vector at `base_point`.
    curvature = function(tangent_vec_a, tangent_vec_b, tangent_vec_c, base_point) {
      super$get_python_class()$curvature(
        tangent_vec_a = tangent_vec_a,
        tangent_vec_b = tangent_vec_b,
        tangent_vec_c = tangent_vec_c,
        base_point = base_point
      )
    },

    #' @description Computes the directional curvature (tidal force operator).
    #'
    #' @details For two vector fields \eqn{X|_P = \mathrm{tangent\_vec\_a}} and
    #'   \eqn{Y|_P = \mathrm{tangent\_vec\_b}} with tangent vector specified in
    #'   argument at the base point \eqn{P}, the directional curvature, better
    #'   known in relativity as the tidal force operator, is defined by
    #'   \deqn{R_Y(X) = R(Y,X)Y.}
    #'
    #' @return Tangent vector at `base_point`.
    directional_curvature = function(tangent_vec_a, tangent_vec_b, base_point) {
      super$get_python_class()$directional_curvature(
        tangent_vec_a = tangent_vec_a,
        tangent_vec_b = tangent_vec_b,
        base_point = base_point
      )
    },

    #' @description Computes the covariant derivative of the curvature.
    #'
    #' @details For four vector fields \eqn{H|_P = \mathrm{tangent\_vec\_a}},
    #'   \eqn{X|_P = \mathrm{tangent\_vec\_b}}, \eqn{Y|_P =
    #'   \mathrm{tangent\_vec\_c}}, \eqn{Z|_P = \mathrm{tangent\_vec\_d}} with
    #'   tangent vector value specified in argument at the base point \eqn{P},
    #'   the covariant derivative of the curvature \eqn{(\nabla_H R)(X, Y) Z
    #'   |_P} is computed at the base point \eqn{P}.
    #'
    #' @param tangent_vec_d Tangent vector at `base_point`.
    #'
    #' @return Tangent vector at `base_point`.
    curvature_derivative = function(tangent_vec_a, tangent_vec_b,
                                    tangent_vec_c, tangent_vec_d,
                                    base_point = NULL) {
      super$get_python_class()$curvature_derivative(
        tangent_vec_a = tangent_vec_a,
        tangent_vec_b = tangent_vec_b,
        tangent_vec_c = tangent_vec_c,
        tangent_vec_d = tangent_vec_d,
        base_point = base_point
      )
    },

    #' @description Computes the covariant derivative of the directional
    #'   curvature.
    #'
    #' @details For two vector fields \eqn{X|_P = \mathrm{tangent\_vec\_a}},
    #'   \eqn{Y|_P = \mathrm{tangent\_vec\_b}} with tangent vector value
    #'   specified in argument at the base point \eqn{P}, the covariant
    #'   derivative (in the direction \eqn{X}) \eqn{(\nabla_X R_Y)(X) |_P =
    #'   (\nabla_X R)(Y, X) Y |_P} of the directional curvature (in the
    #'   direction \eqn{Y}) \eqn{R_Y(X) = R(Y, X) Y} is a quadratic tensor in
    #'   \eqn{X} and \eqn{Y} that plays an important role in the computation of
    #'   the moments of the empirical Fréchet mean
    #'   \insertCite{pennec2019curvature}{rgeomstats}.
    #'
    #' ## References
    #'
    #' \insertCited{}
    #'
    #' @return Tangent vector at `base_point`.
    directional_curvature_derivative = function(tangent_vec_a, tangent_vec_b,
                                                base_point = NULL) {
      super$get_python_class()$directional_curvature_derivative(
        tangent_vec_a = tangent_vec_a,
        tangent_vec_b = tangent_vec_b,
        base_point = base_point
      )
    },

    #' @description Generates parametrized function for the geodesic curve.
    #'
    #' @details Geodesic curve defined by either:
    #' - an initial point and an initial tangent vector,
    #' - an initial point and an end point.
    #'
    #' @param initial_point Point on the manifold specifying the initial point
    #'   of the geodesic.
    #' @param end_point Point on the manifold specifying the end point of the
    #'   geodesic. Defaults to `NULL`, in which case an initial tangent vector
    #'   must be given.
    #' @param initial_tangent_vec Tangent vector at base point specifying the
    #'   initial speed of the geodesics. Defaults to `NULL`, in which case an
    #'   end point must be given and a logarithm is computed.
    #'
    #' @return A function representing the time-parametrized geodesic curve. If
    #'   a list of initial conditions is passed, the output list will contain,
    #'   for each time point, a list with the geodesic values each initial
    #'   condition.
    geodesic = function(initial_point, end_point = NULL,
                        initial_tangent_vec = NULL) {
      multiple_init <- is.list(initial_point)
      if (multiple_init) {
        initial_point <- simplify2array(initial_point)
        n_dim <- length(dim(initial_point))
        initial_point <- aperm(initial_point, c(n_dim, 1:(n_dim - 1)))
      }
      gd <- super$get_python_class()$geodesic(
        initial_point = initial_point,
        end_point = end_point,
        initial_tangent_vec = initial_tangent_vec
      )
      function(time_points) {
        res <- reticulate::py_call(gd, time_points) |>
          reticulate::py_to_r() |>
          purrr::array_tree(margin = 1)
        if (multiple_init) {
          res <- res |>
            purrr::map(purrr::array_tree, margin = 1) |>
            purrr::transpose()
        }
        res
      }
    },

    #' @description Computes the parallel transport of a tangent vector.
    #'
    #' @details Closed-form solution for the parallel transport of a tangent
    #'   vector along the geodesic between two points `base_point` and
    #'   `end_point` or alternatively defined by \eqn{t \mapsto
    #'   \exp_\mathrm{base_point} (t \mathrm{direction})}.
    #'
    #' @param direction Tangent vector at base point specifying the point along
    #'   which the parallel transport is computed. Defaults to `NULL`.
    #' @param end_point Point on the manifold specifying the point to transport
    #'   to. Defaults to `NULL`.
    #'
    #' @return Tangent vector transported at \eqn{t \mapsto
    #'   \exp_\mathrm{base_point} (t \mathrm{direction})}.
    parallel_transport = function(tangent_vec, base_point,
                                  direction = NULL, end_point = NULL) {
      super$get_python_class()$parallel_transport(
        tangent_vec = tangent_vec,
        base_point = base_point,
        direction = direction,
        end_point = end_point
      )
    },

    #' @description Computes the radius of the injectivity domain.
    #'
    #' @details  This is is the supremum of radii r for which the exponential
    #'   map is a diffeomorphism from the open ball of radius r centered at the
    #'   base point onto its image.
    #'
    #' @return A numeric value representing the injectivity radius.
    injectivity_radius = function(base_point) {
      super$get_python_class()$injectivity_radius(base_point)
    }
  ),
  private = list(
    set_fields = function() {
      self$dim <- super$get_python_class()$dim
      self$shape <- super$get_python_class()$shape
      self$default_coords_type <- super$get_python_class()$default_coords_type
      self$default_point_type <- super$get_python_class()$default_point_type
    }
  )
)
