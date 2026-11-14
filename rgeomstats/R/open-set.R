#' Abstract Class for Open Set Manifolds
#'
#' @description Class for manifolds that are open sets of a vector space. In
#'   this case, tangent vectors are identified with vectors of the ambient
#'   space.
#'
#' @author Nicolas Guigui and Nina Miolane
#'
#' @seealso SPDMatrix
#'
#' @keywords internal
OpenSet <- R6::R6Class(
  classname = "OpenSet",
  inherit = Manifold,
  public = list(
    #' @field ambient_space An object of class [`VectorSpace`] specifying the
    #'   ambient space.
    ambient_space = NULL,

    #' @description The [`OpenSet`] class constructor.
    #'
    #' @param dim An integer value specifying the dimension of the manifold.
    #' @param ambient_space An object of class [`VectorSpace`] specifying the
    #'   ambient space.
    #' @param ... Extra arguments to be passed to parent class constructors. See
    #'   [`Manifold`] class.
    #' @param py_cls A Python object of class `OpenSet`. Defaults to `NULL` in
    #'   which case it is instantiated on the fly using the other input
    #'   arguments.
    #'
    #' @return An object of class [`OpenSet`].
    initialize = function(dim, ambient_space, ..., py_cls = NULL) { # nocov start
      if (is.null(py_cls)) {
        dots <- capture_extra_params(...)
        dots$dim <- as.integer(dim)
        if ("shape" %in% names(dots)) {
          dots$shape <- dots$shape |>
            purrr::map(as.integer) |>
            reticulate::tuple()
        }
        if ("metric" %in% names(dots))
          dots$metric <- dots$metric$get_python_class()
        dots$ambient_space <- ambient_space$get_python_class()
        py_cls <- do.call(gs$geometry$base$OpenSet, dots)
      }
      super$set_python_class(py_cls)
      private$set_fields()
    }, # nocov end

    #' @description Project a point in the ambient space onto the manifold.
    #'
    #' @param point A numeric array of shape \eqn{[\dots \times
    #'   \{\mathrm{dim}\}]} specifying one or more vectors in the ambient space
    #'   of the manifold.
    #'
    #' @return A numeric array of the same shape storing the corresponding
    #'   projections onto the manifold.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   spd3 <- SPDMatrix(n = 3)
    #'   spd3$projection(diag(1, 3))
    #' }
    projection = function(point) {
      super$get_python_class()$projection(point = point)
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
      self$ambient_space <- super$get_python_class()$ambient_space
    }
  )
)
