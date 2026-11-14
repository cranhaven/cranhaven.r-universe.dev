#' Abstract Class for Vector Space Manifolds
#'
#' @description Abstract class for vector spaces.
#'
#' @author Nicolas Guigui and Nina Miolane
#'
#' @keywords internal
VectorSpace <- R6::R6Class(
  classname = "VectorSpace",
  inherit = Manifold,
  public = list(
    #' @field basis Basis of the vector space.
    basis = NULL,

    #' @description The [`VectorSpace`] class constructor.
    #'
    #' @param shape An integer vector specifying the shape of one element of the
    #'   manifold. Defaults to `NULL`.
    #' @param ... Extra arguments to be passed to parent class constructors. See
    #'   [`Manifold`] class.
    #' @param py_cls A Python object of class `VectorSpace`. Defaults to `NULL`
    #'   in which case it is instantiated on the fly using the other input
    #'   arguments.
    #'
    #' @return An object of class [`VectorSpace`].
    initialize = function(shape, ..., py_cls = NULL) {
      if (is.null(py_cls)) {
        dots <- capture_extra_params(...)
        dots$shape <- as.integer(shape)
        py_cls <- do.call(gs$geometry$manifold$VectorSpace, dots)
      }
      super$set_python_class(py_cls)
      private$set_fields()
    },

    #' @description Project a point onto the vector space.
    #'
    #' @details This method is for compatibility and returns `point`. `point`
    #'   should have the right shape.
    #'
    #' @param point A numeric array of shape `dim` specifying a vector in the
    #'   ambient space onto the manifold.
    #'
    #' @return A numeric array of shape `dim` storing the input `vector`
    #'   projected onto the manifold.
    projection = function(point) {
      super$get_python_class()$projection(point)
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
    }
  )
)
