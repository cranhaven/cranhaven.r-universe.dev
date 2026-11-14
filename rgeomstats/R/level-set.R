#' Abstract Class for Level Set Manifolds
#'
#' @description Class for manifolds embedded in a vector space by a submersion.
#'
#' @author Nicolas Guigui and Nina Miolane
#'
#' @keywords internal
LevelSet <- R6::R6Class(
  classname = "LevelSet",
  inherit = Manifold,
  public = list(
    #' @field embedding_space An object of class [`VectorSpace`] specifying the
    #'   embedding space.
    embedding_space = NULL,

    #' @field embedding_metric ???
    embedding_metric = NULL,

    #' @field submersion ???
    submersion = NULL,

    #' @field value ???
    value = NULL,

    #' @field tangent_submersion ???
    tangent_submersion = NULL,

    #' @description The [`LevelSet`] class constructor.
    #'
    #' @param dim An integer value specifying the dimension of the manifold.
    #' @param embedding_space An object of class [`VectorSpace`] specifying the
    #'   embedding space.
    #' @param submersion ???
    #' @param value ???
    #' @param tangent_submersion ???
    #' @param default_coords_type A string specifying the coordinate type.
    #'   Choices are `extrinsic` or `intrinsic`. Defaults to `intrinsic`.
    #' @param ... Extra arguments to be passed to parent class constructors. See
    #'   [`Manifold`] class.
    #' @param py_cls A Python object of class `LevelSet`. Defaults to `NULL` in
    #'   which case it is instantiated on the fly using the other input
    #'   arguments.
    #'
    #' @return An object of class [`LevelSet`].
    initialize = function(dim,
                          embedding_space,
                          submersion,
                          value,
                          tangent_submersion,
                          default_coords_type = "intrinsic",
                          ...,
                          py_cls = NULL) { # nocov start
      if (is.null(py_cls)) {
        dots <- capture_extra_params(...)
        dots$dim <- as.integer(dim)
        if ("shape" %in% names(dots)) {
          dots$shape <- dots$shape |>
            purrr::map(as.integer) |>
            reticulate::tuple()
        }
        dots$embedding_space <- embedding_space$get_python_class()
        dots$submersion <- submersion
        dots$value <- if (is.null(dim(value))) as.array(value) else value
        dots$tangent_submersion <- tangent_submersion
        dots$default_coords_type <- default_coords_type
        py_cls <- do.call(gs$geometry$base$LevelSet, dots)
      }
      super$set_python_class(py_cls)
      private$set_fields()
    }, # nocov end

    #' @description Converts from intrinsic to extrinsic coordinates.
    #'
    #' @param point_intrinsic A numeric array of shape `dim` specifying a point
    #'   in the embedded manifold in intrinsic coordinates.
    #'
    #' @return A numeric array of shape `dim_embedding` representing the same
    #'   point in the embedded manifold in extrinsic coordinates.
    intrinsic_to_extrinsic_coords = function(point_intrinsic) {
      super$get_python_class()$intrinsic_to_extrinsic_coords(
        point_intrinsic = point_intrinsic
      )
    },

    #' @description Converts from extrinsic to intrinsic coordinates.
    #'
    #' @param point_extrinsic A numeric array of shape `dim_embedding`
    #'   specifying a point in the embedded manifold in extrinsic coordinates,
    #'   i.E. in the coordinates of the embedding manifold.
    #'
    #' @return A numeric array of shape `dim` representing the same point in the
    #'   embedded manifold in intrinsic coordinates.
    extrinsic_to_intrinsic_coords = function(point_extrinsic) {
      super$get_python_class()$extrinsic_to_intrinsic_coords(
        point_extrinsic = point_extrinsic
      )
    },

    #' @description Projects a point in embedding manifold on embedded manifold.
    #'
    #' @param point A numeric array of shape `dim_embedding` specifying a point
    #'   in the embedding manifold.
    #'
    #' @return A numeric array of shape `dim_embedding` storing the projected
    #'   point.
    projection = function(point) {
      super$get_python_class()$projection(point = point)
    }
  ),
  private = list(
    set_fields = function() {
      super$set_fields()
      self$embedding_space <- super$get_python_class()$embedding_space
      self$embedding_metric <- super$get_python_class()$embedding_space$metric
      self$submersion <- super$get_python_class()$submersion
      self$value <- super$get_python_class()$value
      self$tangent_submersion <- super$get_python_class()$tangent_submersion
    }
  )
)
