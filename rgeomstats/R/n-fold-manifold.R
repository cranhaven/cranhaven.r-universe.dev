#' Class for N-Fold Product Manifolds
#'
#' @description Class for an \eqn{n}-fold product manifold \eqn{M^n}. It defines
#'   a manifold as the product manifold of \eqn{n} copies of a given base
#'   manifold \eqn{M}.
#'
#' @author Nicolas Guigui
#' @family product manifold
#'
#' @export
NFoldManifold <- R6::R6Class(
  classname = "NFoldManifold",
  inherit = Manifold,
  public = list(
    #' @description The [`NFoldManifold`] class constructor.
    #'
    #' @param base_manifold An [R6::R6Class] specifying the base manifold to
    #'   copy.
    #' @param n_copies An integer value specifying the number of replication of
    #'   the base manifold.
    #' @param metric An [R6::R6Class] specifying the base metric to use.
    #'   Defaults to `NULL` which uses the Riemannian metric.
    #' @param default_coords_type A string specifying the coordinate type.
    #'   Choices are `"intrinsic"` or `"extrinsic"`. Defaults to `"intrinsic"`.
    #' @param py_cls A Python object of class `NFoldManifold`. Defaults to
    #'   `NULL` in which case it is instantiated on the fly using the other
    #'   input arguments.
    #'
    #' @return A [`NFoldManifold`] [R6::R6Class] object.
    #'
    #' @examples
    #' if (reticulate::py_module_available("geomstats")) {
    #'   nfm <- NFoldManifold$new(
    #'     base_manifold = SPDMatrix(n = 3),
    #'     n_copies = 3
    #'   )
    #'   nfm
    #' }
    initialize = function(base_manifold,
                          n_copies,
                          metric = NULL,
                          default_coords_type = "intrinsic",
                          py_cls = NULL) {
      if (is.null(py_cls)) {
        n_copies <- as.integer(n_copies)
        if (!is.null(metric)) metric <- metric$get_python_class()
        py_cls <- gs$geometry$product_manifold$NFoldManifold(
          base_manifold = base_manifold$get_python_class(),
          n_copies = n_copies,
          metric = metric,
          default_coords_type = default_coords_type
        )
      }
      super$set_python_class(py_cls)
    }
  )
)
