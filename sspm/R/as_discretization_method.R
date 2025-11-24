#' Cast into a `discretization_method` object
#'
#' Cast a character value into [`discretization_method`][discretization_method-class]
#' object, using the list of possible methods in [`spm_methods`][spm_methods].
#'
#' @param name **\[character\]** The name of the method.
#' @param method **\[character\]** If custom method, the function to use. See
#'     [`spm_discretize`][spm_discretize] for more details.
#'
#' @return
#' An objectof class [`discretization_method`][discretization_method-class].
#'
#' @seealso [spm_methods].
#'
#' @examples
#' as_discretization_method("tesselate_voronoi")
#'
#' @export
setGeneric(name = "as_discretization_method",
           def = function(name, method) {
             standardGeneric("as_discretization_method")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname as_discretization_method
setMethod(f = "as_discretization_method",
          signature(name = "character"),
          function(name) {

            method_f <- dispatch_method(name)

            if (!is.character(method_f)) {

              method_object <- new("discretization_method",
                                   name = name,
                                   method = method_f)

              return(method_object)
            }

          }
)

#' @export
#' @rdname as_discretization_method
setMethod(f = "as_discretization_method",
          signature(name = "missing", method = "function"),
          function(method) {

            method_object <- new("discretization_method",
                                 name = "Custom",
                                 method = method)

            return(method_object)

          }
)

# Helpers -----------------------------------------------------------------

# Dispatch the correct function based on the name of the method
dispatch_method <- function(discretization_method) {

  checkmate::assert_character(discretization_method)

  if (discretization_method == "tesselate_voronoi") {
    return(tesselate_voronoi)
  } else if (discretization_method == "triangulate_delaunay") {
    return(triangulate_delaunay)
  } else {
    message <- paste0("Method '", discretization_method,
                      "' is not part of the supported methods.")
    cli::cli_alert_danger(message)
    cli::cli_alert_info("See `?spm_methods()`")
    stop(message, call. = FALSE)
  }
}
