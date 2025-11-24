#' Accessing OR replacing `discretization_method` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [discretization_method][discretization_method-class].
#'
#' @param sspm_object **\[discretization_method\]** An object of class
#'   [discretization_method][discretization_method-class].
#' @param object **\[discretization_method\]** An object of class
#'     [discretization_method][discretization_method-class].
#' @inheritParams base::Extract
#'
#' @rdname accessors-methods-discret_method
#'
#' @return
#' The object in the required slot.
#'
#' @examples
#' method <- as_discretization_method("tesselate_voronoi")
#' method_func(method)

# Function ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setGeneric(name = "method_func",
           def = function(sspm_object) standardGeneric("method_func")
)

#' @rdname accessors-methods-discret_method
#' @export
setMethod("method_func",
          signature("sspm_object" = "discretization_method"),
          function(sspm_object) sspm_object@method
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-discret_method
#' @export
setGeneric(name = "method_func<-",
           def = function(object, value) standardGeneric("method_func<-")
)

#' @rdname accessors-methods-discret_method
#' @export
setMethod("method_func<-",
          signature("object" = "discretization_method"),
          function(object, value) {
            object@method <- value
            validObject(object)
            return(object)
          }
)

# Name --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-discret_method
#' @export
setMethod("spm_name", signature("sspm_object" = "discretization_method"),
          function(sspm_object) sspm_object@name
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-discret_method
#' @export
setMethod("spm_name<-",
          signature("object" = "discretization_method"),
          function(object, value) {
            object@name <- value
            validObject(object)
            return(object)
          }
)
