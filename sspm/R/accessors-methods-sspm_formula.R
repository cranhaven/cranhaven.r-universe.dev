#' Accessing OR replacing `sspm_formula` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [sspm_formula][sspm_formula-class].
#'
#' @param sspm_object **\[sspm_formula\]** An object of class
#'     [sspm_formula][sspm_formula-class].
#' @param object **\[sspm_formula\]** An object of class
#'     [sspm_formula][sspm_formula-class].
#' @inheritParams base::Extract
#'
#' @rdname accessors-methods-sspm_formula
#'
#' @return
#' The object in the required slot.
#'
#' @examples
#' form <- new("sspm_formula",
#'             raw_formula = as.formula("weight_per_km2 ~ smooth_time()"),
#'             translated_formula = as.formula("weight_per_km2 ~ s(year_f,
#'                       k = 24L, bs = 're', xt = list(penalty = pen_mat_time))"),
#'                     vars = list(pen_mat_time = matrix(),
#'                                 pen_mat_space = matrix()),
#'                     response = "weight_per_km2")
#' translated_formula(form)

# Raw formula -------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setGeneric(name = "raw_formula",
           def = function(sspm_object) standardGeneric("raw_formula")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("raw_formula",
          signature("sspm_object" = "sspm_formula"),
          function(sspm_object) sspm_object@raw_formula
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_formula
#' @export
setGeneric(name = "raw_formula<-",
           def = function(object, value) standardGeneric("raw_formula<-")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("raw_formula<-",
          signature("object" = "sspm_formula"),
          function(object, value) {
            object@raw_formula <- value
            validObject(object)
            return(object)
          }
)

# Translated_formula ------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_formula
#' @export
setGeneric(name = "translated_formula",
           def = function(sspm_object) standardGeneric("translated_formula")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("translated_formula",
          signature("sspm_object" = "sspm_formula"),
          function(sspm_object) sspm_object@translated_formula
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_formula
#' @export
setGeneric(name = "translated_formula<-",
           def = function(object, value) standardGeneric("translated_formula<-")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("translated_formula<-",
          signature("object" = "sspm_formula"),
          function(object, value) {
            object@translated_formula <- value
            validObject(object)
            return(object)
          }
)

# Vars --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_formula
#' @export
setGeneric(name = "formula_vars",
           def = function(sspm_object) standardGeneric("formula_vars")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("formula_vars",
          signature("sspm_object" = "sspm_formula"),
          function(sspm_object) sspm_object@vars
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_formula
#' @export
setGeneric(name = "formula_vars<-",
           def = function(object, value) standardGeneric("formula_vars<-")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("formula_vars<-",
          signature("object" = "sspm_formula"),
          function(object, value) {
            object@vars <- value
            validObject(object)
            return(object)
          }
)

# Type --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_formula
#' @export
setGeneric(name = "formula_type",
           def = function(sspm_object) standardGeneric("formula_type")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("formula_type",
          signature("sspm_object" = "sspm_formula"),
          function(sspm_object) sspm_object@type
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_formula
#' @export
setGeneric(name = "formula_type<-",
           def = function(object, value) standardGeneric("formula_type<-")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("formula_type<-",
          signature("object" = "sspm_formula"),
          function(object, value) {
            object@type <- value
            validObject(object)
            return(object)
          }
)

# Is_fitted ---------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_formula
#' @export
setGeneric(name = "is_fitted",
           def = function(sspm_object) standardGeneric("is_fitted")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("is_fitted",
          signature("sspm_object" = "sspm_formula"),
          function(sspm_object) sspm_object@is_fitted
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_formula
#' @export
setGeneric(name = "is_fitted<-",
           def = function(object, value) standardGeneric("is_fitted<-")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("is_fitted<-",
          signature("object" = "sspm_formula"),
          function(object, value) {
            object@is_fitted <- value
            validObject(object)
            return(object)
          }
)

# Response ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_formula
#' @export
setGeneric(name = "spm_response",
           def = function(sspm_object) standardGeneric("spm_response")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("spm_response",
          signature("sspm_object" = "sspm_formula"),
          function(sspm_object) sspm_object@response
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_formula
#' @export
setGeneric(name = "spm_response<-",
           def = function(object, value) standardGeneric("spm_response<-")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("spm_response<-",
          signature("object" = "sspm_formula"),
          function(object, value) {
            object@response <- value
            validObject(object)
            return(object)
          }
)

# Lagged vars -------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_formula
#' @export
setGeneric(name = "spm_lagged_vars",
           def = function(sspm_object) standardGeneric("spm_lagged_vars")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("spm_lagged_vars",
          signature("sspm_object" = "sspm_formula"),
          function(sspm_object) sspm_object@lag_vars
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_formula
#' @export
setGeneric(name = "spm_lagged_vars<-",
           def = function(object, value) standardGeneric("spm_lagged_vars<-")
)

#' @rdname accessors-methods-sspm_formula
#' @export
setMethod("spm_lagged_vars<-",
          signature("object" = "sspm_formula"),
          function(object, value) {
            object@lag_vars <- value
            validObject(object)
            return(object)
          }
)
