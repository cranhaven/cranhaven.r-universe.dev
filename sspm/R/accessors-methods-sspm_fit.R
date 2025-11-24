#' Accessing OR replacing `sspm_fit` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [sspm_fit][sspm_fit-class].
#'
#' @param sspm_object **\[sspm_fit\]** An object of class
#'     [sspm_fit][sspm_fit-class].
#' @param object **\[sspm_fit\]** An object of class
#'     [sspm_fit][sspm_fit-class].
#' @inheritParams base::Extract
#'
#' @rdname accessors-methods-sspm_fit
#'
#' @return
#' The object in the required slot.
#'
#' @examples
#' data(borealis_simulated, package = "sspm")
#' biomass_dataset <- spm_as_dataset(data.frame(borealis_simulated), name = "borealis",
#'                                   density = "weight_per_km2",
#'                                   time = "year_f",
#'                                   coords = c('lon_dec','lat_dec'),
#'                                   uniqueID = "uniqueID")
#' spm_formulas(biomass_dataset)

# Unique ID ---------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setMethod("spm_unique_ID",
          signature("sspm_object" = "sspm_fit"),
          function(sspm_object) sspm_object@uniqueID
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_fit
#' @export
setMethod("spm_unique_ID<-",
          signature("object" = "sspm_fit"),
          function(object, value) {
            object@uniqueID <- value
            validObject(object)
            return(object)
          }
)

# Time col ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_fit
#' @export
setMethod("spm_time",
          signature("sspm_object" = "sspm_fit"),
          function(sspm_object) sspm_object@time
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_fit
#' @export
setMethod("spm_time<-",
          signature("object" = "sspm_fit"),
          function(object, value) {
            object@time <- value
            validObject(object)
            return(object)
          }
)

# Formulas ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_fit
#' @export
setMethod("spm_formulas",
          signature("sspm_object" = "sspm_fit"),
          function(sspm_object) sspm_object@formula
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_fit
#' @export
setMethod("spm_formulas<-",
          signature("object" = "sspm_fit"),
          function(object, value) {
            object@formula <- value
            validObject(object)
            return(object)
          }
)

# Smoothed data -----------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_fit
#' @export
setMethod("spm_smoothed_data",
          signature("sspm_object" = "sspm_fit"),
          function(sspm_object) sspm_object@smoothed_data
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_fit
#' @export
setMethod("spm_smoothed_data<-",
          signature("object" = "sspm_fit"),
          function(object, value) {
            object@smoothed_data <- value
            validObject(object)
            return(object)
          }
)


# Get fit -----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_fit
#' @export
setGeneric(name = "spm_get_fit",
           def = function(sspm_object) standardGeneric("spm_get_fit")
)

#' @rdname accessors-methods-sspm_fit
#' @export
setMethod("spm_get_fit",
          signature("sspm_object" = "sspm_fit"),
          function(sspm_object) sspm_object@fit
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_fit
#' @export
setGeneric(name = "spm_get_fit<-",
           def = function(object, value) standardGeneric("spm_get_fit<-")
)

#' @rdname accessors-methods-sspm_fit
#' @export
setMethod("spm_get_fit<-",
          signature("object" = "sspm_fit"),
          function(object, value) {
            object@fit <- value
            validObject(object)
            return(object)
          }
)

# Boundaries --------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_fit
#' @export
setMethod("spm_boundaries", signature("sspm_object" = "sspm_fit"),
          function(sspm_object) sspm_object@boundaries
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_fit
#' @export
setMethod("spm_boundaries<-",
          signature("object" = "sspm_fit"),
          function(object, value) {
            object@boundaries <- value
            validObject(object)
            return(object)
          }
)


# Boundary_col ------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_fit
#' @export
setMethod("spm_boundary", signature("sspm_object" = "sspm_fit"),
          function(sspm_object) sspm_object@boundaries@boundary
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_fit
#' @export
setMethod("spm_boundary<-",
          signature("object" = "sspm_fit"),
          function(object, value) {
            object@boundaries@boundary <- value
            validObject(object)
            return(object)
          }
)
