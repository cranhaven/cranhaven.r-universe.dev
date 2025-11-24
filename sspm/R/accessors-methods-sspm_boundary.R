#' Accessing OR replacing `sspm_boundary` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [sspm_boundary][sspm_boundary-class].
#'
#' @param sspm_object **\[sspm_boundary\]** An object of class
#'     [sspm_boundary][sspm_boundary-class].
#' @param object **\[sspm_boundary\]** An object of class
#'     [sspm_boundary][sspm_boundary-class].
#'
#' @inheritParams base::Extract
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
#' spm_boundaries(biomass_dataset)

# Boundaries --------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_boundaries", signature("sspm_object" = "sspm_boundary"),
          function(sspm_object) sspm_object@boundaries
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_boundaries<-",
          signature("object" = "sspm_boundary"),
          function(object, value) {
            object@boundaries <- value
            validObject(object)
            return(object)
          }
)

# DISCRETE BEYOND THIS POINT ----------------------------------------------

# Discretization method ---------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_discret_method",
           def = function(sspm_object) standardGeneric("spm_discret_method")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_discret_method",
          signature("sspm_object" = "sspm_discrete_boundary"),
          function(sspm_object) sspm_object@method
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_discret_method<-",
           def = function(object, value) standardGeneric("spm_discret_method<-")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_discret_method<-",
          signature("object" = "sspm_discrete_boundary"),
          function(object, value) {
            object@method <- value
            validObject(object)
            return(object)
          }
)

# Patches -----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_patches",
           def = function(sspm_object) standardGeneric("spm_patches")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_patches",
          signature("sspm_object" = "sspm_discrete_boundary"),
          function(sspm_object) sspm_object@patches
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_patches<-",
           def = function(object, value) standardGeneric("spm_patches<-")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_patches<-",
          signature("object" = "sspm_discrete_boundary"),
          function(object, value) {
            object@patches <- value
            validObject(object)
            return(object)
          }
)

# Points ------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_points",
           def = function(sspm_object) standardGeneric("spm_points")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_points",
          signature("sspm_object" = "sspm_discrete_boundary"),
          function(sspm_object) sspm_object@points
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_points<-",
           def = function(object, value) standardGeneric("spm_points<-")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_points<-",
          signature("object" = "sspm_discrete_boundary"),
          function(object, value) {
            object@points <- value
            validObject(object)
            return(object)
          }
)

# Boundary col ------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_boundary",
           def = function(sspm_object) standardGeneric("spm_boundary")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_boundary",
          signature("sspm_object" = "sspm_boundary"),
          function(sspm_object) sspm_object@boundary
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_boundary<-",
           def = function(object, value) standardGeneric("spm_boundary<-")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_boundary<-",
          signature("object" = "sspm_boundary"),
          function(object, value) {
            object@boundary <- value
            validObject(object)
            return(object)
          }
)

# Surface col -------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_boundary_area",
           def = function(sspm_object) standardGeneric("spm_boundary_area")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_boundary_area",
          signature("sspm_object" = "sspm_boundary"),
          function(sspm_object) sspm_object@boundary_area
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_boundary_area<-",
           def = function(object, value) standardGeneric("spm_boundary_area<-")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_boundary_area<-",
          signature("object" = "sspm_boundary"),
          function(object, value) {
            object@boundary_area <- value
            validObject(object)
            return(object)
          }
)

# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_patches_area",
           def = function(sspm_object) standardGeneric("spm_patches_area")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_patches_area",
          signature("sspm_object" = "sspm_discrete_boundary"),
          function(sspm_object) sspm_object@patches_area
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_patches_area<-",
           def = function(object, value) standardGeneric("spm_patches_area<-")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_patches_area<-",
          signature("object" = "sspm_discrete_boundary"),
          function(object, value) {
            object@patches_area <- value
            validObject(object)
            return(object)
          }
)
