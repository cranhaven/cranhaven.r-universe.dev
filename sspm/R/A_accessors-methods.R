#' Accessing OR replacing `sspm` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of the different classes of the package.
#'
#' @param sspm_object **\[sspm OR adjacent\]** An object of class
#'     [sspm][sspm-class] or others derivative classes.
#' @param object **\[sspm OR adjacent\]** An object of class
#'     [sspm][sspm-class] or others derivative classes.
#' @inheritParams base::Extract
#' @inheritParams spm_as_dataset
#'
#' @aliases spm_
#' @rdname sspm-accessors-methods
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
#' spm_name(biomass_dataset)

# Name --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setGeneric(name = "spm_name",
           def = function(sspm_object) standardGeneric("spm_name")
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_name<-",
           def = function(object, value) standardGeneric("spm_name<-")
)

# Datasets ----------------------------------------------------------------
# Accesors ----------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_datasets",
           def = function(sspm_object) standardGeneric("spm_datasets")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_datasets", signature("sspm_object" = "sspm"),
          function(sspm_object) {
            datasets <- sspm_object@datasets
            names(datasets) <- sapply(datasets, spm_name)
            return(datasets)
          }
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_datasets<-",
           def = function(object, value) standardGeneric("spm_datasets<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_datasets<-",
          signature("object" = "sspm"),
          function(object, value) {
            object@datasets <- value
            validObject(object)
            return(object)
          }
)

# Boundaries --------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_boundaries",
           def = function(sspm_object) standardGeneric("spm_boundaries")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_boundaries", signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@boundaries
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_boundaries<-",
           def = function(object, value) standardGeneric("spm_boundaries<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_boundaries<-",
          signature("object" = "sspm"),
          function(object, value) {
            object@boundaries <- value
            validObject(object)
            return(object)
          }
)

# TODO dim should get dims of data and sf if discrete
# setMethod("dim",
#           "sspm", function(x) length(x@snpid))

# Smoothed data -----------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_smoothed_data",
           def = function(sspm_object) standardGeneric("spm_smoothed_data")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_smoothed_data",
          signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@smoothed_data
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_smoothed_data<-",
           def = function(object, value) standardGeneric("spm_smoothed_data<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_smoothed_data<-",
          signature("object" = "sspm"),
          function(object, value) {
            object@smoothed_data <- value
            validObject(object)
            return(object)
          }
)

# Time col ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_time",
           def = function(sspm_object) standardGeneric("spm_time")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_time",
          signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@time
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_time<-",
           def = function(object, value) standardGeneric("spm_time<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_time<-",
          signature("object" = "sspm"),
          function(object, value) {
            object@time <- value
            validObject(object)
            return(object)
          }
)

# Is split ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "is_split",
           def = function(sspm_object) standardGeneric("is_split")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("is_split",
          signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@is_split
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "is_split<-",
           def = function(object, value) standardGeneric("is_split<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("is_split<-",
          signature("object" = "sspm"),
          function(object, value) {
            object@is_split <- value
            validObject(object)
            return(object)
          }
)

# Unique ID ---------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_unique_ID",
           def = function(sspm_object) standardGeneric("spm_unique_ID")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_unique_ID",
          signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@uniqueID
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_unique_ID<-",
           def = function(object, value) standardGeneric("spm_unique_ID<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_unique_ID<-",
          signature("object" = "sspm"),
          function(object, value) {
            object@uniqueID <- value
            validObject(object)
            return(object)
          }
)
