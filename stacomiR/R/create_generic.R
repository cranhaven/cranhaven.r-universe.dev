#' Generic method for manual choice using the command line
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
#' @keywords internal
#' @export
setGeneric("choice_c", def = function(object, ...) standardGeneric("choice_c"))
#' Generic method to load referentials
#' @param object Object
#' @param ... Additional parm
#' @author cedric.briand
#' @export
setGeneric("charge", def = function(object, ...) standardGeneric("charge"))
#' Generic method to load referentials, with filters from the parent object in the database
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
#' @keywords internal
#' @export
setGeneric("charge_with_filter", def = function(object, ...) standardGeneric("charge_with_filter"))
# setGeneric('connect',def=function(object,...) standardGeneric('connect')) #
# package stacomirtools setGeneric('plot',def=function(x,y,...)
# standardGeneric('plot'))
#' Generic for prediction
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
#' @export
setGeneric("model", def = function(object, ...) standardGeneric("model"))
#' Generic method to load additional data
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
#' @export
setGeneric("charge_complement", def = function(object, ...) standardGeneric("charge_complement"))
#' Generic method for calculations
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
#' @export
setGeneric("calcule", def = function(object, ...) standardGeneric("calcule"))
#' Generic method to delete entires from the database
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
#' @seealso \link{calcule.report_ge_weight}, \link{calcule.report_mig_char}, \link{calcule.report_mig_env},
#' \link{calcule.report_mig_interannual},\link{calcule.report_mig_mult},\link{calcule.report_mig_mult},
#' \link{calcule.report_sample_char},  \link{calcule.report_sea_age},  \link{calcule.report_silver_eel},
#'  \link{calcule.report_species}
#' @export
setGeneric("supprime", def = function(object, ...) standardGeneric("supprime"))
#' Generic method write_database
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
#' @export
setGeneric("write_database", def = function(object, ...) standardGeneric("write_database"))
#' Generic method getvalue
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
#' @export
setGeneric("getvalue", def = function(object, ...) standardGeneric("getvalue"))
#' Generic method to transform quantitative par into a qualitative one
#' @param object Object
#' @param ... Additional parms 
#' @author cedric.briand
#' @export
setGeneric("setasqualitative", def = function(object, ...) standardGeneric("setasqualitative"))
#' Generic method for getting the final date
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric(
		"end_date",
		def = function(object, ...)
			standardGeneric("end_date")
)


#' Generic method to get the years
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric(
		"get_year",
		def = function(object, ...)
			standardGeneric("get_year")
)

#' Environment where most objects from the package are stored and then loaded
#' by the charge method
#' 
#' envir_stacomi \code{envir_stacomi <- new.env(parent = baseenv())} is the
#' environment where most object created by the interface are stored
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
"envir_stacomi"
