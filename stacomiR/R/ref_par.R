#' Class 'ref_par'
#' 
#' Class enabling to load the list of parameters and select one of them
#' @include create_generic.R
#' @slot data A data.frame
#' @slot par_selected A character vector corresponding to par_code
#' @section Objects from the Class: Objects can be created by calls of the form
#' @author cedric.briand@eptb-vilaine.fr
#' @keywords classes
#' @slot data='data.frame' the list of parameters
#' @family referential objects
setClass(Class = "ref_par", representation = representation(data = "data.frame",
    par_selected = "character"))


setValidity("ref_par", method = function(object) {
    if (length(object@par_selected) != 0) {
        if (nrow(object@data) > 0) {
            concord <- object@par_selected %in% object@data$par_code
            if (any(!concord)) {
                return(paste("No data for par", object@par_selected[!concord]))

            } else {
                return(TRUE)
            }
        } else {
            return("You tried to set a value for par_selected without initializing the data slot")
        }
    } else return(TRUE)

})
#' Loading method for ref_par referential objects
#' @aliases charge.ref_par
#' @param object An object of class \link{ref_par-class}
#' @return An S4 object of class ref_par
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @return An S4 object of class \link{ref_par-class} 
#' @examples 
#' \dontrun{
#'  object=new('ref_par')
#' charge(object)
#' }
setMethod("charge", signature = signature("ref_par"), definition = function(object) {
    requete = new("RequeteDB")
    requete@sql = paste("SELECT par_code, par_nom, par_unite, par_nature, par_definition  from ref.tg_parametre_par")
    requete <- stacomirtools::query(requete)
    #funout(gettext("Loading parameters query completed\n", domain = "R-stacomiR"))
    object@data <- requete@query
    return(object)
})


#' Loading method for \code{ref_par referential} objects searching only those parameters existing for a DC, a Taxa, and a stage
#' @aliases charge_with_filter.ref_par
#' @param object An object of class \link{ref_par-class}
#' @param dc_selected A counting device selected for the report 
#' @param taxa_selected The taxa selected for the report
#' @param stage_selected The stage selected for the report
#' @return An S4 object of class \link{ref_par-class} 
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new('ref_par')
#' charge_with_filter(object,dc_selected=6,taxa_selected=2038,stage_selected=c('AGJ','CIV'))
#' }
setMethod("charge_with_filter", signature = signature("ref_par"), definition = function(object,
    dc_selected, taxa_selected, stage_selected) {
    requete = new("RequeteDBwhere")
    requete@select = paste("SELECT DISTINCT ON (par_code) par_code, par_nom, par_unite, par_nature, par_definition", " FROM ",
				get_schema(), "tg_dispositif_dis", " JOIN ", get_schema(), "t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
        " JOIN ", get_schema(), "t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
        " JOIN ", get_schema(), "t_lot_lot on lot_ope_identifiant=ope_identifiant",
        " JOIN ", get_schema(), "tj_caracteristiquelot_car on car_lot_identifiant=lot_identifiant",
        " JOIN ref.tg_parametre_par on par_code=car_par_code", sep = "")
    requete@where = paste("where dis_identifiant in ", vector_to_listsql(dc_selected))
    requete@and = paste("and lot_tax_code in", vector_to_listsql(taxa_selected),
        " and lot_std_code in ", vector_to_listsql(stage_selected), sep = "")
    requete@order_by = "ORDER BY par_code"
    requete <- stacomirtools::query(requete)
    object@data <- requete@query
    if (nrow(object@data) == 0)
        funout(gettext("No data for selected device, taxa and stage\n", domain = "R-stacomiR"),
            arret = TRUE)
    return(object)
})


#' Command line interface to select a parameter
#' 
#' @aliases choice_c.ref_par
#' @param object an object of class  \link{ref_par-class}
#' @param par A character vector of par
#' @param silent Default FALSE but not used there
#' @return An object of class \link{ref_par-class}
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
setMethod("choice_c", signature = signature("ref_par"), definition = function(object,
    par, silent = FALSE) {
    if (inherits(par , "numeric")) {
        par <- as.character(par)
    }
    if (any(is.na(par)))
        stop("NA values par")
    object@par_selected <- par
    if (nrow(object@data) == 0) {
        stop("Internal error : tried to set a value for par_selected without initializing the data slot")
    }
    # validObject(object,test=FALSE) here I don't want to generate an error if
    # parm is not present so I'm not using the validObject which will throw and
    # error
    concord <- object@par_selected %in% object@data$par_code

    if (any(!concord)) {
        warning(paste(gettextf("No data for par %s", object@par_selected[!concord],
            domain = "R-stacomiR")))
    }
		# to work with daughter class
		if (inherits(object, "ref_parquan")) {
			assign("ref_parquan", object, envir = envir_stacomi)
			} else if (inherits(object, "ref_parqual")){
				assign("ref_parqual", object, envir = envir_stacomi)	
			} else {
				assign("ref_par", object, envir = envir_stacomi)
			}				

    return(object)
})



