# Nom fichier : ref_env (classe) Date de creation : 02/01/2009 15:02:40

#' Class 'ref_env'
#' 
#' Enables to load measure stations and to select one of them
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new('ref_env', ...)}. 
#' @slot dataframe Data concerning the
#' measure station
#' @slot env_selected The selected measure station 
#' @author cedric.briand@eptb-vilaine.fr
#' @keywords classes
setClass(Class = "ref_env", representation = 
				representation(data = "data.frame",
						env_selected="character"),
    prototype = prototype(
				data = data.frame(),
				env_selected=character()))

#' Loading method for ref_env referential object
#' @return An S4 object of class ref_env with data loaded from the database
#' @param object An object of class \link{ref_env-class}
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new('ref_env')
#'  charge(object)
#' }
setMethod("charge", signature = signature("ref_env"), definition = function(object) {
    requete = new("RequeteDB")
    requete@sql = paste("SELECT stm_identifiant, stm_libelle, stm_sta_code, stm_par_code, stm_description",
        " FROM ", get_schema(), "tj_stationmesure_stm", " ORDER BY stm_identifiant;",
        sep = "")
    requete@silent = TRUE
    requete <- stacomirtools::query(requete)
    object@data <- requete@query
    return(object)
})



#' Command line interface to select a monitoring  station
#' 
#' the choice_c method is intended to have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line. 
#' @param object an object of class ref_env
#' @param stationMesure a character vector of the monitoring station code (corresponds to stm_libelle in the tj_stationmesure_stm table)
#' @return an object of class \link{ref_env-class} with the monitoring station selected
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
setMethod("choice_c", signature = signature("ref_env"), definition = function(object,
    stationMesure) {
    if (!inherits(stationMesure, "character")) {
        stop("the stationmesure should be of class character")
    }
    if (length(stationMesure) == 0) {
        stop("Select at least one value\n")
    }
    if (any(is.na(stationMesure))) {
        stop("NA values for stationmesure")
    }
    # I can use the stm_libelle as there is a unique constraint in the table
    libellemanquants <- stationMesure[!stationMesure %in% object@data$stm_libelle]
    if (length(libellemanquants) > 0)
        warning(gettextf("stationmesure code not present :\n %s", stringr::str_c(libellemanquants,
            collapse = ", "), domain = "R-stacomiR"))
    object@env_selected <- object@data$stm_libelle[object@data$stm_libelle %in% stationMesure]
    assign("ref_env", object, envir_stacomi)
    return(object)
})
