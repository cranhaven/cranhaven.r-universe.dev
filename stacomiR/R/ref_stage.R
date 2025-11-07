#' Class 'ref_stage'
#' 
#' Representation of a fish phase
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new('ref_stage', data='data.frame')}.  \describe{
#' \item{list('data')}{Object of class \code{'data.frame'} ~ The phases
#' available in the database}\item{:}{Object of class \code{'data.frame'} ~ The
#' phases available in the database} }
#' @slot data A data frame containing data loaded from the database by either charge or charge_with_filter methods
#' @slot stage_selected Contains the code \code{'tax_code'} of the stage selected by choice_c() method
#' @author cedric.briand@eptb-vilaine.fr
#' @keywords classes
#' @family referential objects
setClass(Class = "ref_stage", representation = representation(data = "data.frame", stage_selected="character"))

#' Loading method for ref_stage referential objects
#' @param object An object of class \link{ref_stage-class}
#' @return An S4 object of class \link{ref_stage-class} with all stages available in the database
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new('ref_stage')
#'  charge(object)
#' }
setMethod("charge", signature = signature("ref_stage"), definition = function(object) {
    req = new("RequeteDB")
    req@sql = "SELECT std_code, std_libelle FROM ref.tr_stadedeveloppement_std ORDER BY std_code ;"
    req <- stacomirtools::query(req) 
    object@data <- req@query
    return(object)
})


#' Loading method for ref_stage referential objects searching only those stages existing for a DC and a Taxon
#' @param object An object of class \link{ref_stage-class}
#' @param dc_selected The selected counting device
#' @param taxa_selected The selected species
#' @return An S4 object of class \link{ref_stage-class} listing all stages available for one DC and one taxon
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  dc_selected=6
#'taxa_selected=2038
#'  object=new('ref_stage')
#'  charge_with_filter(object,dc_selected,taxa_selected)
#' }
setMethod("charge_with_filter", signature = signature("ref_stage"), definition = function(object,
    dc_selected, taxa_selected) {
    requete = new("RequeteDBwhere")
    requete@select = paste("SELECT DISTINCT ON (std_code) std_code, std_libelle",
        " FROM ", get_schema(), "tg_dispositif_dis", " JOIN ",
        get_schema(), "t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
        " JOIN ", get_schema(), "t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
        " JOIN ", get_schema(), "t_lot_lot on lot_ope_identifiant=ope_identifiant",
        " JOIN ref.tr_stadedeveloppement_std on lot_std_code=std_code", sep = "")
    requete@where = paste("where dis_identifiant in ", vector_to_listsql(dc_selected),
        sep = "")
    requete@and = paste("and lot_tax_code in ", vector_to_listsql(taxa_selected),
        sep = "")
    requete@order_by = "ORDER BY std_code"
    requete <- stacomirtools::query(requete)  # appel de la methode connect de l'object requeteDB
    object@data <- requete@query
    if (nrow(object@data) == 0)
        funout(gettext("No data for this counting device and this taxa\n", domain = "R-stacomiR"),
            arret = TRUE)
    return(object)
})


#' choice_c method for ref_stage
#' 
#' the choice_c method is intended to have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line. The values passed to the choice_c method
#' for stage is the code.  Any numeric value will be discarded
#' @param object An object of class \link{ref_stage-class}
#' @param stage the vector of stages chosen
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An S4 object of class \link{ref_stage-class} with the stage selected in the data slot
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples
#' \dontrun{
#'object=new('ref_stage')
#'object<-charge(object)
#' }
setMethod("choice_c", signature = signature("ref_stage"), definition = function(object,
    stage, silent = FALSE) {
    if (is.null(stage)) {
        funout(gettext("No value for argument stage\n", domain = "R-stacomiR"), arret = TRUE)
    }
    missing_std_libelle <- stage[!stage %in% object@data$std_code]
    if (length(missing_std_libelle) > 0 & !silent)
        funout(gettextf("No data for this counting device and this taxa\n %s", stringr::str_c( missing_std_libelle,
            collapse = ", "), domain = "R-stacomiR"))
    object@stage_selected <- object@data[object@data$std_code %in% stage,"std_code"]
    if (nrow(object@data) == 0) {
        funout(gettext("Stop there is no line in the taxa table (problem with the DB link ?)\n",
            domain = "R-stacomiR"), arret = TRUE)
    }
    assign("ref_stage", object, envir = envir_stacomi)
    return(object)
})
