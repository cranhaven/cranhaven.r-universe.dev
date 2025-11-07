#' Class 'ref_parquan'
#' 
#' Class enabling to load the list of quantitative parameters and to select one
#' of them. It inherits from 'ref_par', uses its 'choice' method
#' @author cedric.briand@eptb-vilaine.fr
#' @keywords classes
#' @family referential objects
#' @include ref_par.R
setClass(Class = "ref_parquan", contains = "ref_par")

#' Loading method for Reparquan referential objects
#' @param object An object of class \link{ref_parquan-class}
#' @return An S4 object of class \link{ref_parquan-class} with data loaded
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new('ref_parquan')
#'  charge(object)
#' }
setMethod("charge", signature = signature("ref_parquan"), definition = function(object) {
    requete = new("RequeteDB")
    requete@sql = "SELECT par_code, par_nom, par_unite, par_nature, par_definition FROM ref.tg_parametre_par 
	  INNER JOIN ref.tr_parametrequantitatif_qan ON qan_par_code=par_code"
    requete <- stacomirtools::query(requete)
    # funout(gettext('The query to load parameters is done
    # \n',domain='R-stacomiR'))
    object@data <- requete@query
    return(object)
})


#' Loading method for Reparquan referential objects searching only those parameters existing for a DC (counting device), a Taxon, and a stage
#' @param object An object of class \link{ref_parquan-class}
#' @param dc_selected The dc set in the report object
#' @param taxa_selected The taxa set in the report object
#' @param stage_selected The stage set in the report object
#' @return An S4 object of class \link{ref_parquan-class} with data loaded showing available parameters for one DC
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  dc_selected=6
#'taxa_selected=2038
#'  stage_selected='AGJ'
#'  object=new('ref_parquan')
#'  charge_with_filter(object,dc_selected,taxa_selected,stage_selected)
#' }  
setMethod("charge_with_filter", signature = signature("ref_parquan"), definition = function(object,
    dc_selected, taxa_selected, stage_selected) {
    requete = new("RequeteDBwhere")
    requete@select = paste("SELECT DISTINCT ON (par_code) par_code, par_nom, par_unite, par_nature, par_definition", " FROM ",
        get_schema(), "tg_dispositif_dis", " JOIN ", get_schema(), "t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
        " JOIN ", get_schema(), "t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
        " JOIN ", get_schema(), "t_lot_lot on lot_ope_identifiant=ope_identifiant",
        " JOIN ", get_schema(), "tj_caracteristiquelot_car on car_lot_identifiant=lot_identifiant",
        " JOIN ref.tg_parametre_par on par_code=car_par_code", " JOIN ref.tr_parametrequantitatif_qan ON qan_par_code=par_code",
        sep = "")
    requete@where = paste("where dis_identifiant in ", vector_to_listsql(dc_selected))
    requete@and = paste("and lot_tax_code in ", vector_to_listsql(taxa_selected),
        " and lot_std_code in ", vector_to_listsql(stage_selected), "", sep = "")
    requete@order_by = "ORDER BY par_code"
    requete <- stacomirtools::query(requete)
    object@data <- requete@query
    return(object)
})


