#' Class 'ref_parqual'
#' 
#' Class enabling to load the list of qualitative parameters and to select one
#' of them. It inherits from 'ref_par', uses its 'choice' method
#' @author cedric.briand@eptb-vilaine.fr
#' @slot valqual='data.frame' the list of qualitative parameters
#' @include ref_par.R
#' @family referential objects
setClass(Class = "ref_parqual", representation = representation(valqual = "data.frame"),
    contains = "ref_par")

#' Loading method for Reparqual referential objects
#' @param object An object of class \link{ref_parqual-class}
#' @return An S4 object of class ref_parqual
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new('ref_parqual')
#'  charge(object)
#' }
setMethod("charge", signature = signature("ref_parqual"), definition = function(object) {
    requete = new("RequeteDB")
    requete@sql = "select par_code, par_nom, par_unite, par_nature, par_definition, qal_valeurs_possibles from ref.tg_parametre_par
     INNER JOIN ref.tr_parametrequalitatif_qal ON tr_parametrequalitatif_qal.qal_par_code::text = tg_parametre_par.par_code::text"
    requete <- stacomirtools::query(requete)
    # funout(gettext('The query to load parameters is done
    # \n',domain='R-stacomiR'))
    object@data <- requete@query
    return(object)
})

#' Loading method for Reparqual referential objects searching only those parameters existing for a DC, a Taxon, and a stage
#' @param object An object of class \link{ref_parqual-class}
#' @param dc_selected The dc set in the report object
#' @param taxa_selected The taxa set in the report object
#' @param stage_selected The stage set in the report object
#' @return An S4 object of class \link{ref_parqual-class}
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  dc_selected=6
#'  taxa_selected=2038
#'  stage_selected='AGJ'
#'  object=new('ref_parqual')
#'  charge_with_filter(object,dc_selected,taxa_selected,stage_selected)
#' }
setMethod("charge_with_filter", signature = signature("ref_parqual"), definition = function(object,
    dc_selected, taxa_selected, stage_selected) {
    requete = new("RequeteDBwhere")
    requete@select = paste("SELECT DISTINCT ON (par_code) par_code, par_nom, par_unite, par_nature, par_definition, qal_valeurs_possibles", " FROM ",
        get_schema(), "tg_dispositif_dis", " JOIN ", get_schema(), "t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
        " JOIN ", get_schema(), "t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
        " JOIN ", get_schema(), "t_lot_lot on lot_ope_identifiant=ope_identifiant",
        " JOIN ", get_schema(), "tj_caracteristiquelot_car on car_lot_identifiant=lot_identifiant",
        " JOIN ref.tg_parametre_par on par_code=car_par_code", " JOIN ref.tr_parametrequalitatif_qal ON tr_parametrequalitatif_qal.qal_par_code::text = tg_parametre_par.par_code::text",
        sep = "")
    requete@where = paste("where dis_identifiant in ", vector_to_listsql(dc_selected))
    requete@and = paste("and lot_tax_code in ", vector_to_listsql(taxa_selected),
        " and lot_std_code in ", vector_to_listsql(stage_selected), sep = "")
    requete@order_by = "ORDER BY par_code"
    requete <- stacomirtools::query(requete)
    object@data <- requete@query
    return(object)
})

#'  Loads an additional dataset
#' this method is loaded to obtain the possible values of a qualitative parameter. Here data only contains one line
#' @param object An object of class \link{ref_parqual-class}
#' @return An S4 object of class \link{ref_parqual-class} with the valqual slot filled
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  dc_selected=6
#'taxa_selected=2038
#'  stage_selected='AGJ'
#'  object=new('ref_parqual')
#'  object<-charge(object)
#'  charge_complement(object)
#' }  
setMethod("charge_complement", signature = signature("ref_parqual"), definition = function(object) {
    requete = new("RequeteDB")
    requete@sql = paste("select * from ref.tr_valeurparametrequalitatif_val", " WHERE val_qal_code in ",
        vector_to_listsql(object@par_selected), " ORDER BY val_rang", sep = "")
    requete <- stacomirtools::query(requete)
    # funout(gettext('The query to load parameters is done
    # \n',domain='R-stacomiR'))
    object@valqual <- requete@query
    return(object)
})


