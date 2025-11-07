#' Class 'ref_taxa'
#' 
#' Loading and selection of fish species. This class is a referential class, and it is 
#' integrated into refreport objects.
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new('ref_taxa', ...)}.   
#' @slot data A \code{'data.frame'} of species available in the database
#' @slot taxa_selected Contains the code \code{'tax_code'} of the taxa selected by choice_c() method
#' @author cedric.briand@eptb-vilaine.fr
#' @family referential objects
setClass(Class = "ref_taxa", representation = representation(data = "data.frame",taxa_selected = "character"))


#' Loading method for ref_taxa referential objects
#' 
#' @return An S4 object of class ref_taxa
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @param object An object of class \link{ref_taxa-class}
#' @return An S4 object of class \link{ref_taxa-class} with all taxa loaded from the database
#' @examples \dontrun{
#'  object=new('ref_taxa')
#'  charge(object)}
setMethod("charge", signature = signature("ref_taxa"), definition = function(object) {
    req = new("RequeteDB")
    req@sql = "SELECT tax_code, tax_nom_latin, tax_nom_commun, tax_ntx_code, tax_tax_code FROM ref.tr_taxon_tax  ORDER BY tax_rang ASC ;"
    req <- stacomirtools::query(req) 
    object@data <- req@query
    return(object)
})

#' Loading method for ref_taxa referential objects searching only taxa existing for a DC
#' @param object An object of class \link{ref_taxa-class}
#' @param dc_selected A counting device selected, only taxa attached to this dc are selected
#' @return An S4 object of class \link{ref_taxa-class} with all taxa present on a DC (counting device)
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples \dontrun{
#'  dc_selected=6
#'  object=new('ref_taxa')
#'  charge_with_filter(object,dc_selected=dc_selected)}
setMethod("charge_with_filter", signature = signature("ref_taxa"), definition = function(object,
    dc_selected) {
    requete = new("RequeteDBwhere")
    requete@select = paste("SELECT DISTINCT ON (tax_rang) tax_code, tax_nom_latin, tax_nom_commun, tax_ntx_code, tax_tax_code",
        " FROM ", get_schema(), "tg_dispositif_dis", " JOIN ",
        get_schema(), "t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
        " JOIN ", get_schema(), "t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
        " JOIN ", get_schema(), "t_lot_lot on lot_ope_identifiant=ope_identifiant",
        " JOIN ref.tr_taxon_tax on lot_tax_code=tax_code", sep = "")
    requete@where = paste("where dis_identifiant in", vector_to_listsql(dc_selected))
    requete@order_by = "ORDER BY tax_rang ASC"
    requete <- stacomirtools::query(requete)
    object@data <- requete@query
    return(object)
})


#' choice_c method for ref_taxa
#' 
#' the choice_cc method is intended to have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line. The values passed to the choice_c method
#' for taxa can be either numeric (2038 = Anguilla anguilla) or character.  
#' @param object An object from the class ref_taxa
#' @param taxa The vector of taxa, can be either code (numeric) or latin name
#' @return An S4 object of class \link{ref_taxa-class} with data filtered according to the taxa
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples
#' \dontrun{
#' object=new('ref_taxa')
#' object<-charge(object)
#' objectreport=new('report_mig_mult')
#' choice_c(object=object,'Anguilla anguilla')
#' }
setMethod("choice_c", signature = signature("ref_taxa"), definition = function(object,
    taxa) {
    if (is.null(taxa)) {
        funout(gettext("No value for argument taxa\n", domain = "R-stacomiR"), arret = TRUE)
    } else if (inherits(taxa, "character") & suppressWarnings(all(is.na(as.numeric(taxa))))) {
			# taxa is 'Anguilla anguilla'
        libellemanquants <- taxa[!taxa %in% object@data$tax_nom_latin]
        if (length(libellemanquants) > 0)
            warning(gettextf("Taxa not present :\n %s", stringr::str_c(libellemanquants,
                collapse = ", "), domain = "R-stacomiR"))
        object@taxa_selected <- object@data[object@data$tax_nom_latin %in% taxa,"tax_code"]
    } else if (inherits(taxa, "numeric")){
			# taxa is  2038
        codemanquants <- taxa[!as.character(taxa) %in% object@data$tax_code]
        if (length(codemanquants) > 0)
            warning(gettextf("Taxa not present :\n %s", stringr::str_c(codemanquants,
                collapse = ", "), domain = "R-stacomiR"))
        object@taxa_selected <- object@data[object@data$tax_code %in% as.character(taxa),"tax_code"]
    } else if (inherits(taxa, "character") & !suppressWarnings(all(is.na(as.numeric(taxa))))){
			# taxa is "2038"
			codemanquants <- taxa[!taxa %in% object@data$tax_code]
			if (length(codemanquants) > 0)
				warning(gettextf("Taxa not present :\n %s", stringr::str_c(codemanquants,
										collapse = ", "), domain = "R-stacomiR"))
			object@taxa_selected <- object@data[object@data$tax_code %in% taxa, "tax_code"]		
		}
    if (nrow(object@data) == 0) {
        funout(gettext("Stop there is no line in the taxa table (problem with the DB link ?)\n",
            domain = "R-stacomiR"), arret = TRUE)
    }
    assign("ref_taxa", object, envir = envir_stacomi)
    return(object)
})
