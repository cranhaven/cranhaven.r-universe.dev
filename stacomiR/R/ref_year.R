#' Validity check for ref_year
#'
#' validity_year tests for validity within the class
#'
#'
#' @param object An object of class \code{\linkS4class{ref_year}}
#' @return boolean The test for the object refannee
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @keywords internal
validity_year = function(object)
{
	rep1 = inherits(object@data, "data.frame")
	rep2 = inherits(object@year_selected, "numeric")
	
	return(ifelse(rep1 & rep2, TRUE, FALSE))
}
#definition de la classe

#' Year reference class
#'
#' Class used to select one or several years
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_year", data=data.frame(), year_selected=numeric())}.
#' @include create_generic.R
#' @slot data A \code{data.frame} with the list of possible years selected as numerics
#' @slot year_selected A numeric vector
#' @keywords classes
#' @family referential objects
#' @author cedric.briand@eptb-vilaine.fr
setClass(
		Class = "ref_year",
		representation =
				representation(data = "data.frame", year_selected = "numeric"),
		validity = validity_year,
		prototype = prototype(data = data.frame(), year_selected = numeric())
)

#' Loading method for ref_year referential objects
#'
#' Selects year available either in the bjo table if report_object==report_mig_interannual) or in the t_operation_ope table
#' @param object An object of class \link{ref_year-class}
#' @param objectreport The object report, default \code{report_ge_weight} other possible value report_mig_interannual
#' @return object An object of class \link{ref_year-class} with slot data filled with the available years for the corresponding report
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples
#' \dontrun{
#' object=new("ref_year")
#' charge(object)
#'  validObject(annee)
#' showMethods("charge")
#' }
setMethod(
		"charge",
		signature = signature("ref_year"),
		definition = function(object, objectreport = "report_ge_weight") {
			requete = new("RequeteDB")
			if (objectreport == "report_mig_interannual") {
				if (exists("ref_dc", envir_stacomi)) {
					dc <- get("ref_dc", envir_stacomi)
					and1 <- paste(" AND bjo_dis_identifiant in", vector_to_listsql(dc@dc_selected))
				} else {
					and1 <- ""
				}
				if (exists("ref_taxa", envir_stacomi)) {
					taxa <- get("ref_taxa", envir_stacomi)
					and2 <-
							stringr::str_c(" AND bjo_tax_code ='", taxa@taxa_selected, "'")
				} else {
					and2 <- ""
				}
				if (exists("ref_stage", envir_stacomi)) {
					stage <- get("ref_stage", envir_stacomi)
					and3 <-
							stringr::str_c(" AND bjo_std_code ='", stage@stage_selected, "'")
				} else
				{
					and3 = ""
				}
				requete@sql = paste(
						"select  DISTINCT ON (bjo_annee) bjo_annee from ",
						get_schema(),
						"t_bilanmigrationjournalier_bjo where bjo_identifiant>0 ",
						# I want and statements to not have to choose the order
						# the where statement is always verified
						and1,
						and2,
						and3,
						sep = ""
				)
			} else if (objectreport == "report_ge_weight") {
				requete@sql = paste(
						"select  DISTINCT ON (year) year from( select date_part('year', ope_date_debut) as year from ",
						get_schema(),
						"t_operation_ope) as tabletemp",
						sep = ""
				)
			} else if (objectreport == "report_annual" |
					objectreport == "report_species") {
				if (exists("ref_dc", envir_stacomi)) {
					dc <- get("ref_dc", envir_stacomi)
					and1 <-
							paste(" AND ope_dic_identifiant in ",
									vector_to_listsql(dc@dc_selected))
				} else {
					and1 <- ""
				}
				if (exists("ref_taxa", envir_stacomi)) {
					taxa <- get("ref_taxa", envir_stacomi)
					if (!length(taxa@taxa_selected)==0){
						and2 <-
								stringr::str_c(" AND lot_tax_code in ",
										vector_to_listsql(taxa@taxa_selected))
					} else {
						and2 <- ""
					}
				} else {
					and2 <- ""
				}
				if (exists("ref_stage", envir_stacomi)) {
					stage <- get("ref_stage", envir_stacomi)
					if (!length(stage@stage_selected)==0){
						and3 <-
								stringr::str_c(" AND lot_std_code in ",
										vector_to_listsql(stage@stage_selected))
					} else
					{
						and3 = ""
					}
				} else
				{
					and3 = ""
				}
				requete@sql = paste(
						"select  DISTINCT ON (year) year from (select date_part('year', ope_date_debut) as year from ",
						get_schema(),
						"t_operation_ope JOIN ",
						get_schema(),
						"t_lot_lot on lot_ope_identifiant=ope_identifiant",
						" WHERE lot_lot_identifiant is null",
						and1,
						and2,
						and3,
						") as tabletemp",
						sep = ""
				)
			} else {
				funout(gettextf("Not implemented for objectreport = %s", objectreport),
						arret = TRUE)
			}
			requete <-
					stacomirtools::query(requete)  
			object@data <- stacomirtools::getquery(requete)
			return(object)
		}
)


#' choice_c method for ref_year referential from the command line
#'
#' The choice_c method will issue a warning if the year is not present in the database
#' Allows the selection of year and the assignment in environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @param object An object of class \link{ref_year-class}
#' @param annee The year to select, either as a character or as a numeric
#' @param nomassign The name to be assigned in envir_stacomi
#' @param funoutlabel The label that appears in funout
#' @param silent Stops messages from being displayed if silent=TRUE, default FALSE
#' @return object An object of class \link{ref_year-class} with year selected
#' @examples
#' \dontrun{
#' object=new("ref_year")
#' object<-charge(object)
#' win=gwindow(title="test ref_year")
#' group=ggroup(container=win,horizontal=FALSE)
#' choice(object,nomassign="ref_year",funoutlabel="essai",titleFrame="essai ref_year",preselect=1)
#' dispose(win)
#' }
setMethod(
		"choice_c",
		signature = signature("ref_year"),
		definition = function(object,
				annee,
				nomassign = "ref_year",
				funoutlabel = gettext("Year selected\n", domain = "R-stacomiR"),
				silent = FALSE) {
			if (length(annee) > 1)
				stop("horodate should be a vector of length 1")
			if (inherits (annee, "character"))
				annee <- as.numeric(annee)
			# the charge method must be performed before
			gettext("no year", domain = "R-stacomiR")
			if (!annee %in% object@data[, 1]) {
				warning(
						stringr::str_c(
								"Attention, year ",
								annee,
								" is not available in the database, available years :",
								ifelse(
										length(object@data$bjo_annee) == 0,
										gettext(" none", domain = "R-stacomiR"),
										stringr::str_c(object@data$bjo_annee, collapse = ",")
								)
						)
				)
			}
			object@year_selected <- annee
			
			assign(nomassign, object, envir_stacomi)
			if (!silent)
				funout(funoutlabel)
			return(object)
		}
)
