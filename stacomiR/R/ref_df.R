#' Class 'ref_df'
#' 
#' Representation of a fishway, contains description data of all fishways from
#' the database along with the selected fishways (df) (integer)
#' Objects from the Class: Objects can be created by calls of the form
#' \code{new('ref_df', df_selected=integer(), ouvrage=integer(),
#' data=data.frame())}.  
#' @param df_selected Object of class \code{'integer'} The identifier of the fishway
#' @param ouvrage Object of class \code{'integer'} The attached dam
#' @param data Object of class \code{'data.frame'} Data concerning the fishway
#' @author cedric.briand@eptb-vilaine.fr
#' @family referential objects
setClass(Class = "ref_df", representation = representation(df_selected = "integer",
				ouvrage = "integer", data = "data.frame"))

setValidity("ref_df", method = function(object) {
			if (length(object@df_selected) != 0) {
				if (nrow(object@data) > 0) {
					concord <- object@df_selected %in% object@data$df
					if (any(!concord)) {
						return(paste("No data for DF", object@df_selected[!concord]))
						
					} else {
						return(TRUE)
					}
				} else {
					return("You tried to set a value for df_selected without initializing the data slot")
				}
			} else return(TRUE)
			
		})
#' Loading method for DF referential objects
#' @param object An object of class \link{ref_df-class}
#' @return An object of class ref_df with df loaded
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new('ref_df')
#' charge(object)
#' }
setMethod("charge", signature = signature("ref_df"), definition = function(object) {
			requete = new("RequeteDB")
			requete@sql = paste("select dis_identifiant as DF,", " dis_date_creation,", " dis_date_suppression,",
					" dis_commentaires,", " dif_ouv_identifiant,", " ouv_libelle,", " dif_code as DF_code,",
					" dif_localisation,", " dif_orientation,", " tdf_libelle as type_DF", " from ",
					get_schema(), "tg_dispositif_dis", " JOIN ", get_schema(), "t_dispositiffranchissement_dif ON dif_dis_identifiant=dis_identifiant",
					" JOIN ", get_schema(), "tj_dfesttype_dft ON dif_dis_identifiant=dft_df_identifiant",
					" JOIN ", get_schema(), "t_ouvrage_ouv on dif_ouv_identifiant=ouv_identifiant",
					" JOIN ref.tr_typedf_tdf ON tdf_code=dft_tdf_code", " ORDER BY dis_identifiant;",
					sep = "")
			requete <- stacomirtools::query(requete)
			object@data <- requete@query
			return(object)
		})


#' Command line interface to choose a fishway
#' 
#' the choice_c method is intended to have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line.  The parameters for dF are transformed to integer as the ref_df only 
#' takes integer in the df slots. 
#' DF are third in hierarchy in the stacomi database Station>ouvrage>DF>DC>operation. This class is only used in the
#' report_df class.
#' @param object an object of class \link{ref_df-class}
#' @param df a character vector of df chosen
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @return An object of class ref_df with df selected
#' @examples
#' \dontrun{
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object=new('ref_df')
#' object<-charge(object)
#' objectreport=new('report_mig_mult')
#' choice_c(object=object,objectreport=objectreport,dc=1)
#' }
setMethod("choice_c", signature = signature("ref_df"), definition = function(object,
				df) {
			# object<-ref_df
			if (inherits(df, "numeric")) {
				df <- as.integer(df)
			} else if (inherits(df, "character")) {
				
				suppressWarnings(expr = {df <- as.integer(as.numeric(df))})

			}
			if (any(is.na(df)))
				stop("NA values df")
			object@df_selected <- df
			object@ouvrage = object@data$dif_ouv_identifiant[object@data$df %in% object@df_selected]
			validObject(object)
			# the method validObject verifies that the df is in the data slot of
			# ref_df  
			
			assign("ref_df", object, envir = envir_stacomi)
			return(object)
		})
