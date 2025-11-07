# Name : ref_coe(classe)

#' Class 'ref_coe'
#' 
#' Enables to load conversion coefficients quantity-number. This class only exists to load
#' the data with its method charge. It is not used directly as component of the graphical interface,
#' as the year is already loaded in the different report objects
#' 
#' 
#' @note Class loading coefficient of conversion between quantity (weights or
#' volumes of glass eel) and numbers between a starting and finishing date
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new('ref_coe')}.
#' @slot data A \code{data.frame}
#' @slot datedebut A 'POSIXlt'
#' @slot datefin A 'POSIXlt'
#' @author cedric.briand@eptb-vilaine.fr
#' @family referential objects
#' @keywords classes
setClass(Class = "ref_coe", representation = representation(data = "data.frame",
				datedebut = "POSIXlt", datefin = "POSIXlt"), prototype = prototype(data = data.frame()))

#' loads the coefficients for the period defined in class
#' 
#' 
#' The slots datedebut and datefin have to be filled before using charge
#' @param object An object of class \link{ref_coe-class}
#' @return An object of class \link{ref_coe-class} 
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#' object<- new('ref_coe')
#' object@datedebut<-strptime('01/01/1996',format='%d/%m/%Y')
#' object@datefin<-strptime('01/01/1997',format='%d/%m/%Y')
#' charge(object) 
#' }
setMethod("charge", signature = signature("ref_coe"), definition = function(object) {
			requete = new("RequeteDBwheredate")
			requete@datedebut = object@datedebut
			requete@datefin = object@datefin
			requete@colonnedebut = "coe_date_debut"
			requete@colonnefin = "coe_date_fin"
			requete@datefin = as.POSIXlt(object@datefin)
			requete@select = stringr::str_c("select * from ", get_schema(), "tj_coefficientconversion_coe")
			requete@and = " and  coe_tax_code='2038' and coe_std_code='CIV' and coe_qte_code='1'"
			requete <- query(requete)
			object@data <- requete@query
			return(object)
		})


#' supprime method for 'ref_coe' class
#' @param object An object of class \link{ref_coe-class}
#' @param tax '2038=Anguilla anguilla'
#' @param std 'CIV=civelle'
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return Nothing, called for side effect
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @export
setMethod("supprime", signature = signature("ref_coe"), definition = function(object,
				tax, std, silent = FALSE) {
			# object<-r_gew@coe;tax=2038;std='CIV' getting the data to import
			
			# here I assume that dc_selected will be unique (no report with several
			# dc)
			req = new("RequeteDB")
			req@sql <- stringr::str_c(
					"WITH deleted AS (", 
					"DELETE FROM ", get_schema(), "tj_coefficientconversion_coe ",
					"WHERE coe_date_debut >= '",object@datedebut,"'",
					" AND coe_date_debut  <= '", object@datefin, "'",
					" AND  coe_tax_code='", tax, "' and coe_std_code='", std,
					"' and coe_qte_code='1'",
					" RETURNING *)",
					" SELECT * FROM deleted"
			)
			del <- stacomirtools::getquery(query(req))
			nr <- nrow(del)
			if (!silent)
				funout(gettextf("%s rows deleted from table tj_coefficientconversion_coe",
								nr, domain = "R-stacomiR"))
			return(invisible(NULL))
		})

