#' Class "report_sea_age"
#'
#' the report_sea_age class is used to dispatch adult salmons to age class according to
#' their size and to basin dependent limits set by the user. Once checked with graphs and summary
#' statistics, the results are to be written to the database.
#' @include create_generic.R
#' @include ref_textbox.R
#' @include ref_dc.R
#' @include ref_taxa.R
#' @include ref_stage.R
#' @include ref_horodate.R
#' @include ref_par.R
#' @note This class is displayed by interface_report_sea_age
#' @slot data A data frame with data generated from the database
#' @slot calcdata A list of dc with processed data. This lists consists of two elements
#' \itemize{
#' \item (1) data A dataset with age set to be used by the plot and summary methods
#' \item (2) tj_caracteristitiquelot_car A dataset to import into the database
#' }
#' @slot dc Object of class \link{ref_dc-class}: the control devices
#' @slot taxa Object of class \link{ref_taxa-class}: the species
#' @slot stage Object of class \link{ref_stage-class} : the stages of the fish
#' @slot par Object of class \link{ref_par-class}: the parameters used
#' @slot horodatedebut An object of class \code{ref_horodate-class}
#' @slot horodatefin An object of class \code{ref_horodate-class}
#' @slot limit1hm The size limit, in mm between 1 sea winter fishes and 2 sea winter fishes
#' @slot limit2hm The size limit, in mm between 2 sea winter fishes and 3 sea winter fishes
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("report_sea_age", ...)}
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @family report Objects
#' @keywords classes
#' @example inst/examples/report_sea_age-example.R
#' @aliases report_sea_age
#' @export
setClass(
		Class = "report_sea_age",
		representation = representation(
				data = "data.frame",
				calcdata = "list",
				dc = "ref_dc",
				taxa = "ref_taxa",
				stage = "ref_stage",
				par = "ref_par",
				horodatedebut = "ref_horodate",
				horodatefin = "ref_horodate",
				limit1hm = "ref_textbox",
				limit2hm = "ref_textbox"
		),
		prototype = prototype(
				data = data.frame(),
				calcdata = list(),
				dc = new("ref_dc"),
				taxa = new("ref_taxa"),
				stage = new("ref_stage"),
				par = new("ref_par"),
				horodatedebut = new("ref_horodate"),
				horodatefin = new("ref_horodate"),
				limit1hm = new("ref_textbox"),
				limit2hm = new("ref_textbox")
		)
)
setValidity("report_sea_age", function(object)
		{
			rep1 = object@taxa@taxa_selected[1] == '2220'
			label1 <-
					'report_sea_age should only be for salmon (tax_code=2220)'
			rep2 = all(object@stage@stage_selected %in% c('5', '11', 'BEC', 'BER', 'IND'))
			label2 <-
					'Only stages 5,11,BEC,BER,IND should be used in report_sea_age'
			return(ifelse(rep1 &
									rep2 , TRUE , c(label1, label2)[!c(rep1, rep2)]))
		})
#' connect method for report_sea_age
#'
#' @param object An object of class \link{report_sea_age-class}
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return An object of class \link{report_sea_age-class} with slot data \code{@data} filled
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases connect.report_sea_age
setMethod(
		"connect",
		signature = signature("report_sea_age"),
		definition = function(object, silent = FALSE) {
			requete <- new("RequeteDBwheredate")
			requete@select = paste("SELECT * FROM ",
					get_schema(),
					"vue_lot_ope_car",
					sep = "")
			requete@colonnedebut = "ope_date_debut"
			requete@colonnefin = "ope_date_fin"
			requete@datedebut <- object@horodatedebut@horodate
			requete@datefin <- object@horodatefin@horodate
			requete@order_by = "ORDER BY ope_date_debut"
			requete@and = paste(
					" AND ope_dic_identifiant in ",
					vector_to_listsql(object@dc@dc_selected),
					" AND lot_tax_code in ",
					vector_to_listsql(object@taxa@taxa_selected),
					" AND lot_std_code in ",
					vector_to_listsql(object@stage@stage_selected),
					" AND car_par_code in ",
					vector_to_listsql(object@par@par_selected),
					sep = ""
			)
			requete <- stacomirtools::query(requete)
			object@data <- requete@query
			if (!silent)
				funout(gettext("Data loaded", domain = "R-stacomiR"))
			return(object)
		}
)


#' Loads data and check that all choices in the graphical interface have been made.
#'
#' It is not necessary to run this method if the choice_c method has been run.
#' This method verifies that boxes have been clicked in the user interface and gets the objects pasted in
#' envir_stacomi
#' @param object An object of class \link{report_sea_age-class}
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class \link{report_sea_age-class} with slots filled with user choice
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @return An object of class \link{report_sea_age-class} with slots filled from values assigned in \code{envir_stacomi} environment
#' @aliases charge.report_sea_age
#' @keywords internal
setMethod(
		"charge",
		signature = signature("report_sea_age"),
		definition = function(object, silent=FALSE) {
			if (exists("ref_dc", envir_stacomi)) {
				object@dc <- get("ref_dc", envir_stacomi)
			} else {
				funout(
						gettext(
								"You need to choose a counting device, clic on validate\n",
								domain = "R-stacomiR"
						),
						arret = TRUE
				)
			}
			if (exists("ref_taxa", envir_stacomi)) {
				object@taxa <- get("ref_taxa", envir_stacomi)
			} else {
				funout(
						gettext("You need to choose a taxa, clic on validate\n", domain = "R-stacomiR"),
						arret = TRUE
				)
			}
			if (exists("ref_stage", envir_stacomi)) {
				object@stage <- get("ref_stage", envir_stacomi)
			} else {
				funout(
						gettext("You need to choose a stage, clic on validate\n", domain = "R-stacomiR"),
						arret = TRUE
				)
			}
			if (exists("ref_par", envir_stacomi)) {
				object@par <- get("ref_par", envir_stacomi)
			} else {
				funout(
						gettext("You need to choose a parameter, clic on validate\n", domain = "R-stacomiR"),
						arret = TRUE
				)
			}
			# rem pas tres satisfaisant car ce nom est choisi dans l'interface
			if (exists("r_seaa_date_debut", envir_stacomi)) {
				object@horodatedebut@horodate <-
						get("r_seaa_date_debut", envir_stacomi)
			} else {
				funout(gettext("You need to choose the starting date\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			# rem id
			if (exists("r_seaa_date_fin", envir_stacomi)) {
				object@horodatefin@horodate <- get("r_seaa_date_fin", envir_stacomi)
			} else {
				funout(gettext("You need to choose the ending date\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			if (exists("limit1hm", envir_stacomi)) {
				object@limit1hm <- get("limit1hm", envir_stacomi)
			} else {
				funout(gettext("you need to choose a value for limit1hm", domain = "R-stacomiR"),
						arret = TRUE)
			}
			if (exists("limit2hm", envir_stacomi)) {
				object@limit2hm <- get("limit2hm", envir_stacomi)
			} else {
				funout(gettext("you need to choose a value for limit2hm", domain = "R-stacomiR"),
						arret = TRUE)
			}
			return(object)
			validObject(object)
      if (!silent)
        funout(
            gettext(
                "Writing report_sea_age in the environment envir_stacomi : write r_seaa=get('r_seaa',envir_stacomi) ",
                domain = "R-stacomiR"
            )
        )
			assign("r_seaa", object, envir_stacomi)
		}
)


#' command line interface for report_sea_age class
#' 
#' #' The choice_c method fills in the data slot for classes \link{ref_dc-class}, \link{ref_taxa-class}, \link{ref_stage-class}, \link{ref_par-class} and two slots of \link{ref_horodate-class} and then
#' uses the choice_c methods of these object to select the data.
#' @param object An object of class \link{report_sea_age-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,ref_dc-method}
#' @param taxa '2220=Salmo salar',
#' these should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,ref_taxa-method}
#' @param stage '5','11','BEC','BER','IND'
#' @param par Parameters chosen for the report are measured body size (1786), measured fork length (1785),video size (C001) and number of year at sea (A124)
#' @param horodatedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param horodatefin The finishing date of the report, for this class this will be used to calculate the number of daily steps.
#' @param limit1hm Size limit of a salmon for an one sea winter fish
#' @param limit2hm Size limit of a salmon for a two sea winter fish
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return An object of class \link{report_sea_age-class}
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases choice_c.report_sea_age
setMethod(
		"choice_c",
		signature = signature("report_sea_age"),
		definition = function(object,
				dc,
				taxa = 2220,
				stage = c('5', '11', 'BEC', 'BER', 'IND'),
				par = c('1786', '1785', 'C001', 'A124'),
				horodatedebut,
				horodatefin,
				limit1hm,
				limit2hm,
				silent = FALSE) {
			# code for debug using example
			#horodatedebut="2012-01-01";horodatefin="2013-12-31";dc=c(107,108,101);
			#taxa=2220;	stage=c('5','11','BEC','BER','IND');par=c('1786','1785','C001');silent=FALSE
			if (!(is.numeric(limit1hm) |
						is.integer(limit1hm)))
				funout(gettext("limit1hm should be numeric or integer", domain = "R-stacomiR"),
						arret = TRUE)
			if (!(is.numeric(limit2hm) |
						is.integer(limit2hm)))
				funout(gettext("limit2hm should be numeric or integer", domain = "R-stacomiR"),
						arret = TRUE)
			
			r_seaa <- object
			r_seaa@dc = charge(r_seaa@dc)
			# loads and verifies the dc
			# this will set dc_selected slot
			r_seaa@dc <- choice_c(object = r_seaa@dc, dc)
			# only taxa present in the report_mig are used
			r_seaa@taxa <-
					charge_with_filter(object = r_seaa@taxa, r_seaa@dc@dc_selected)
			r_seaa@taxa <- choice_c(r_seaa@taxa, taxa)
			r_seaa@stage <-
					charge_with_filter(object = r_seaa@stage,
							r_seaa@dc@dc_selected,
							r_seaa@taxa@taxa_selected)
			r_seaa@stage <- choice_c(r_seaa@stage, stage, silent = silent)
			r_seaa@par <-
					charge_with_filter(
							object = r_seaa@par,
							r_seaa@dc@dc_selected,
							r_seaa@taxa@taxa_selected,
							r_seaa@stage@stage_selected
					)
			r_seaa@par <- choice_c(r_seaa@par, par, silent = silent)
			r_seaa@horodatedebut <- choice_c(
					object = r_seaa@horodatedebut,
					nomassign = "r_seaa_date_debut",
					funoutlabel = gettext("Beginning date has been chosen\n", domain = "R-stacomiR"),
					horodate = horodatedebut,
					silent = silent
			)
			r_seaa@horodatefin <- choice_c(
					r_seaa@horodatefin,
					nomassign = "r_seaa_date_fin",
					funoutlabel = gettext("Ending date has been chosen\n", domain = "R-stacomiR"),
					horodate = horodatefin,
					silent = silent
			)
			r_seaa@limit1hm <-
					choice_c(r_seaa@limit1hm, as.character(limit1hm), "limit1hm")
			r_seaa@limit2hm <-
					choice_c(r_seaa@limit2hm, as.character(limit2hm), "limit2hm")
			validObject(r_seaa)
			return(r_seaa)
		}
)

#' Split data according to the limits
#' set in the limit1hm, and limit2hm arguments of the \link{report_sea_age-class}.
#'
#' If no value are provided in the limit1hm slot, an error is returned, if
#' no value is provided in the limit2hm slot a default upper value for salmon
#' size is taken to ensure all salmon are either of age 1 or 2, but no age 3 are
#' returned
#' @param object An object of class \code{\link{report_sea_age-class}}
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @return An object of class \link{report_sea_age-class} with calculated data in slot calcdata
#' @aliases calcule.report_sea_age
setMethod(
		"calcule",
		signature = signature("report_sea_age"),
		definition = function(object, silent) {
			#r_seaa<-r_sample_char
			r_seaa <- object
			if (nrow(r_seaa@data) == 0) {
				funout(
						gettext("you have no line in the database for this period", domain = "R-stacomiR"),
						arret = TRUE
				)
			}
			adm = r_seaa@data # we get the data.frame
			# the age already present in the database don't interest us there
			adm = adm[adm$car_par_code != 'A124', ]
			if (is.na(as.numeric(r_seaa@limit1hm@label)))
				stop("internal error")
			# if no value, a dummy value of 2m
			if (is.na(as.numeric(r_seaa@limit2hm@label)))
				r_seaa@limit2hm@label <- 2000
			lescoupes <-
					c(0,
							as.numeric(r_seaa@limit1hm@label),
							as.numeric(r_seaa@limit2hm@label),
							2001)
			adm$age <-
					cut(
							x = adm$car_valeur_quantitatif,
							breaks = lescoupes,
							labels = FALSE
					)
			r_seaa@calcdata[["data"]] <- adm
			assign("r_seaa", r_seaa, envir_stacomi)
			return(r_seaa)
		}
)


#' Plots of various type for report_sea_age
#'
#' @param x An object of class \link{report_sea_age-class}
#' @param plot.type Default "1"
#'  \describe{
#' 		\item{plot.type="1"}{density plot by sea age}
#' 		\item{plot.type="2"}{Density plot by sea age and dc}
#' }
#' @param silent Default FALSE, if TRUE the program should no display messages.
#' @return Nothing, called for its side effect of plotting
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases plot.report_sea_age
#' @export
setMethod(
		"plot",
		signature(x = "report_sea_age", y = "missing"),
		definition = function(x,
				plot.type = "1",
				silent = FALSE) {
			#require(ggplot2);plot.type="1"
			#browser()
			r_seaa <- x
			plot.type <- as.character(plot.type)# to pass also characters
			if (!plot.type %in% c("1", "2"))
				stop('plot.type must be 1,2')
			if (nrow(r_seaa@calcdata[["data"]]) == 0) {
				if (!silent)
					funout(
							gettext("You need to launch computation first, clic on calc\n", domain =
											"R-stacomiR"),
							arret = TRUE
					)
			}
			dat <- r_seaa@calcdata[["data"]]
			# cols are using viridis::inferno(6,alpha=0.9)
			les_coupes = as.numeric(c(r_seaa@limit1hm@label, r_seaa@limit2hm@label))
			
			
			#################################################
			# plot.type =1 density plot
			#################################################
			
			if (plot.type == "1") {
				p <-
						ggplot(dat) + geom_histogram(
								aes(x = car_valeur_quantitatif, fill = factor(age)),
								binwidth = 10,
								alpha = 0.8
						) +
						geom_vline(xintercept = les_coupes,
								lty = 2,
								lwd = 1) +
						annotate(
								"text",
								x = les_coupes,
								y = 0,
								label = les_coupes,
								vjust = 1,
								hjust = -0.2
						) +
						theme_minimal() +
						scale_fill_manual("Age",
								values = c("1" = "#379ec6", "2" = "#173957", "3" = "#b09953")) +
						xlab("Size in mm") +
						ylab("Effectif")
				print(p)
				assign("p", p, envir = envir_stacomi)
				if (!silent){
					funout(
							gettext(
									"The graphical object is written is env_stacomi, type p<-get('p',envir=envir_stacomi)",
									domain = "R-stacomiR"
							)
					)}
				
			}
			######################################
			# Migration according to stage, month and year
			######################################
			# todo see of anotation is possible
			if (plot.type == "2") {
				p <-
						ggplot(dat) + geom_histogram(
								aes(x = car_valeur_quantitatif, fill = factor(age)),
								binwidth = 10,
								alpha = 0.8
						) +
						geom_vline(xintercept = les_coupes,
								lty = 2,
								lwd = 1) +
						theme_minimal() +
						scale_fill_manual("Age",
								values = c("1" = "#379ec6", "2" = "#173957", "3" = "#b09953")) +
						xlab("Size in mm") +
						ylab("Effectif") +
						facet_grid(ope_dic_identifiant ~ .)
				print(p)
				assign("p", p, envir = envir_stacomi)
				if (!silent){
					funout(
							gettext(
									"The graphical object is written is env_stacomi, type p<-get('p',envir=envir_stacomi)",
									domain = "R-stacomiR"
							)
					)
				}
			}
			return(invisible(NULL))
		}
)

#' summary for report_sea_age
#' @param object An object of class \link{report_sea_age-class}
#' @param silent Default FALSE, if TRUE the program should no display messages.
#' @param ... Additional parameters
#' @return The summary
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases summary.report_sea_age
#' @export
setMethod(
		"summary",
		signature = signature(object = "report_sea_age"),
		definition = function(object, silent = FALSE, ...) {
			r_seaa <- object
			dat <- r_seaa@calcdata[["data"]]
			if (nrow(dat) == 0) {
				if (!silent)
					funout(
							gettext("You need to launch computation first, clic on calc\n", domain =
											"R-stacomiR"),
							arret = TRUE
					)
			}
			ndc = unique(dat$ope_dic_identifiant)
			result <- list()
			for (i in 1:length(ndc)) {
				datdc <-	dat[dat$ope_dic_identifiant == ndc[i], ]
				dc_code <- r_seaa@dc@data$dc_code[r_seaa@dc@data$dc == ndc[i]]
				ouvrage <-
						gsub("[\r\n]", "", r_seaa@dc@data[r_seaa@dc@data$dc == r_seaa@dc@dc_selected[i], "ouv_libelle"])
				dc <- as.character(unique(datdc$ope_dic_identifiant))
				result[[dc]] <- list()
				result[[dc]][["ouvrage"]] <- ouvrage
				print(noquote(
								stringr::str_c("Age Statistics for dam : ", ouvrage, " CD=", dc_code)
						))
				print(noquote("========================"))
				print(table(datdc$age))
				result[[dc]][["age"]] <- table(datdc$age)
				
			}
			if (length(ndc) > 1) {
				print(noquote(stringr::str_c("Age Statistics total")))
				print(noquote("========================"))
				print(table(dat$age))
				
			}
			return(result)
		}
)

#' Command line method to write the characteristic "sea age" (car_par_code='A124')
#' into the tj_caracteristiquelot_car table in the user's scheme
#'
#' The sea age characteristic is calculated from the measured or calculated size of salmon and with a size/age rule
#' defined by the user. 
#' @param object an object of class \link{report_sea_age-class}
#' @param silent : Default FALSE, if TRUE the program should no display messages.
#' @return Nothing, called for its side effect of writing data to the database
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases write_database.report_sea_age
#' @export
setMethod(
		"write_database",
		signature = signature("report_sea_age"),
		definition = function(object, silent = TRUE) {
			# dbname="bd_contmig_nat"
			r_seaa <- object
			calcdata <- r_seaa@calcdata[["data"]]
			data_in_base <- r_seaa@data
			if (nrow(calcdata) == 0) {
				if (!silent)
					funout(
							gettext("You need to launch computation first, clic on calc\n", domain =
											"R-stacomiR"),
							arret = TRUE
					)
			}
			if (!inherits(r_seaa, "report_sea_age"))
				stop("the r_seaa should be of class report_sea_age")
			if (!inherits(silent, "logical"))
				stop("the silent argument should be a logical")
			data_in_base <- data_in_base[data_in_base$car_par_code == 'A124', ]
			if (nrow(data_in_base) > 0) {
				supprime(r_seaa, silent = silent)
			}
			#--------------
			# creating the table to import
			#--------------
			code_parametre_age = 'A124'
			code_methode_obtention = "CALCULE"
			comment = gettextf(
					"Age calculated from the size of fish compared to reference value %s for the limit between 1 sea winter and 2 sea winter fish, and %s for the limit between 2 sea winter fish and 3 sea winter fish",
					r_seaa@limit1hm@label,
					r_seaa@limit2hm@label
			)
			bam = data.frame(
					r_seaa@calcdata$data$lot_identifiant,
					code_parametre_age,
					code_methode_obtention,
					as.integer(NA),
					r_seaa@calcdata$data$age,
					as.integer(NA),
					comment,
					get_org()
			)
			#--------------
			# writing the table in the database
			#--------------
			con <- new("ConnectionDB")
			con <- connect(con)
			on.exit(pool::poolClose(con@connection))
			pool::dbWriteTable(con@connection, 
					name = "bam", 
					value=bam, 
					temporary=TRUE)	
			
			sql <-
					stringr::str_c(
							"INSERT INTO ",
							get_schema(),
							"tj_caracteristiquelot_car 	SELECT * FROM  bam;"
					)
			
			pool::dbExecute(con@connection, statement = sql)
			
			if (!silent) {
				funout(gettextf("Writing  %s age values in the database \n", nrow(bam)))
			}
			return(invisible(NULL))
		}
)

#' Method to print the command line of the object
#' @param x An object of class report_sea_age
#' @param ... Additional parameters passed to print
#' @return NULL
#' @author cedric.briand
#' @aliases print.report_sea_age
#' @export
setMethod(
		"print",
		signature = signature("report_sea_age"),
		definition = function(x, ...) {
			sortie1 <- "r_seaa=new('report_sea_age')"
			sortie2 <- stringr::str_c(
					"r_seaa=choice_c(r_seaa,",
					"dc=c(",
					stringr::str_c(x@dc@dc_selected, collapse = ","),
					"),",
					"taxa=c(",
					stringr::str_c(shQuote(x@taxa@data$tax_nom_latin), collapse = ","),
					"),",
					"stage=c(",
					stringr::str_c(shQuote(x@stage@stage_selected), collapse = ","),
					"),",
					"par=c(",
					stringr::str_c(shQuote(x@par@par_selected), collapse = ","),
					"),",
					"horodatedebut=",
					shQuote(
							strftime(x@horodatedebut@horodate, format = "%d/%m/%Y %H-%M-%S")
					),
					",horodatefin=",
					shQuote(
							strftime(x@horodatefin@horodate, format = "%d/%m/%Y %H-%M-%S")
					),
					")"
			)
			# removing backslashes
			funout(sortie1)
			funout(stringr::str_c(sortie2, ...))
			return(invisible(NULL))
		}
)




#' supprime method for report_mig_interannual class
#' @param object An object of class \link{report_sea_age-class}
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return Nothing, called for its side effect of deleting data in the database
#' @aliases supprime.report_sea_age
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @export
setMethod(
		"supprime",
		signature = signature("report_sea_age"),
		definition = function(object, silent = FALSE)
		{
			r_seaa <- object
			data_in_base <- r_seaa@data
			data_in_base <- data_in_base[data_in_base$car_par_code == 'A124', ]
			if (nrow(data_in_base) == 0) {
				if (!silent)
					funout(gettext("No data to remove"), arret = TRUE)
			}
			con = new("ConnectionDB")
			con <- connect(con)
			on.exit(pool::poolClose(con@connection))
			sql =  stringr::str_c("DELETE from ", 
					get_schema(),
					"tj_caracteristiquelot_car ",
					"WHERE car_lot_identifiant IN ",
					vector_to_listsql(data_in_base$lot_identifiant),
					" AND car_par_code='A124';"					
			)
			pool::dbExecute(con@connection, statement = sql)			
			return(invisible(NULL))
		}

)