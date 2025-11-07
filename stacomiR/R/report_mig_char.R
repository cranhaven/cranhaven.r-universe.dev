#' Migration report along with quantitative and
#' qualitative characteristics
#' 
#' Migration along with qualitative or quantitative characteristics or both
#' (e.g.) weight of eels according to the size class per period of time, weight
#' of fish according to gender, number of fish per age class. This class does not split migration evenly over 
#' time period. So, unlike calculations made in class report_mig and report_mig_mult
#' the whole time span of the migration operation is not considered, only  the date of beginning of 
#' the operation is used to perform calculations. 
#' 
#' @include ref_parquan.R
#' @include ref_parqual.R
#' @include ref_choice.R
#' @include report_sample_char.R
#' @note The main difference between this class and \link{report_sample_char-class} is that this class allows to
#' select (or not) the samples, and that it handles quantitative and qualitative parameters separately.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new('report_mig_char', ...)}.  they are loaded by the interface
#' using interface_report_mig_char function.
#' @slot calcdata A 'list' of calculated data, filled in by the calcule method
#' @slot data A \code{data.frame} inherited from \link{report_sample_char-class}
#' @slot dc An object of class \link{ref_dc-class} inherited from \link{report_sample_char-class}
#' @slot taxa An object of class \link{ref_taxa-class} inherited from \link{report_sample_char-class}
#' @slot stage An object of class \link{ref_stage-class} inherited from \link{report_sample_char-class}
#' @slot horodatedebut An object of class \link{ref_horodate-class} inherited from \link{report_sample_char-class}
#' @slot horodatefin An object of class \link{ref_horodate-class} inherited from \link{report_sample_char-class}
#' @slot par An object of class \link{ref_par-class} inherited from \link{report_sample_char-class}
#' @slot echantillon An object of class \link{ref_choice-class}, vector of choice
#' @slot parquan An object of class \link{ref_parquan-class}, quantitative parameter 
#' @slot parqual An object of class \link{ref_parqual-class}, qualitative parameter
#' @family report Objects
#' @aliases report_mig_char
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @concept report Object 
#' @example inst/examples/report_mig_char-example.R
#' @keywords classes
#' @export
setClass(Class = "report_mig_char", 
		representation = representation(
				echantillon = "ref_choice",
				calcdata = "list", 
				parqual = "ref_parqual", 
				parquan = "ref_parquan"), 
		prototype = list(
				data = list(),
				echantillon = new("ref_choice", listechoice = c("with", "without"),
						selectedvalue = "with"), 
				calcdata = list(),
				parqual = new("ref_parqual"),
				parquan = new("ref_parquan")),
		contains = "report_sample_char")


setValidity("report_mig_char", function(object) {
			retValue = ""
			rep4 <- length(object@taxa) == 1
			if (!rep4)
				retValue = gettext("This report should be for just one taxa")
			rep5 <- length(object@parqual) == 1 | length(object@parquan) == 1
			if (!rep5)
				retValue = gettext("length(object@parqual)==1|length(object@parquan)==1 not TRUE")
			return(ifelse(rep4 & rep5, TRUE, retValue))
		})


#' command line interface for report_mig_char class
#' @param object An object of class \link{report_mig_char-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,ref_dc-method}
#' @param taxa '2220=Salmo salar', can be a vector with several values
#' these should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,ref_taxa-method}
#' @param stage The stages selected, can be a vector with several values
#' @param parquan Quantitative parameter
#' @param parqual Qualitative parameter
#' @param horodatedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param horodatefin The finishing date of the report, for this class this will be used to calculate the number of daily steps
#' @param echantillon 'with' can be 'without', checking without modifies the query
#' in the connect method so that subsamples are not allowed 
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return An object of class \link{report_sea_age-class}
#' The choice_c method fills in the data slot for classes \link{ref_dc-class}, \link{ref_taxa-class}, \link{ref_stage-class}, \link{ref_par-class} and two slots of \link{ref_horodate-class} and then 
#' uses the choice_c methods of these object to select the data.
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases choice_c.report_mig_char
setMethod("choice_c", signature = signature("report_mig_char"), definition = function(object,
				dc, taxa, stage, parquan = NULL, parqual = NULL, horodatedebut, horodatefin,
				echantillon = c("with","without"), silent = FALSE) {
			echantillon <-  match.arg(echantillon)
			# code for debug using example
			# horodatedebut='2012-01-01';horodatefin='2013-12-31';dc=c(107,108,101);taxa=2220;stage=c('5','11','BEC','BER','IND');parquan=c('1786','1785','C001','A124');parqual='COHO';silent=FALSE
			r_mig_char <- object
			r_mig_char@dc = charge(r_mig_char@dc)
			r_mig_char@dc <- choice_c(object = r_mig_char@dc, dc)
			r_mig_char@taxa <- charge_with_filter(object = r_mig_char@taxa, r_mig_char@dc@dc_selected)
			r_mig_char@taxa <- choice_c(r_mig_char@taxa, taxa)
			r_mig_char@stage <- charge_with_filter(object = r_mig_char@stage, r_mig_char@dc@dc_selected,
					r_mig_char@taxa@taxa_selected)
			r_mig_char@stage <- choice_c(r_mig_char@stage, stage, silent = silent)
			r_mig_char@parquan <- charge_with_filter(object = r_mig_char@parquan, dc_selected = r_mig_char@dc@dc_selected,
					taxa_selected = r_mig_char@taxa@taxa_selected, stage_selected = r_mig_char@stage@stage_selected)
			if (!is.null(parquan))
				r_mig_char@parquan <- choice_c(r_mig_char@parquan, parquan, silent = silent)
			# the method choice_c is written in ref_par, and each time
			assign("ref_parquan", r_mig_char@parquan, envir_stacomi)
			r_mig_char@parqual <- charge_with_filter(object = r_mig_char@parqual, r_mig_char@dc@dc_selected,
					r_mig_char@taxa@taxa_selected, r_mig_char@stage@stage_selected)
			if (!is.null(parqual)) {
				r_mig_char@parqual <- choice_c(r_mig_char@parqual, parqual, silent = silent)
				r_mig_char@parqual <- charge_complement(r_mig_char@parqual)
			}
			assign("ref_parqual", r_mig_char@parqual, envir_stacomi)
			r_mig_char@horodatedebut <- choice_c(object = r_mig_char@horodatedebut, nomassign = "bmC_date_debut",
					funoutlabel = gettext("Beginning date has been chosen\n", domain = "R-stacomiR"),
					horodate = horodatedebut, silent = silent)
			r_mig_char@horodatefin <- choice_c(r_mig_char@horodatefin, nomassign = "bmC_date_fin",
					funoutlabel = gettext("Ending date has been chosen\n", domain = "R-stacomiR"),
					horodate = horodatefin, silent = silent)
			r_mig_char@echantillon <- charge(r_mig_char@echantillon, vecteur = c("with","without"), label = "essai",
					selected = as.integer(1))
			r_mig_char@echantillon <- choice_c(r_mig_char@echantillon, selectedvalue = echantillon)
			validObject(r_mig_char)
			return(r_mig_char)
		})

#' charge method for report_mig_char
#' 
#' Used by the graphical interface to collect and test objects in the environment envir_stacomi, 
#' fills also the data slot by the connect method. It is not necessary to run the charge method
#' if the choice is made from the command line using the choice_c method.
#' @param object An object of class \link{report_mig_char-class}
#' @param silent Default FALSE, if TRUE the program should not display messages
#' @return \link{report_mig_char-class}  with slot filled from values assigned in \code{envir_stacomi} environment
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases charge.report_mig_char
#' @keywords internal
setMethod("charge", signature = signature("report_mig_char"), definition = function(object,
				silent = FALSE) {
			r_mig_char <- object
			if (exists("bmC_date_debut", envir_stacomi)) {
				r_mig_char@horodatedebut@horodate <- get("bmC_date_debut", envir_stacomi)
			} else {
				funout(gettext("You need to choose the starting date\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			if (exists("bmC_date_fin", envir_stacomi)) {
				r_mig_char@horodatefin@horodate <- get("bmC_date_fin", envir_stacomi)
			} else {
				funout(gettext("You need to choose the ending date\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			
			if (exists("ref_dc", envir_stacomi)) {
				r_mig_char@dc <- get("ref_dc", envir_stacomi)
			} else {
				funout(gettext("You need to choose a counting device, clic on validate\n",
								domain = "R-stacomiR"), arret = TRUE)
			}
			if (exists("ref_taxa", envir_stacomi)) {
				r_mig_char@taxa <- get("ref_taxa", envir_stacomi)
			} else {
				funout(gettext("You need to choose a taxa, clic on validate\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			if (exists("ref_stage", envir_stacomi)) {
				r_mig_char@stage <- get("ref_stage", envir_stacomi)
			} else {
				funout(gettext("You need to choose a stage, clic on validate\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			
			if (exists("refchoice", envir_stacomi)) {
				r_mig_char@echantillon <- get("refchoice", envir_stacomi)
			} else {
				r_mig_char@echantillon@listechoice <- gettext("with", domain = "R-stacomiR")
				r_mig_char@echantillon@selected <- as.integer(1)
			}
			
			if (!(exists("ref_parquan", envir_stacomi) | exists("ref_parqual", envir_stacomi))) {
				funout(gettext("You need to choose at least one parameter qualitative or quantitative\n",
								domain = "R-stacomiR"), arret = TRUE)
			}
			
			if (exists("ref_parquan", envir_stacomi)) {
				r_mig_char@parquan <- get("ref_parquan", envir_stacomi)
			}
			if (exists("ref_parqual", envir_stacomi)) {
				r_mig_char@parqual <- get("ref_parqual", envir_stacomi)
			}
			
			stopifnot(validObject(r_mig_char, test = TRUE))
			return(r_mig_char)
		})

#' connect method for report_mig_char
#' 
#' 
#' uses the report_mig_mult method
#' @param object An object of class \link{report_mig_char-class}
#' @param silent Boolean default FALSE, if TRUE information messages not displayed
#' @return An object of class \link{report_mig_char-class}  with list in \code{@data$parquan} and \code{@data$parqual} filled in from the database
#' @aliases connect.report_mig_char
setMethod("connect", signature = signature("report_mig_char"), definition = function(object,
				silent = FALSE) {
			r_mig_char <- object
			if (r_mig_char@echantillon@selectedvalue == "without") {
				echantillons = " AND lot_pere IS NULL"
			} else {
				echantillons = ""
			}
			# data can be selected but not in the database or the inverse
			parquan <- intersect(r_mig_char@parquan@par_selected, r_mig_char@parquan@data$par_code)
			parqual <- intersect(r_mig_char@parqual@par_selected, r_mig_char@parqual@data$par_code)
			if (length(parquan) == 0 & length(parqual) == 0) {
				stop("You need to choose at least one quantitative or qualitative attribute")
			} else {
				if (length(parqual) != 0)
				{
					# caracteristique qualitative
					req = new("RequeteDB")
					# this query will get characteristics from lot_pere when null
					req@sql = paste("SELECT ", " ope_date_debut,", " ope_date_fin,",
							" lot_methode_obtention,", " lot_identifiant ,", " lot_effectif,",
							" car_val_identifiant,", " ope_dic_identifiant,", " lot_tax_code,",
							" lot_std_code,", " car_par_code", " FROM ", get_schema(), "vue_ope_lot_ech_parqual", " WHERE ope_dic_identifiant in ",
							vector_to_listsql(r_mig_char@dc@dc_selected), echantillons,
							" AND lot_tax_code in ", vector_to_listsql(r_mig_char@taxa@taxa_selected),
							" AND lot_std_code in ", vector_to_listsql(r_mig_char@stage@stage_selected),
							" AND car_par_code in ", vector_to_listsql(parqual), " AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '",
							r_mig_char@horodatedebut@horodate, "', TIMESTAMP '", r_mig_char@horodatefin@horodate,
							"')", sep = "")
					r_mig_char@data[["parqual"]] <- query(req)@query
				}  # end if (parqual)
				if (length(parquan) != 0)
				{
					# Caracteristique quantitative
					req = new("RequeteDB")
					# we round the date to be consistent with daily values from the
					req@sql = paste("SELECT ", " ope_date_debut,", " ope_date_fin,",
							" lot_methode_obtention,", " lot_identifiant ,", " lot_effectif,",
							" car_valeur_quantitatif,", " ope_dic_identifiant,", " lot_tax_code,",
							" lot_std_code,", " car_par_code", " FROM ", get_schema(),
							"vue_ope_lot_ech_parquan", " WHERE ope_dic_identifiant in ",
							vector_to_listsql(r_mig_char@dc@dc_selected), echantillons,
							" AND lot_tax_code in ", vector_to_listsql(r_mig_char@taxa@taxa_selected),
							" AND lot_std_code in ", vector_to_listsql(r_mig_char@stage@stage_selected),
							" AND car_par_code in ", vector_to_listsql(parquan), " AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '",
							r_mig_char@horodatedebut@horodate, "', TIMESTAMP '", r_mig_char@horodatefin@horodate,
							"')", sep = "")
					
					r_mig_char@data[["parquan"]] <- query(req)@query
				}  # end if (parquan)
			}  # end else  
			return(r_mig_char)
		})


#' Turns a continuous parameter into discrete values 
#' 
#' The parm name becomes "parm_discrete". New values are created in the `data[["parqual"]]` slot 
#' of the report and the parqual slot is updated
#' 
#' @param object An object of class \link{ref_parquan-class}
#' @param par The code of a quantitative parameter
#' @param silent Default FALSE, if TRUE the program should not display messages
#' @param ... Additional parms to the cut method \link[base]{cut}   
#' @return An object of class \link{ref_parquan-class} with lines removed from \code{r@data[["parquan"]]}
#' and added (after transformation to qualitative values) in \code{r@data[["parqal"]]}
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
setMethod("setasqualitative", signature = signature("report_mig_char"), definition = function(object,
				par, silent = FALSE, ...) {
			r_mig_char <- object
			# par <-'A124' ========= initial checks ================
			if (!inherits(par , "character"))
				stop("par should be a character")
			if (nrow(r_mig_char@data[["parquan"]]) == 0)
				funout(gettext("No data for quantitative parameter, perhaps you forgot to run the calcule method"))
			if (!par %in% r_mig_char@parquan@par_selected)
				funout(gettextf("The parameter %s is not in the selected parameters", par),
						arret = TRUE)
			if (!par %in% r_mig_char@parquan@data$par_code)
				funout(gettextf("No data for this parameter : %s, nothing to do", par), arret = TRUE)

			# r_mig_char@data[["parqual"]] in report_mig_char -----------------------------	
			
			newtabqual <- r_mig_char@data[["parquan"]]
			lignes_du_par <- newtabqual$car_par_code == par
			newtabqual <- newtabqual[lignes_du_par, ]
			nbnaquan <- sum(is.na(newtabqual$car_valeur_quantitatif))
			newtabqual$car_valeur_quantitatif <- cut(newtabqual$car_valeur_quantitatif, ...)
			nbnaqual <- sum(is.na(newtabqual$car_valeur_quantitatif))

			if (all(is.na(newtabqual$car_valeur_quantitatif))) stop("Only NA produced, please check the bounds")
			if (nbnaqual > nbnaquan) warning(sprintf("You are producing %s NA values, maybe change your limits",nbnaqual - nbnaquan))
			# newtabqual$car_valeur_quantitatif<-cut(newtabqual$car_valeur_quantitatif,breaks=c(0,1.5,2.5,10),label=c('1','2','3'))
			newtabqual <- chnames(newtabqual, "car_valeur_quantitatif", "car_val_identifiant")
			newtabqual$car_par_code <- paste0(newtabqual$car_par_code,"_discrete")
			new_car_code <- newtabqual$car_par_code[1] # e.g "A124_qualitatif"
			new_car_nom <- paste0(r_mig_char@parquan@data[r_mig_char@parquan@data$par_code==par,"par_nom"], " (qual)")

			tabqual <- r_mig_char@data[["parqual"]]
			# remove first lines already processed earlier in valqual with the same parm
			if (!is.null(tabqual)){
				tabqual <- tabqual[!tabqual$car_par_code %in% new_car_code,]			
			}
			r_mig_char@data[["parqual"]] <- rbind(tabqual, newtabqual)
			# Adding the par to parqual
			
			# valqual slot in parqual -----------------------------			
			
			tabvalqual <- r_mig_char@parqual@valqual
			if (!is.null(tabvalqual)){
				tabvalqual <- tabvalqual[!tabvalqual$val_qual_code %in% new_car_code,]
			}
			tabvalqual <- rbind(
					tabvalqual,
					data.frame(val_identifiant = levels(newtabqual$car_val_identifiant),
							val_qal_code = new_car_code,
							val_rang = 1:length(levels(newtabqual$car_val_identifiant)),
							val_libelle = levels(newtabqual$car_val_identifiant))
			)
			r_mig_char@parqual@valqual <- tabvalqual
			
			# data slot in parqual -----------------------------

			tabdata <- r_mig_char@parqual@data 
			if (!is.null(tabdata)){
				tabdata <- tabdata[!tabdata$par_code %in% new_car_code,]
			}
			tabdata <- rbind(tabdata, 
					c("par_code"=new_car_code, "par_nom"=new_car_nom,"par_unite"=NA, "par_nature"=NA,"par_definition"=NA,"qual_valeurs_possibles"=NA)
			)
			colnames(tabdata) <- c("par_code", "par_nom", "par_unite", "par_nature", "par_definition", "qal_valeurs_possibles")
			r_mig_char@parqual@data <-tabdata
			
			# selected parm in parqual -----------------------------

			r_mig_char@parqual@par_selected <- unique(c(r_mig_char@parqual@par_selected, new_car_code))
			
			if (!silent)
				funout(gettextf("%s lines have been converted from quantitative to qualitative parameters",
								nrow(newtabqual)))
			return(r_mig_char)
		})


# TODO create a dataframe with only one line per fish for all parameters
#' Computes data to a standard format for the summary and plot methods.
#' 
#' Merges the content of the list elements 'parqual' and 'parquan' in the data slot, and creates
#' a single dataframe with one line per qualitative and quantitative pair. This methods allow to 
#' cross one quantity (e.g. length) with a qualitative parameter (e.g. sex).
#' 
#' @param object An object of class \link{report_mig_char-class}
#' @param silent Boolean default FALSE, if TRUE information messages not displayed
#' @return An object of class \link{report_mig_char-class} with slot \code{@calcdata} filled
#' @aliases calcule.report_mig_char
setMethod("calcule", signature = signature("report_mig_char"), definition = function(object,
				silent = FALSE) {
			r_mig_char <- object
			qual <- r_mig_char@data[["parqual"]]
			quan <- r_mig_char@data[["parquan"]]
			if (is.null(qual) & is.null(quan))
				stop("cannot perform calcule method, no data in either qualitative or quantitative parameters")
			if (!is.null(qual))
				qual <- chnames(qual, "car_par_code", "car_par_code_qual")
			if (!is.null(quan))
				quan <- chnames(quan, "car_par_code", "car_par_code_quan")
			if (is.null(qual)) {
				quaa <- quan
				quaa$car_par_code_qual = NA
			} else if (is.null(quan)) {
				quaa <- qual
				quaa$car_par_code_quan = NA
			} else {
				quaa <- merge(qual, quan, by = c("ope_dic_identifiant", "lot_identifiant",
								"ope_date_debut", "ope_date_fin", "lot_methode_obtention", "lot_effectif",
								"lot_tax_code", "lot_std_code"), all.x = TRUE, all.y = TRUE)
			}
			quaa = fun_date_extraction(data = quaa, nom_coldt = "ope_date_debut")
			quaa <- quaa[order(quaa$ope_dic_identifiant, quaa$lot_tax_code, quaa$lot_std_code,
							quaa$ope_date_debut), ]
			r_mig_char@calcdata <- quaa
			if (!silent)
				funout(gettext("The calculated data are in slot calcdata"))
			assign("r_mig_char", r_mig_char, envir_stacomi)
			return(r_mig_char)
		})


#' plot method for report_mig_char
#' 
#' 
#' @param x An object of class report_mig_char
#' @param plot.type One of 'qual', 'quant' 'crossed' default to qual
#' @param color_parm A named vector for the colors of either parameters (if plot.type=quant) or levels for
#' parameters (if plot.type=qual).
#' @param silent Boolean default FALSE, if TRUE information messages not displayed
#' @param ... Additional parameters
#' @return Nothing, called for its side effect of plotting data
#' @aliases plot.report_mig_char
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @export
setMethod("plot", signature = signature(x = "report_mig_char", y = "missing"), 
		definition = function(x,
				color_parm = NULL, plot.type = "qual", silent = FALSE, ...) {
			r_mig_char <- x
			if (nrow(r_mig_char@calcdata) == 0)
				stop("no data in calcdata, have you forgotten to run calculations ?")
			# transformation du tableau de donnees color_parm<-c('age 1'='red','age
			# 2'='blue','age 3'='green') color_parm<-c('C001'='red')
			if (plot.type == "qual")
			{
				parlevels <- r_mig_char@parqual@valqual$val_identifiant
				if (nrow(r_mig_char@parqual@valqual)==0) stop("No data loaded in qualitative parameters")
				cs <- colortable(color = color_parm, vec = parlevels, palette = "Dark2")
				cs <- stacomirtools::chnames(cs, "name", "car_val_identifiant")
				calcdata <- r_mig_char@calcdata
				calcdata <- merge(calcdata, cs)
				g <- ggplot(calcdata) + geom_bar(aes(x = mois, y = lot_effectif, fill = color),
								stat = "identity") + xlab(gettext("Month")) + ylab(gettext("Number")) +
						scale_fill_identity(name = gettext("Classes"), labels = cs[, "car_val_identifiant"],
								breaks = cs[, "color"], guide = "legend") + theme_bw()
				
				assign("g", g, envir_stacomi)
				if (!silent)
					funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi) \n",
									domain = "R-stacomiR"))
				print(g)
			}  #end plot.type = 'qual'
			if (plot.type == "quant")
			{
				calcdata <- r_mig_char@calcdata
				calcdata$car_par_code_quan[is.na(calcdata$car_par_code_quan)] <- "NA"
				the_parms <- unique(calcdata$car_par_code_quan)
				cs <- colortable(color = color_parm, vec = the_parms, palette = "Dark2")
				cs <- stacomirtools::chnames(cs, "name", "car_par_code_quan")
				calcdata <- merge(calcdata, cs)
				g <- ggplot(calcdata) + geom_point(aes(x = ope_date_debut, y = car_valeur_quantitatif,
										col = color), stat = "identity") + xlab(gettext("Month")) + ylab(gettext("Quantitative parameter")) +
						scale_colour_identity(name = gettext("Param"), labels = cs[, "car_par_code_quan"],
								breaks = cs[, "color"], guide = "legend") + theme_bw()
				assign("g", g, envir_stacomi)
				if (!silent)
					funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi) \n",
									domain = "R-stacomiR"))
				print(g)
			}  #end plot.type='quant'
			if (plot.type == "crossed")
			{
				parlevels <- r_mig_char@parqual@valqual$val_identifiant
				
				cs <- colortable(color = color_parm, vec = parlevels, palette = "Dark2")
				cs <- stacomirtools::chnames(cs, "name", "car_val_identifiant")
				calcdata <- r_mig_char@calcdata
				#calcdata$car_val_identifiant
				calcdata <- merge(calcdata, cs)
				if (length(unique(calcdata$car_par_code_quan))==1){
					label <- paste(
							r_mig_char@parquan@data[r_mig_char@parquan@par_selected ==r_mig_char@parquan@data$par_code,"par_nom"],
							" (",
							r_mig_char@parquan@data[r_mig_char@parquan@par_selected ==r_mig_char@parquan@data$par_code,"par_unite"],
							")", sep="")
				g <- ggplot(calcdata) + geom_point(aes(x = ope_date_debut, y = car_valeur_quantitatif,
										col = color), stat = "identity") + xlab(gettext("Month")) + ylab(label) +
						scale_colour_identity(name = gettext("Param"), labels = cs[, "car_val_identifiant"],
								breaks = cs[, "color"], guide = "legend") + theme_bw()
			} else {
				g <- ggplot(calcdata) + geom_point(aes(x = ope_date_debut, y = car_valeur_quantitatif,
										col = color), stat = "identity") + xlab(gettext("Month")) + ylab(gettext("Quantitative parameter")) +
						scale_colour_identity(name = gettext("Param"), labels = cs[, "car_val_identifiant"],
								breaks = cs[, "color"], guide = "legend") + 
						facet_wrap(~car_par_code_quan, scales="free_y") +					
						theme_bw() 
			}
				assign("g", g, envir_stacomi)
				if (!silent)
					funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi) \n",
									domain = "R-stacomiR"))
				print(g)
			}  #end plot.type='xyplot'
			return(invisible(NULL))
		})


#' summary for report_mig_char 
#' @param object An object of class \code{\link{report_mig_char-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters
#' @return A table with the summary
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases summary.report_mig_char
#' @export
setMethod("summary", signature = signature(object = "report_mig_char"), definition = function(object,
				silent = FALSE, ...) {
			r_mig_char <- object
			bm <- r_mig_char@calcdata
			if (nrow(bm) == 0)
				stop("No data in slot calcdata, did you forget to run the calcule method ?")
			if (length(unique(bm$annee)) == 1) {
				table = round(tapply(bm$lot_effectif, list(bm$mois, bm$car_val_identifiant),
								sum), 1)
				table <- rbind(table, colSums(table, na.rm = TRUE))
				rownames(table)[nrow(table)] <- gettext("Sum")
				table <- as.data.frame(table)
			} else {
				table = round(tapply(bm$lot_effectif, list(bm$annee, bm$mois, bm$car_val_identifiant),
								sum), 1)
				ftable2data.frame <- function(x, ...) {
					y <- format(x, quote = FALSE)
					z <- data.frame(y[-1, ], stringsAsFactors = FALSE)
					names(z) <- y[1, ]
					z
				}
				table <- ftable2data.frame(ftable(table))
			}
			return(table)
		})


#' xtable function for \link{report_mig_char-class}
#' create an xtable objet to be later used by the print.xtable method.
#' @param x, an object of class 'report_mig_char'
#' @param caption, see xtable
#' @param label, see xtable
#' @param align, see xtable, overidden if NULL
#' @param digits, see xtable
#' @param display see xtable
#' @param auto see xtable
#' @param ... Additional parameters
#' @return A xtable
#' @aliases xtable.report_mig_char
#' @export
setMethod("xtable", signature = signature("report_mig_char"), definition = function(x,
				caption = NULL, label = NULL, align = NULL, ...) {
			r_mig_char <- x
			dat = r_mig_char@data
			dc = stringr::str_c(r_mig_char@dc@dc_selected, collapse = " ")
			tax = stringr::str_c(r_mig_char@taxa@taxa_selected, collapse = " ")
			std = stringr::str_c(r_mig_char@stage@stage_selected, collapse = " ")
			
			dat <- summary(r_mig_char, silent = TRUE)
			
			xt <- xtable::xtable(dat, ...)
			if (is.null(align)) {
				align <- c("l", rep("r", ncol(dat)))
				align(xt) <- align
			}
			if (is.null(display)) {
				display = c("s", rep("f", ncol(dat)))
				display(xt) <- display
			}
			if (is.null(caption)) {
				caption = gettextf("Summary for dc %s, taxa %s, stage %s.", dc, tax,
						std)
				caption(xt) <- caption
			}
			return(xt)
		})

