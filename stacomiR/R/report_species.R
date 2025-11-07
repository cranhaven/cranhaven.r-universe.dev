#' Counts of number per taxa/stages
#' 
#' This class is used to make the assessment of all species, and their number. It is intended
#' as a simple way to check what fishes are present (taxa + development stage). It was altered to include ref_taxa,
#' to allow excluding some of the most numerous taxa from reports. The taxa is reported unless 
#' a taxa has several stages, in which case the different stages for the taxa will be reported
#' Using the split arguments
#' the calc method of the class will count numbers, subsamples are not accounted for in the Overview.
#' The split argument currently takes values year or month. The class is intended to be used over long periods
#' e.g years. The plot method writes either an histogram or a pie chart of number per
#' year/week/month.
#' @slot dc an object of class \link{ref_dc-class} 
#' @slot taxa Object of class \link{ref_taxa-class}: the species
#' @slot start_year Object of class \code{\link{ref_year-class}}
#' @slot end_year Object of class \code{\link{ref_year-class}}
#' @slot data \code{data.frame}
#' @slot calcdata \code{data.frame} with data processed by the calc method
#' @slot split Object of class \code{\link{ref_list-class}} ref_list referential class choose within a list
#' @include ref_taxa.R
#' @include ref_dc.R
#' @include ref_list.R
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @family report Objects
#' @aliases report_species 
#' @example inst/examples/report_species-example.R
#' @keywords classes
#' @export
setClass(Class = "report_species", representation = representation(dc = "ref_dc", taxa = "ref_taxa",
				start_year = "ref_year", end_year = "ref_year", data = "data.frame", calcdata = "data.frame",
				split = "ref_list"), prototype = prototype(dc = new("ref_dc"), taxa = new("ref_taxa"), start_year = new("ref_year"),
				end_year = new("ref_year"), data = data.frame(), calcdata = data.frame(), split = new("ref_list")))


#' connect method for report_species
#' @param object An object of class report_species
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class \link{report_species-class} with data slot filled with slot data \code{@data} filled
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases connect.report_species
setMethod("connect", signature = signature("report_species"), definition = function(object,
				silent = FALSE) {
			bilesp <- object
			requete = new("RequeteDB")
			start_year = bilesp@start_year@year_selected
			end_year = bilesp@end_year@year_selected
			requete@sql = paste("SELECT lot_identifiant, ope_date_debut, ope_date_fin,",
					" lot_effectif, lot_tax_code, lot_std_code, tax_nom_latin, std_libelle,",
					" date_part('year', ope_date_debut) as annee,", " date_part('month',ope_date_debut) as mois,",
					" date_part('week',ope_date_debut) as semaine", " FROM ", 
					get_schema(), 
					"t_operation_ope", " INNER JOIN ", 
					get_schema(),
					"t_lot_lot ON ope_identifiant=lot_ope_identifiant", " INNER JOIN ref.tr_taxon_tax on tax_code=lot_tax_code",
					" INNER JOIN ref.tr_stadedeveloppement_std on std_code=lot_std_code", " WHERE extract(year from ope_date_debut)>=",
					start_year, " AND extract(year from ope_date_debut)<=", end_year, " AND ope_dic_identifiant in",
					vector_to_listsql(bilesp@dc@dc_selected), "AND lot_tax_code in", vector_to_listsql(bilesp@taxa@taxa_selected),
					" AND lot_lot_identifiant IS NULL",
					" AND lot_effectif IS NOT NULL", sep = "")
			requete <- stacomirtools::query(requete)
			if (requete@status != "success")
				funout(gettext("Query failed for for report species \n", domain = "R-stacomiR"),
						arret = TRUE)
			bilesp@data <- requete@query
			if (!silent)
				funout(gettext("data loaded from the database for report_species"))
			assign("bilesp", bilesp, envir = envir_stacomi)
			return(bilesp)
		})


#' command line interface for \link{report_species-class}
#' 
#' @param object An object of class \link{report_species-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,ref_dc-method}
#' @param taxa Either 'all' (default) or a species name in latin or the SANDRE code for species (ie 2038=Anguilla anguilla),
#' it should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,ref_taxa-method}
#' @param start_year The starting the first year, passed as character or integer
#' @param end_year the finishing year
#' @param split one of c('none','week','month','year')
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class \link{report_species-class} with data selected
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases choice_c.report_species
setMethod("choice_c", signature = signature("report_species"), 
		definition = function(object,
				dc, 
				taxa="all",
				split = "none",
				start_year, 
				end_year, 
				silent = FALSE) {
			# code for debug using example
			# dc=c(5,6);taxa = c(start_year='1996';end_year='2016';split='none';silent=TRUE
			bilesp <- object
			bilesp@dc = charge(bilesp@dc)
			# loads and verifies the dc this will set dc_selected slot
			bilesp@dc <- choice_c(object = bilesp@dc, dc)
			
			bilesp@taxa <-
					charge_with_filter(object = bilesp@taxa, bilesp@dc@dc_selected)
			
			if (any(taxa=="all")) {
				# taxa selected correspond to all loaded taxa for the dc
				bilesp@taxa@taxa_selected <- bilesp@taxa@data$tax_code
				# Here we are not using the standard choice_c which assigns in envir_stacomi...
				assign("ref_taxa", bilesp@taxa, envir=envir_stacomi)
			} else {
				bilesp@taxa <- choice_c(bilesp@taxa, taxa)
			}
			bilesp@split = charge(object = bilesp@split, listechoice = c("none", "week",
							"month", "year"), label = gettext("choice of number in sample (one, several,all)",
							domain = "R-stacomiR"))  # choix de la categorie d'effectif)
			bilesp@split <- choice_c(bilesp@split, selectedvalue = split)
			# by default choice_c returns reflist but usefull to mimic gr.interface
			assign("refliste", bilesp@split, envir_stacomi)
			bilesp@start_year <- charge(object = bilesp@start_year, objectreport = "report_species")
			bilesp@start_year <- choice_c(object = bilesp@start_year, nomassign = "start_year",
					annee = start_year, silent = silent)
			bilesp@end_year@data <- bilesp@start_year@data
			bilesp@end_year <- choice_c(object = bilesp@end_year, nomassign = "end_year",
					annee = end_year, silent = silent)
			assign("bilesp", bilesp, envir = envir_stacomi)
			return(bilesp)
		})


#' charge method for report_species
#' 
#' Verifies the content of objects when the graphical interface is used, it is not necessary
#' to call the charge method if the choice_c method has been used
#' @param object An object of class \link{report_species-class}
#' @param silent Stops displaying the messages. 
#' @return report_species with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @return An object of class \link{report_species-class} with slots filled from values assigned in \code{envir_stacomi} environment
#' @aliases charge.report_species
#' @keywords internal
setMethod("charge", signature = signature("report_species"), definition = function(object,
				silent = FALSE) {
			if (!silent)
				funout(gettext("Checking objects and launching query\n", domain = "R-stacomiR"))
			bilesp <- object
			if (exists("ref_dc", envir_stacomi)) {
				bilesp@dc <- get("ref_dc", envir_stacomi)
			} else {
				funout(gettext("You need to choose a counting device, clic on validate\n",
								domain = "R-stacomiR"), arret = TRUE)
			}
			if (exists("ref_taxa", envir_stacomi)) {
				bilesp@taxa <- get("ref_taxa", envir_stacomi)
			} else {
				funout(
						gettext("You need to choose a taxa, clic on validate\n", domain = "R-stacomiR"),
						arret = TRUE
				)
			}
			if (exists("start_year", envir_stacomi)) {
				bilesp@start_year <- get("start_year", envir_stacomi)
			} else {
				funout(gettext("You need to choose the starting year\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			if (exists("end_year", envir_stacomi)) {
				bilesp@end_year <- get("end_year", envir_stacomi)
			} else {
				funout(gettext("You need to choose the ending year\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			
			if (exists("refliste", envir_stacomi)) {
				bilesp@split <- get("refliste", envir_stacomi)
			} else {
				funout(gettext("You need to choose a size class\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			assign("bilesp", bilesp, envir_stacomi)
			if (!silent)
				funout(gettext("A report_species object was stored into envir_stacomi environment : write bilesp=get('bilesp',envir_stacomi)",
								domain = "R-stacomiR"))
			return(bilesp)
		})



#' calcule method for report_species
#' 
#' The number will be split according to the split argument passed to the class, e.g.
#' per year or month or week. Data from different DC will be grouped. Counts are given per taxa,
#' unless there are several stages, in which case the counts correspond to taxa + stage.
#' @param object An object of class \link{report_species-class}
#' @param silent Boolean, if TRUE, information messages are not displayed, only warnings and errors
#' @return An object of class \link{report_species-class} with calcdata slot filled.
#' @aliases calcule.report_species
setMethod("calcule", signature = signature("report_species"), definition = function(object,
				silent = FALSE) {
			bilesp <- object
			DC = as.numeric(bilesp@dc@dc_selected)
			# update of refliste which does not need calcul button pushed
			tableEspeces = bilesp@data
			if (nrow(tableEspeces) == 0)
				funout(gettext("No fish in the database for this period\n", domain = "R-stacomiR"),
						arret = TRUE)
			tableEspeces$taxa_stage = paste(tableEspeces$tax_nom_latin, tableEspeces$std_libelle,
					sep = "_")
			# only keeping taxa stage for species with several stages
			nbstage = tapply(tableEspeces$tax_nom_latin, tableEspeces$taxa_stage, function(X) (length(unique(X))))
			# we currently have taxa+stage, below this is replaces with taxa unless
			# there are more than one stage per species
			if (length(nbstage[nbstage > 1]) > 0) {
				les_multiples = names(nbstage[nbstage > 1])
				tableEspeces[!tableEspeces$taxa_stage %in% les_multiples, "taxa_stage"] <- tableEspeces$tax_nom_latin[!tableEspeces$taxa_stage %in%
								les_multiples]
			} else tableEspeces$taxa_stage <- tableEspeces$tax_nom_latin
			if (min(tableEspeces$lot_effectif) < 0) {
				if (!silent)
					funout(gettext("Some negative counts are transformed into positive ones\n",
									domain = "R-stacomiR"))
				tableEspeces$lot_effectif = abs(tableEspeces$lot_effectif)
			}
			sumEspeces = switch(bilesp@split@selectedvalue, year = as.data.frame(xtabs(lot_effectif ~
											taxa_stage + annee, data = tableEspeces)), month = as.data.frame(xtabs(lot_effectif ~
											taxa_stage + mois, data = tableEspeces)), week = as.data.frame(xtabs(lot_effectif ~
											taxa_stage + semaine, data = tableEspeces)), none = as.data.frame(xtabs(lot_effectif ~
											taxa_stage, data = tableEspeces)))
			colnames(sumEspeces)[colnames(sumEspeces) == "Freq"] <- "Effectif"  # pas forcement le m nb de colonnes
			if (bilesp@split@selectedvalue != "none") {
				colnames(sumEspeces)[2] <- bilesp@split@selectedvalue
			}
			bilesp@calcdata <- sumEspeces
			assign("bilesp", bilesp, envir_stacomi)
			return(bilesp)
		})

#' Plot method for report_species
#' 
#' @param x An object of class \link{report_species-class}
#' @param plot.type Default pie
#' #' \describe{
#'   \item{plot.type='pie'}{A pie}' 
#'   \item{plot.type='barchart'}{A barchart}
#' }
#' @param color Default NULL, a vector of colors of length corresponding to the number of taxa-stage
#' different values, use unique(bilesp@calcdata$taxa_stage) to get that number. The color applies to both
#' pie and barchart plots
#' @param silent Stops displaying the messages
#' @return Nothing, called for producing plots
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases plot.reportreport_species
#' @export
setMethod("plot", signature(x = "report_species", y = "missing"), definition = function(x,
				plot.type = "pie", color = NULL, silent = FALSE) {
			bilesp <- x
			if (nrow(bilesp@calcdata) == 0)
				stop("No data in the calcdata slot, did you forget to run calculations ?")
			nb = length(unique(bilesp@calcdata$taxa_stage))
			g <- ggplot(bilesp@calcdata)
			g <- g + geom_col(aes(x = "", y = Effectif, fill = taxa_stage)) + ggtitle(paste("report Especes, DC",
									str_c(bilesp@dc@dc_selected, collapse = "+"), bilesp@start_year@year_selected,
									"=>", bilesp@end_year@year_selected)) + xlab("") + ylab(gettext("Number",
									domain = "R-stacomiR"))
			# theme(axis.line.x=element_line('none'))+theme(axis.title.x=
			# element_text('none'))
			if (bilesp@split@selectedvalue != "none") {
				facet <- switch(bilesp@split@selectedvalue, year = as.formula(~year), month = as.formula(~month),
						week = as.formula(~week))
				g <- g + facet_wrap(facet, scales = "fixed")
			}
			if (is.null(color)) {
				if (nb <= 8) {
					g <- g + scale_fill_brewer(palette = "Accent", name = gettext("Taxa-stage",
									domain = "R-stacomiR"))
				} else if (nb <= 12) {
					p <- g + scale_fill_brewer(palette = "Set3", name = gettext("Taxa-stage",
									domain = "R-stacomiR"))
				} else {
					g <- g + scale_fill_manual(values = grDevices::rainbow(nb), name = gettext("Taxa-stage",
									domain = "R-stacomiR"))
				}
			} else {
				# color is not null
				if (length(color) != nb)
					stop(gettextf("The vector of color should be of length %s", domain = "R-stacomiR",
									nb))
				g <- g + scale_fill_manual(values = color, gettext("Taxa-stage", domain = "R-stacomiR"))
			}
			if (plot.type == "barplot") {
				print(g)
				assign("g", g, envir = envir_stacomi)
			} else if (plot.type == "pie") {
				g <- g + coord_polar(theta = "y", start = pi) + xlab("") + ylab("")
				print(g)
				assign("g", g, envir = envir_stacomi)
			} else {
				funout(gettext("plot.type should be one of barplot or pie", domain = "R-stacomiR"),
						arret = TRUE)
			}
			if (!silent)
				funout(gettext("the object g has been assigned to envir_stacomi", domain = "R-stacomiR"))
			
			return(invisible(NULL))
		})



#' summary for report_species 
#' 
#'  generate csv and html output in the user data directory
#' @param object An object of class \code{\link{report_species-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @return nothing, but writes summary in \code{get("datawd", envir = envir_stacomi)}, and prints output
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases summary.report_species
#' @export
setMethod("summary", signature = signature(object = "report_species"), definition = function(object,
				silent = FALSE) {
			bilesp <- object
			if (nrow(bilesp@calcdata) == 0)
				stop("No data in the calcdata slot, did you forget to run calculations ?")
			loc <- str_c(str_c(bilesp@dc@dc_selected, collapse = "+"), bilesp@start_year@year_selected,
					bilesp@end_year@year_selected, sep = "_")
			
			path = file.path(normalizePath(path.expand(get("datawd", envir = envir_stacomi))),
					paste("tableEspece", loc, ".csv", sep = ""), fsep = "\\")
			res <- tryCatch(
					write.table(bilesp@calcdata, path, row.names = TRUE, col.names = TRUE, sep = ";",
							append = FALSE),
					error = function(e) e,
					finally =
							if (!silent) 		{
								funout(gettextf("writing of %s \n", path))
								funout(gettextf("attention, negative numbers were transformed into positive numbers"))
							})
			if (inherits(res, "simpleError")) {
				warnings("The table could not be reported, please change the working directory datawd with options(stacomiR.path='path/to/directory'")
			}
			
		})

