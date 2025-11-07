#' Class "report_mig_env"
#'
#' Enables to compute an annual overview of fish migration and environmental
#' conditions in the same chart. Environmental conditions may trigger migration events, variation in flow
#' or temperatures can  be plotted along migration to check graphically for a possible relation. To enable this,
#' environmental conditions are loaded from an "environmental monitoring station", which records environmental
#' parameters and is attached to a migratory station in the database.
#' This class enables both continuous output (temperature -flow) as well as discrete parameters (qualitative = moon
#' phase, type of operation of a gate, opening of a gate...) which will be displayed on the graph. Values are scaled so that
#' single plot can display migration numbers and environmental parameters. Environmental parameters when stored at a
#' time scale lower that a day are averaged per day, unless they are qualitative parameters, in which case only the first
#' event of the day is displayed on the annual plot.
#'
#' @include report_mig_mult.R
#' @include report_env.R
#' @include create_generic.R
#' @include utilities.R
#' @slot report_mig_mult \link{report_mig_mult-class}
#' @slot report_env \link{report_env-class}
#' @author cedric.briand@eptb-vilaine.fr marion.legrand@logrami.fr
#' @family report Objects
#' @keywords classes
#' @aliases report_mig_env
#' @keywords classes
#' @example inst/examples/report_mig_env-example.R
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @family report Objects
#' @keywords classes
#' @export
setClass(
		Class = "report_mig_env",
		representation =
				representation(report_mig_mult = "report_mig_mult",
						report_env = "report_env"),
		prototype = prototype(
				report_mig_mult = new("report_mig_mult"),
				report_env = new("report_env")
		
		)
)


setValidity("report_mig_env",
		function(object)
		{
			rep1 = validObject(object@report_mig_mult, test = TRUE)
			rep2 = validObject(object@report_env, test = TRUE)
			return(ifelse(rep1 & rep2 , TRUE, c(1:2)[!c(rep1, rep2)]))
		})
#' connect method for report_mig_env class
#' @param object An object of class \link{report_mig_env-class}
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return an object of report_mig_env class
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases summary.report_mig_env
setMethod(
		"connect",
		signature = signature("report_mig_env"),
		definition = function(object, silent = FALSE) {
			#object<-r_mig_env
			r_mig_env <- object
			r_mig_env@report_mig_mult <-
					connect(r_mig_env@report_mig_mult, silent = silent)
			r_mig_env@report_env <- connect(r_mig_env@report_env, silent = silent)
			return(r_mig_env)
		}
)
#' command line interface for report_env class
#' 
#' @param object An object of class \link{report_env-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,ref_dc-method}
#' @param taxa '2038=Anguilla anguilla',
#' these should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,ref_taxa-method}
#' @param stage 'AGJ=Yellow eel', 'AGG=Silver eel', 'CIV=glass eel'
#' @param stationMesure A character, the code of the monitoring station, which records environmental parameters \link{choice_c,ref_env-method}
#' @param datedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param datefin The finishing date of the report, for this class this will be used to calculate the number of daily steps.
#' @param silent Boolean default FALSE, if TRUE information messages not displayed.
#' @aliases choice_c.report_mig_env
#' @return An object of class \link{report_env-class} with data selected
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
setMethod(
		"choice_c",
		signature = signature("report_mig_env"),
		definition = function(object,
				dc,
				taxa,
				stage,
				stationMesure,
				datedebut,
				datefin,
				silent = FALSE) {
			# code for debug
			# dc=c(5,6,12);	taxa=c("Anguilla anguilla");stage=c("AGJ","AGG","CIV");
			# stationMesure=c("temp_gabion","coef_maree");
			# datedebut="2008-01-01";datefin="2008-12-31";silent=FALSE
			r_mig_env <- object
			r_mig_env@report_mig_mult =
					choice_c(
							r_mig_env@report_mig_mult,
							dc = dc,
							taxa = taxa,
							stage = stage,
							datedebut = datedebut,
							datefin = datefin,
							silent = silent
					)
			r_mig_env@report_env = choice_c(
					r_mig_env@report_env,
					stationMesure = stationMesure,
					datedebut = datedebut,
					datefin = datefin,
					silent = silent
			)
			return(r_mig_env)
		}
)
#' charge method for report_mig_env class
#'
#' #' Unique the other report classes where the charge method is only used by the graphical interface
#' to collect and test objects in the environment envir_stacomi, and see if the right choices have
#' been made in the graphical interface, this methods runs the \link{charge,report_mig_mult-method}
#' and needs to be called from the command line (see examples)
#' @param object An object of class \link{report_mig_env-class}
#' @param silent Should the function remain silent (boolean)
#' @return An object of class \link{report_mig_env-class} with data set from values assigned in \code{envir_stacomi} environment
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases charge.report_mig_env
setMethod(
		"charge",
		signature = signature("report_mig_env"),
		definition = function(object, silent = FALSE) {
			# silent=FALSE
			r_mig_env <- object
			r_mig_env@report_mig_mult <-
					charge(r_mig_env@report_mig_mult, silent = silent)
			# the values for date are not initiated by the interface
			assign(
					"report_env_date_debut",
					get("timestep", envir_stacomi)@"dateDebut",
					envir_stacomi
			)
			assign("report_env_date_fin",
					as.POSIXlt(end_date(get(
											"timestep", envir_stacomi
									))),
					envir_stacomi)
			r_mig_env@report_env <-
					charge(r_mig_env@report_env, silent = silent)
			return(r_mig_env)
		}
)

#' Calculations for migration in the class \link{report_mig_env-class}
#'
#' Runs the calcule method in \link{report_mig_mult-class}
#' @param object An object of class \link{report_mig_env-class}
#' @param silent Boolean default FALSE, if TRUE information messages not displayed
#' @aliases calcule.report_mig_env
#' @return \link{report_mig_env-class} with data in slot r_mig_env@report_mig_mult@calcdata
setMethod(
		"calcule",
		signature = signature("report_mig_env"),
		definition = function(object, silent = FALSE) {
			# silent=FALSE
			r_mig_env <- object
			r_mig_env@report_mig_mult <-
					calcule(r_mig_env@report_mig_mult, silent = silent)
			if (!silent)
				funout(
						gettext(
								"r_mig_env object is stocked into envir_stacomi environment\n",
								domain = "R-stacomiR"
						)
				)
			return(r_mig_env)
		}
)


#' Plot method for report_mig_env
#' @param x An object of class \link{report_mig_env}
#' @param silent Stops displaying the messages.
#' @param color_station A named vector of station color (e.g. c("temp_gabion"="red","coef_maree"="blue","phases_lune"="green")) default null
#' @param color_dc A named vector giving the color for each dc default null (e.g. c("5"="#4D4D4D","6"="#E6E6E6","12"="#AEAEAE"))
#' @return Nothing, called for its side effect of plotting 
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases plot.report_mig_env
#' @importFrom dplyr pull
#' @export
setMethod(
		"plot",
		signature(x = "report_mig_env", y = "missing"),
		definition = function(x,
				color_station = NULL,
				color_dc = NULL,
				silent = FALSE) {
			#color_station=NULL;color_dc=NULL
			# color_station<-c("temp_gabion"="red","coef_maree"="blue","phases_lune"="green")
			# color_dc=c("5"="#4D4D4D","6"="#E6E6E6","12"="#AEAEAE")
			r_mig_env <- x
			
			
			grdata <- fun_aggreg_for_plot(r_mig_env@report_mig_mult)
			# we collect the dataset used to build the graph
			
			taxa <- as.character(r_mig_env@report_mig_mult@taxa@data[
							r_mig_env@report_mig_mult@taxa@data$tax_code %in% 
									r_mig_env@report_mig_mult@taxa@taxa_selected, "tax_nom_latin"]
			)
			stage <- as.character(r_mig_env@report_mig_mult@stage@data[
							r_mig_env@report_mig_mult@stage@data$std_code%in% 
									r_mig_env@report_mig_mult@stage@stage_selected,"std_libelle"]
			)
			dc <- unique(grdata$DC)
			stations <- r_mig_env@report_env@stationMesure@data
			dc_code <- r_mig_env@report_mig_mult@dc@data$dc_code[match(dc, r_mig_env@report_mig_mult@dc@data$dc)]
			# tableau conditions environnementales
			tableauCE <- r_mig_env@report_env@data
			if (nrow(tableauCE) == 0) {
				funout(
						gettext(
								"You don't have any environmental conditions within the time period\n",
								domain = "R-stacomiR"
						),
						arret = TRUE
				)
			}
			
			# we collect libelle and source of data from station
			source <- unlist(lapply(regmatches(stations$stm_description,gregexpr("(?<=:([[:space:]])).*", stations$stm_description, perl =TRUE),invert=NA),
							function(X) X[2]))
			if (any(is.na(source))) {
				missing <- stations[is.na(source),"stm_libelle"]
				missing_and_selected <- missing[missing%in%r_mig_env@report_env@stationMesure@env_selected]
				missing_and_selected <- paste(missing_and_selected, collapse=" , ")
				warning(sprintf("the source of data is not present in column stm_description of table tj_stationmesure_stm for stations: %s. \n Please consider adding it by adding something like source: Banque hydro at the end of the line in table tj_stationmesure_stm column stm_description", 
								missing_and_selected))
				source[is.na(source)] <- "Not defined"
			}

			stations$source <- source 
			tableauCE <- merge(tableauCE,stations, by.x="env_stm_identifiant", by.y="stm_identifiant")
			# (?<= ) lookbehind, get the string : with a space after but not capture it. .* capture anything after
			# currently we've added the source of data within the stm_description string so as not to change the db
	    # regmatches, invert = NA returns a list with first elt matching and second non mathing
	    # we want the full list returned with NA when missing match



			
			# the data can be in the POSIXct format, we need to round them
			tableauCE$date <-
					as.POSIXct(Hmisc::roundPOSIXt(tableauCE$env_date_debut, digits = "days"))
			qualitative <- !is.na(tableauCE$env_val_identifiant)
			tableauCEquan <- tableauCE[!qualitative, ]
			tableauCEqual <- tableauCE[qualitative, ]
			if (nrow(unique(cbind(tableauCE$date, tableauCE$stm_libelle))) !=	nrow(tableauCE)) {
				# do not cut character chain below...
      
				funout(
						gettextf(
								"Attention, on one station :there are several entries for the same day : we will calculate average for numeric	and use the first value for qualitative parameter" , sep ="")
						,
						arret = FALSE
				)
				# for quantitative parameters we group by date and station and use the average to
				# extract one value per day
				tableauCEquan <-
						dplyr::select(tableauCEquan, date, stm_libelle, env_valeur_quantitatif) %>%
						dplyr::group_by(date, stm_libelle) %>%
						dplyr::summarize(valeur = mean(env_valeur_quantitatif)) %>%
						dplyr::ungroup()
				# for qualitative value, when there are several values for the same date
				# we arbitrarily select the first
				tableauCEqual <-
						dplyr::select(tableauCEqual, date, stm_libelle, env_val_identifiant) %>%
						dplyr::group_by(date, stm_libelle) %>%
						dplyr::summarize(valeur = first(env_val_identifiant)) %>%
						dplyr::ungroup()
			} else {
				# we want the same format as above
				tableauCEquan <-
						dplyr::select(tableauCEquan, date, stm_libelle, env_valeur_quantitatif) %>%
						dplyr::rename(valeur = env_valeur_quantitatif)
				tableauCEqual <-
						dplyr::select(tableauCEqual, date, stm_libelle, env_val_identifiant) %>%
						dplyr::rename(valeur = env_val_identifiant)
			}
			variables_quant <- unique(tableauCEquan$stm_libelle)
			variables_qual <- unique(tableauCEqual$stm_libelle)
			grdata <- fun_date_extraction(
					grdata,
					nom_coldt = "debut_pas",
					annee = FALSE,
					mois = TRUE,
					quinzaine = TRUE,
					semaine = TRUE,
					jour_an = TRUE,
					jour_mois = FALSE,
					heure = FALSE
			)
		
			# to rescale everything on the same graph
			maxeff <- floor(log10(max(abs(grdata$effectif_total), na.rm = TRUE)))
			
			for (i in 1:length(variables_quant)) {
				diff = maxeff - round(log10(max(tableauCEquan[tableauCEquan$stm_libelle ==
														variables_quant[i], "valeur"], na.rm = TRUE)))
				if (diff != 0 & !is.na(diff)) {
					tableauCEquan[tableauCEquan$stm_libelle == variables_quant[i], "valeur"] <- 
             (tableauCEquan %>% 
             dplyr::filter(stm_libelle ==  variables_quant[i]) %>%
             dplyr::select(valeur) %>% dplyr::pull()) * 10 ^ diff
					variables_quant[i] <- paste(variables_quant[i], ".10^", diff, sep = "")
				} # end if
			} #end for
			yqualitatif = (10 ^ (maxeff)) / 2
			
			ylegend = gettextf(
					"Number, %s, %s",
					paste(variables_quant, collapse = ", "),
					paste(variables_qual, collapse = ", ")
			)
	
			
			######################
			# treatment of data to group by dc
			# if several taxa or stages are passed, they are aggregated with a warning
			#################################
			if (length(unique(taxa)) > 1)
				warning(gettextf(
								"you have %s taxa in the report, those will be aggregated",
								length(unique(taxa))
						))
			if (length(unique(stage)) > 1)
				warning(gettextf(
								"you have %s stages in the report, those will be aggregated",
								length(unique(stage))
						))
			plotdata <-
					dplyr::select(grdata, debut_pas, DC, effectif_total) %>% dplyr::rename(date =
									debut_pas) %>%
					dplyr::group_by(date, DC) %>% dplyr::summarize(effectif = sum(effectif_total)) %>%
					dplyr::ungroup()
			
			#######################
			# color scheme for station
			#######################
			cs <-
					colortable(color = color_station[names(color_station)%in%tableauCE$stm_libelle],
							vec = unique(tableauCE$stm_libelle),
							palette = "Accent")
			cs <- stacomirtools::chnames(cs, "name", "stm_libelle")
			#######################
			# color scheme for dc
			#######################
			cdc <-
					colortable(color = color_dc[names(color_dc)%in%dc],
							vec = dc,
							color_function = "gray.colors")
			cdc <- stacomirtools::chnames(cdc, "name", "DC")
			#######################
			# merging with colors for manual scales
			######################
			plotdata <- killfactor(merge(plotdata, cdc, by = "DC"))
			tableauCEquan <-
					killfactor(merge(tableauCEquan, cs, by = "stm_libelle"))
			tableauCEqual <-
					killfactor(merge(tableauCEqual, cs, by = "stm_libelle"))
			######################
			#  source of data
			#######################
			source <- paste("source:",paste(unique(tableauCE$source), collapse=", "))
			
			
			g <- ggplot(plotdata) +
					geom_bar(aes(x = date, y = effectif, fill = color),
							position = "stack",
							stat = "identity") +
					ylab(ylegend) +
					geom_line(aes(x = date, y = valeur, colour = color),
							data = tableauCEquan,
							size = 1) +
					geom_point(
							aes(
									x = date,
									shape = valeur,
									colour = color
							),
							y = yqualitatif,
							data = tableauCEqual,
							size = 3
					) +
					scale_fill_identity(name = gettext("DC"),
							labels = dc_code,
							guide = "legend") +
					scale_colour_identity(
							name = gettext("stations"),
							labels = cs[, "stm_libelle"],
							breaks = cs[, "color"],
							guide = "legend"
					) +
					scale_shape(guide = "legend", name = gettext("Qualitative parm")) +
					theme_bw() +
					labs(caption=source)
			print(g)
			assign("g", g, envir_stacomi)
			if (!silent)
				funout(
						gettext(
								"the ggplot object has been assigned to envir_stacomi, type g<-get('g',envir_stacomi)"
						)
				)
			return(invisible(NULL))
		}
)# end function
