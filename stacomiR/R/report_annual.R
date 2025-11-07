#' Class "report_annual"
#'
#' This class displays annual migration counts, for several counting device, taxa or stages.
#' @include ref_dc.R
#' @include ref_taxa.R
#' @include ref_stage.R
#' @include ref_year.R
#' @slot dc Object of class \code{\link{ref_dc-class}}, the counting device, multiple values allowed
#' @slot data Object of class \code{"data.frame"} data for report lot
#' @slot taxa An object of class \code{\link{ref_taxa-class}}, multiple values allowed
#' @slot stage An object of class \code{\link{ref_stage-class}}, multiple values allowed
#' @slot start_year Object of class \code{\link{ref_year-class}}. ref_year allows to choose year of beginning
#' @slot end_year Object of class \code{\link{ref_year-class}}
#' ref_year allows to choose last year of the report
#' @aliases report_annual
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @family report Objects
#' @keywords classes
#' @example inst/examples/report_annual-example.R
#' @export
setClass(
		Class = "report_annual",
		representation =
				representation(
						dc = "ref_dc",
						taxa = "ref_taxa",
						stage = "ref_stage",
						data = "data.frame",
						start_year = "ref_year",
						end_year = "ref_year"
				),
		prototype = prototype(
				dc = new("ref_dc"),
				taxa = new("ref_taxa"),
				stage = new("ref_stage"),
				data = data.frame(),
				start_year = new("ref_year"),
				end_year = new("ref_year")
		)
)


#' charge method for report_annual class
#'
#' Method used by the graphical interface to load data and check that all choices have
#' been made by the user
#' @param object An object of class \link{report_annual-class}
#' @param silent Stops messages from being displayed if silent=TRUE, default FALSE
#' @aliases charge.report_annual
#' @return object An object of class \link{report_annual-class} with data set from values assigned in \code{envir_stacomi} environment
#' @keywords internal
setMethod(
		"charge",
		signature = signature("report_annual"),
		definition = function(object, silent = FALSE) {
			r_ann <- object
			if (exists("ref_dc", envir_stacomi)) {
				r_ann@dc <- get("ref_dc", envir_stacomi)
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
				r_ann@taxa <- get("ref_taxa", envir_stacomi)
			} else {
				funout(
						gettext("You need to choose a taxa, clic on validate\n", domain = "R-stacomiR"),
						arret = TRUE
				)
			}
			if (exists("ref_stage", envir_stacomi)) {
				r_ann@stage <- get("ref_stage", envir_stacomi)
			} else
			{
				funout(
						gettext("You need to choose a stage, clic on validate\n", domain = "R-stacomiR"),
						arret = TRUE
				)
			}
			if (exists("start_year", envir_stacomi)) {
				r_ann@start_year <- get("start_year", envir_stacomi)
			} else {
				funout(gettext("You need to choose the starting year\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			if (exists("end_year", envir_stacomi)) {
				r_ann@end_year <- get("end_year", envir_stacomi)
			} else {
				funout(gettext("You need to choose the ending year\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			assign("report_annual", r_ann, envir_stacomi)
			funout(
					gettext(
							"The object report_annual is stored in the stacomi environment, type r_ann <-get('report_annual',envir_stacomi)",
							domain = "R-stacomiR"
					)
			)
			return(r_ann)
			
			
		}
)


#' connect method for report_annual class
#' this method performs the sum over the year attention this function does
#' not count subsamples.
#' @param object An object of class \link{report_annual-class}
#' @param silent Stops messages from being displayed if silent=TRUE, default FALSE
#' @return An instantiated object with values filled with user choice
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @return An object of class \link{report_annual-class} including a dataframe with column effectif, comprising the sum of report_mig counts
#' @importFrom dplyr anti_join arrange bind_rows
#' @aliases connect.report_annual
setMethod(
		"connect",
		signature = signature("report_annual"),
		definition = function(object, silent = FALSE)
		{
			r_ann <- object
			req = new("RequeteDB")
			##############################
			##############################"
			start_year =	r_ann@start_year@year_selected
			end_year = r_ann@end_year@year_selected
			dc = vector_to_listsql(r_ann@dc@dc_selected)
			tax = vector_to_listsql(r_ann@taxa@taxa_selected)
			std = vector_to_listsql(r_ann@stage@stage_selected)
			
			reqdiff = new("RequeteDB")
			
			reqdiff@sql = paste(
					"select *, extract(year  from ope_date_debut) as annee_debut, extract(year  from ope_date_fin) as annee_fin	FROM ",
					get_schema(),
					"t_operation_ope  join ",
					get_schema(),
					"t_lot_lot on lot_ope_identifiant=ope_identifiant
							where ope_dic_identifiant in ",
					dc,
					" and extract(year from ope_date_fin)>=",
					start_year,
					" and	 extract(year from ope_date_debut)<=",
					end_year,
					" and ope_dic_identifiant in ",
					dc,
					" and lot_tax_code in ",
					tax,
					" and lot_std_code in ",
					std,
					" and lot_lot_identifiant is null
							order by ope_dic_identifiant, annee_debut,annee_fin; ",
					sep = ""
			)
			reqdiff@sql <-
					stringr::str_replace_all(reqdiff@sql, "[\r\n\t]" , " ")
			reqdiff <- stacomirtools::query(reqdiff)
			detailed_data <- stacomirtools::getquery(reqdiff)
			# If there are some operations with year of date_debut different to the year of date_fin we need to find these operations
			# and apply on it the overlaps function to separate fish that arrive during the first year from the rest
			#If we don't have operation on two years we apply the simple sum per year
			annee_differentes <-
					detailed_data$annee_debut != detailed_data$annee_fin
			if (any(annee_differentes)) {
				data_to_cut <- detailed_data[annee_differentes, ]
				data_not_to_cut <- detailed_data[!annee_differentes, ]
				# vector of years of cut
				round_years <-
						lubridate::floor_date(data_to_cut$ope_date_debut, "years") + lubridate::years(1)
				end_of_the_year = difftime(round_years, data_to_cut$ope_date_debut, units =
								"days")
				beginning_of_the_year = difftime(data_to_cut$ope_date_fin, round_years, units =
								"day")
				operation_duration = difftime(data_to_cut$ope_date_fin,
						data_to_cut$ope_date_debut,
						units = "day")
				data_beginning_of_the_year <- data_to_cut
				data_beginning_of_the_year$lot_effectif <-
						data_beginning_of_the_year$lot_effectif *
						as.numeric(beginning_of_the_year) / as.numeric(operation_duration)
				data_beginning_of_the_year$ope_date_debut <- round_years
				data_beginning_of_the_year$annee_debut <-
						lubridate::year(round_years)
				data_end_of_the_year <- data_to_cut
				data_end_of_the_year$lot_effectif <-
						data_end_of_the_year$lot_effectif *
						as.numeric(end_of_the_year) / as.numeric(operation_duration)
				data_end_of_the_year$ope_date_fin <- round_years
				final_data <-
						rbind(data_not_to_cut,
								data_beginning_of_the_year,
								data_end_of_the_year)
				
				con <- new("ConnectionDB")
				con <- connect(con)
				on.exit(pool::poolClose(con@connection))
				pool::dbWriteTable(con@connection, 
						name = "final_data", 
						value=final_data, 
						temporary=TRUE)	
				r_ann@data <- pool::dbGetQuery(con@connection,
						" select sum(lot_effectif) as effectif, annee_debut as annee,
								ope_dic_identifiant,
								lot_tax_code,
								lot_std_code
								from
								final_data
								group by annee, ope_dic_identifiant, lot_tax_code, lot_std_code
								order by ope_dic_identifiant, annee, lot_tax_code, lot_std_code; ")					
				
			}
			#If we have dc and years with no difference in the years of start and end for the same operation we calculate the "classical" sum by year
			else {
				req@sql = paste(
						" select sum(lot_effectif) as effectif, annee, ope_dic_identifiant,lot_tax_code, lot_std_code  from
								(select *, extract(year  from ope_date_debut) as annee FROM ",
						get_schema(),
						"t_operation_ope ",
						" join ",
						get_schema(),
						"t_lot_lot on lot_ope_identifiant=ope_identifiant where ope_dic_identifiant in",
						dc,
						" and extract(year from ope_date_debut)>=",
						start_year,
						" and extract(year from ope_date_fin)<=",
						end_year,
						" and ope_dic_identifiant in ",
						dc,
						" and lot_tax_code in ",
						tax,
						" and lot_std_code in ",
						std,
						" and lot_lot_identifiant is null) as tmp",
						" group by annee, ope_dic_identifiant, lot_tax_code, lot_std_code ",
						" order by ope_dic_identifiant, annee, lot_tax_code, lot_std_code; ",
						sep = ""
				)
				req@sql <- stringr::str_replace_all(req@sql, "[\r\n\t]" , "")
				req <- stacomirtools::query(req)
				resdata <- getquery(req)

				all_comb <- expand.grid(
						annee = start_year:end_year,
						ope_dic_identifiant = r_ann@dc@dc_selected,
				    lot_tax_code = r_ann@taxa@taxa_selected,
						lot_std_code =  r_ann@stage@stage_selected
						)
				missing <- dplyr::anti_join(all_comb,resdata[,c("annee", "ope_dic_identifiant", "lot_tax_code", 
										"lot_std_code")], by = c("annee", "ope_dic_identifiant", "lot_tax_code", "lot_std_code"))
				if (nrow(missing) > 0){
				missing$effectif = 0
				r_ann@data <- dplyr::bind_rows(resdata,missing)
				} else {
					r_ann@data <-	resdata
				}
				r_ann@data <- dplyr::arrange(r_ann@data,ope_dic_identifiant, lot_tax_code, lot_std_code, annee)		
			}
			return(r_ann)
		}
)

#' command line interface for \link{report_annual-class}
#' 
#' The choice_c method fills in the data slot for classes \link{ref_dc-class}, \link{ref_taxa-class}, 
#' \link{ref_stage-class} and two slots of \link{ref_year-class}
#' @param object An object of class \link{report_annual-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,ref_dc-method}
#' @param taxa Either a species name in latin or the SANDRE code for species (ie 2038=Anguilla anguilla),
#' it should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,ref_taxa-method}
#' @param stage A stage code matching the ref.tr_stadedeveloppement_std table in the stacomi database, see \link{choice_c,ref_stage-method}
#' @param start_year The starting the first year, passed as character or integer
#' @param end_year the finishing year
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class \link{report_annual-class} with data selected
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases choice_c.report_annual
setMethod(
		"choice_c",
		signature = signature("report_annual"),
		definition = function(object,
				dc,
				taxa,
				stage,
				start_year,
				end_year,
				silent = FALSE) {
			# code for debug using example
			#dc=c(5,6);taxa="Anguilla anguilla";stage=c("AGJ","AGG","CIV");start_year="1996";end_year="2016"
			r_ann <- object
			r_ann@dc = charge(r_ann@dc)
			# loads and verifies the dc
			# this will set dc_selected slot
			r_ann@dc <- choice_c(object = r_ann@dc, dc)
			# only taxa present in the report_mig are used
			r_ann@taxa <-
					charge_with_filter(object = r_ann@taxa, r_ann@dc@dc_selected)
			r_ann@taxa <- choice_c(r_ann@taxa, taxa)
			r_ann@stage <-
					charge_with_filter(object = r_ann@stage,
							r_ann@dc@dc_selected,
							r_ann@taxa@taxa_selected)
			r_ann@stage <- choice_c(r_ann@stage, stage)
			
			r_ann@start_year <- charge(object = r_ann@start_year,
					objectreport = "report_annual")
			r_ann@start_year <- choice_c(
					object = r_ann@start_year,
					nomassign = "start_year",
					annee = start_year,
					silent = silent
			)
			r_ann@end_year@data <- r_ann@start_year@data
			r_ann@end_year <- choice_c(
					object = r_ann@end_year,
					nomassign = "end_year",
					annee = end_year,
					silent = silent
			)
			assign("report_annual", r_ann, envir = envir_stacomi)
			return(r_ann)
		}
)

#' xtable function for \link{report_annual-class}
#' create an xtable objet but also assigns an add.to.column argument in envir_stacomi,
#' for later use by the print.xtable method.
#' @param x, an object of class "report_annual"
#' @param caption, see xtable
#' @param label, see xtable
#' @param align, see xtable, overidden if NULL
#' @param digits default 0
#' @param display see xtable
#' @param auto see xtable
#' @param dc_name A string indicating the names of the DC, in the order of  \code{x@dc@dc_selected}
#' if not provided DC codes are used.
#' @param tax_name A string indicating the names of the taxa, if not provided latin names are used
#' @param std_name A string indicating the stages names, if not provided then std_libelle are used
#' @return A xtable for annual report
#' @aliases xtable.report_annual
#' @export
setMethod(
		"xtable",
		signature = signature("report_annual"),
		definition = function(x,
				caption = NULL,
				label = NULL,
				align = NULL,
				digits = 0,
				display = NULL,
				auto = FALSE,
				dc_name = NULL,
				tax_name = NULL,
				std_name = NULL) {
			r_ann <- x
			dat = r_ann@data
			tax = r_ann@taxa@taxa_selected
			std = r_ann@stage@stage_selected
			dc = r_ann@dc@dc_selected
			# giving names by default if NULL else checking that arguments dc_name, tax_name, std_name
			#have the right length
			if (is.null(dc_name)){
				dc_name = r_ann@dc@data$dc_code[r_ann@dc@data$dc %in% r_ann@dc@dc_selected]
			} 
			if (length(dc) != length(dc_name)) {
				stop (stringr::str_c("dc_name argument should have length ", length(dc)))
			}
			if (is.null(tax_name)){
				tax_name = r_ann@taxa@data$tax_nom_latin[r_ann@taxa@data$tax_code %in% r_ann@taxa@taxa_selected]
			} 
			if (length(tax) != length(tax_name)){
				stop (stringr::str_c("tax_name argument should have length ", length(tax)))
			}
			if (is.null(std_name)){
				std_name = r_ann@stage@data$std_libelle[r_ann@stage@data$std_code %in% r_ann@stage@stage_selected]
			}

			if (length(std) != length(std_name)){
				stop (stringr::str_c("std_name argument should have length ", length(std)))
			}
			
			dat <-
					dat[, c("annee",
									"effectif",
									"ope_dic_identifiant",
									"lot_tax_code",
									"lot_std_code")]
			dat <-
					reshape2::dcast(dat,
							annee ~ ope_dic_identifiant + lot_tax_code + lot_std_code,
							value.var = "effectif")
			coln <- colnames(dat)[2:length(colnames(dat))]
			# names header for DC
			# this function creates title as "multicolumn" arguments, repeated over columns if necessary
			# it will be passed later as add.to.row print.xtable command
			fn_title <- function(les_valeurs, valeur_uk, name_uk, total = TRUE) {
				which_arg <- match(les_valeurs, valeur_uk)
				if (length(les_valeurs) == 1) {
					repetes <- FALSE
				} else {
					repetes <-
							c(les_valeurs[2:length(les_valeurs)] == les_valeurs[1:(length(les_valeurs) -
														1)], FALSE) # FALSE, at the end we want the values aggregated anyway
				}
				rr = 1
				les_valeurs_final <- vector()
				for (i in 1:length(les_valeurs)) {
					# if the same argument is repeated over current value and the next
					if (repetes[i]) {
						rr <- rr + 1
					} else {
						# sortie de la boucle
						les_valeurs_final <-
								c(
										les_valeurs_final,
										stringr::str_c(
												"\\multicolumn{",
												rr,
												"}{c}{",
												xtable::sanitize(name_uk[which_arg[i]]),
												"}"
										)
								)
						rr = 1
					}
				}
				if (total) {
					les_valeurs_final <-
							stringr::str_c(" & ",
									stringr::str_c(les_valeurs_final, collapse = " & "),
									" & Total\\\\\n")
				} else {
					les_valeurs_final <-
							stringr::str_c(" & ",
									stringr::str_c(les_valeurs_final, collapse = " & "),
									" & \\\\\n")
				}
				return(les_valeurs_final)
			}
			les_dc <-
					unlist(lapply(stringr::str_split(coln, "_"), function(X)
										X[1]))
			les_dc <-
					fn_title(
							les_valeurs = les_dc,
							valeur_uk = dc,
							name_uk = dc_name,
							total = FALSE
					)
			
			#header for tax
			les_tax <-
					unlist(lapply(stringr::str_split(coln, "_"), function(X)
										X[2]))
			les_tax <-
					fn_title(
							les_valeurs = les_tax,
							valeur_uk = tax,
							name_uk = tax_name,
							total = FALSE
					)
			# name header for std
			les_std <-
					unlist(lapply(stringr::str_split(coln, "_"), function(X)
										X[3]))
			les_std <-
					fn_title(
							les_valeurs = les_std,
							valeur_uk = std,
							name_uk = std_name,
							total = TRUE
					)
			# remove annee (it is now only rownames)
			rownames(dat) <- dat$annee
			dat <- dat[, -1, FALSE]
			# calculating sum
			if (ncol(dat) > 1)
				dat$sum <- rowSums(dat[, 1:ncol(dat)], na.rm = TRUE)
			
			
			if (is.null(align))
				align <- c("l", rep("r", ncol(dat)))
			if (is.null(display))
				display = c("s", rep("f", ncol(dat)))
			xt <- xtable::xtable(
					dat,
					caption = caption,
					label = label,
					align = align,
					digits = 0,
					display = display,
					# integer,small scientific if it saves place, string..
					auto = auto
			)
			addtorow <- list()
			addtorow$pos <- list()
			addtorow$pos[[1]] <- 0
			addtorow$pos[[2]] <- 0
			addtorow$pos[[3]] <- 0
			addtorow$pos[[4]] <- 0
			addtorow$pos[[5]] <- 0
			addtorow$command <-
					c(les_dc, "\\hline\n", les_tax , "\\hline\n", les_std)
			assign("addtorow", addtorow, envir_stacomi)
			return(xt)
		}
)


#' barplot method for object \link{report_annual-class}
#' @param height An object of class report_annual
#' @param legend.text See barplot help
#' @param ... additional arguments passed to barplot
#' @return No return value, called for side effects
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases barplot.report_annual
#' @seealso \link{report_annual-class} for examples
#' @export
setMethod(
		"barplot",
		signature(height = "report_annual"),
		definition = function(height, legend.text = NULL, ...) {
			r_ann <- height
			# require(ggplot2)
			if (nrow(r_ann@data) > 0) {
				dat = r_ann@data
				lesdic <- unique(dat$ope_dic_identifiant)
				lestax <- unique(dat$lot_tax_code)
				lesstd <- unique(dat$lot_std_code)
				
				# create a matrix of each dc, taxa, stage
				if (length(lestax) == 1 & length(lesstd) & length(lesdic) == 1) {
					dat0 <-
							reshape2::dcast(dat, lot_tax_code ~ annee, value.var = "effectif")
					mat <- as.matrix(dat0[, 2:ncol(dat0)])
					mat[is.na(mat)] <- 0
					barplot(mat, ...)
					
				} else if (length(lestax) == 1 & length(lesstd) == 1) {
					dat0 <-
							reshape2::dcast(dat, ope_dic_identifiant ~ annee, value.var = "effectif")
					mat <- as.matrix(dat0[, 2:ncol(dat0)])
					mat[is.na(mat)] <- 0
					if (is.null(legend.text)) {
						legend.text = dat0$ope_dic_identifiant
						barplot(mat, legend.text = legend.text, ...)
					} else {
						barplot(mat, legend.text=legend.text)
					}
					
				} else if (length(lestax) == 1 & length(lesdic) == 1) {
					dat0 <-
							reshape2::dcast(dat, lot_std_code ~ annee, value.var = "effectif")
					mat <- as.matrix(dat0[, 2:ncol(dat0)])
					mat[is.na(mat)] <- 0
					if (is.null(legend.text)) {
						legend.text = dat0$lot_std_code
						barplot(mat, legend.text = legend.text, ...)
					} else {
						barplot(mat, ...)
					}
					
				} else if (length(lesdic) == 1 & length(lesstd) == 1) {
					dat0 <-
							reshape2::dcast(dat, lot_tax_code ~ annee, value.var = "effectif")
					mat <- as.matrix(dat0[, 2:ncol(dat0)])
					mat[is.na(mat)] <- 0
					if (is.null(legend.text)) {
						legend.text <- dat0$lot_tax_code
						barplot(mat, legend.text = legend.text, ...)
					} else {
						barplot(mat, ...)
					}
					
				} else if (length(lestax) == 1) {
					dat0 <-
							reshape2::dcast(dat,
									ope_dic_identifiant + lot_std_code ~ annee,
									value.var = "effectif")
					mat <- as.matrix(dat0[, 3:ncol(dat0)])
					mat[is.na(mat)] <- 0
					if (is.null(legend.text)) {
						legend.text <-
								stringr::str_c(dat0$ope_dic_identifiant, "_", dat0$lot_std_code)
						barplot(mat, legend.text = legend.text, ...)
					} else {
						barplot(mat, ...)
					}
					
				} else if (length(lesstd) == 1) {
					dat0 <-
							reshape2::dcast(dat,
									ope_dic_identifiant + lot_tax_code ~ annee,
									value.var = "effectif")
					mat <- as.matrix(dat0[, 3:ncol(dat0)])
					mat[is.na(mat)] <- 0
					if (is.null(legend.text)) {
						legend.text <-
								stringr::str_c(dat0$ope_dic_identifiant, "_", dat0$lot_tax_code)
						barplot(mat, legend.text = legend.text, ...)
					} else {
						barplot(mat, ...)
					}
				} else if (length(lesdic) == 1) {
					dat0 <-
							reshape2::dcast(dat, lot_std_code + lot_tax_code ~ annee, value.var = "effectif")
					mat <- as.matrix(dat0[, 3:ncol(dat0)])
					mat[is.na(mat)] <- 0
					if (is.null(legend.text)) {
						legend.text <- stringr::str_c(dat0$lot_tax_code, "_", dat0$lot_std_code)
						barplot(mat, legend.text = legend.text, ...)
					} else {
						barplot(mat, ...)
					}
					
				} else {
					dat0 <-
							reshape2::dcast(dat,
									ope_dic_identifiant + lot_tax_code + lot_std_code ~ annee,
									value.var = "effectif")
					mat <- as.matrix(dat0[, 4:ncol(dat0)])
					mat[is.na(mat)] <- 0
					if (is.null(legend.text)) {
						legend.text <- stringr::str_c(dat0$ope_dic_identifiant,
								"_",
								dat0$lot_tax_code,
								"_",
								dat0$lot_std_code)
						barplot(mat, legend.text = legend.text, ...)
					} else {
						barplot(mat, ...)
					}
				}
			}    else     {
				funout(gettext("No data", domain = "R-stacomiR"))
			}
			return(invisible(NULL))
		}
)



#' Plot method for report_annual
#'
#' @param x An object of class \link{report_annual-class}
#' @param plot.type Default point
#' @param silent Stops displaying the messages.
#' \itemize{
#' 		\item{plot.type="point": ggplot+geom_point}'
#' }
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases plot.report_annual
#' @seealso \link{report_mig_interannual-class} for examples
#' @return No return value, called for side effects
#' @importFrom scales breaks_pretty
#' @export
setMethod(
		"plot",
		signature(x = "report_annual", y = "missing"),
		definition = function(x,
				plot.type = "point",
				silent = FALSE) {
			r_ann <- x
			dat <- r_ann@data
			lesdic <- unique(dat$ope_dic_identifiant)
			lestax <- unique(dat$lot_tax_code)
			lesstd <- unique(dat$lot_std_code)
			if (nrow(r_ann@data) > 0) {
				if (plot.type == "point") {
					colnames(dat) <- c("effectif", "annee", "dc", "taxa", "stage")
					dat$dc <- as.factor(dat$dc)
					dat$taxa <- as.factor(dat$taxa)
					if (length(lestax) == 1 & length(lesstd) & length(lesdic) == 1) {
						# note below the scale is made to avoid 2000.5 2001 ... and too much breaks as well 
						# see #27
						g <- ggplot(dat, aes(x = annee, y = effectif)) + geom_point() +
								geom_line() +
								scale_x_continuous(breaks = scales::breaks_pretty(n=pmin(length(unique(dat$annee)),10))) +
								theme_bw()
						print(g)
						assign("g", g, envir_stacomi)
						if (!silent)
							funout(
									gettext(
											"Writing the graphical object into envir_stacomi environment : write g=get('g',envir_stacomi)\n",
											domain = "R-stacomiR"
									)
							)
						
					} else if (length(lestax) == 1 & length(lesstd) == 1) {
						g <- ggplot(dat, aes(x = annee, y = effectif)) +
								geom_point(aes(col = dc)) +
								geom_line(aes(col = dc)) +
								scale_x_continuous(breaks = scales::breaks_pretty(n=pmin(length(unique(dat$annee)),10))) +
								theme_bw()
						print(g)
						assign("g", g, envir_stacomi)
						if (!silent)
							funout(
									gettext(
											"Writing the graphical object into envir_stacomi environment : write g=get('g',envir_stacomi)\n",
											domain = "R-stacomiR"
									)
							)
						
					} else if (length(lestax) == 1 & length(lesdic) == 1) {
						g <- ggplot(dat, aes(x = annee, y = effectif)) + geom_point(aes(col = stage)) +
								geom_line(aes(col = stage)) +
								scale_x_continuous(breaks = scales::breaks_pretty(n=pmin(length(unique(dat$annee)),10))) +
								theme_bw()
						print(g)
						assign("g", g, envir_stacomi)
						if (!silent)
							funout(
									gettext(
											"Writing the graphical object into envir_stacomi environment : write g=get('g',envir_stacomi)\n",
											domain = "R-stacomiR"
									)
							)
						
					} else if (length(lesdic) == 1 & length(lesstd) == 1) {
						g <- ggplot(dat, aes(x = annee, y = effectif)) + geom_point(aes(col = taxa)) +
								geom_line(aes(col = taxa)) +
								scale_x_continuous(breaks = scales::breaks_pretty(n=pmin(length(unique(dat$annee)),10))) +
								theme_bw()
						print(g)
						assign("g", g, envir_stacomi)
						if (!silent)
							funout(
									gettext(
											"Writing the graphical object into envir_stacomi environment : write g=get('g',envir_stacomi)\n",
											domain = "R-stacomiR"
									)
							)
						
						
					} else if (length(lestax) == 1) {
						g <-
								ggplot(dat, aes(x = annee, y = effectif)) + geom_point(aes(col = dc, shape =
														stage)) +
								geom_line(aes(col = dc, linetype = stage)) +
								scale_x_continuous(breaks = scales::breaks_pretty(n=pmin(length(unique(dat$annee)),10))) +
								theme_bw()
						print(g)
						assign("g", g, envir_stacomi)
						if (!silent)
							funout(
									gettext(
											"Writing the graphical object into envir_stacomi environment : write g=get('g',envir_stacomi)\n",
											domain = "R-stacomiR"
									)
							)
						
					} else if (length(lesstd) == 1) {
						g <-
								ggplot(dat, aes(x = annee, y = effectif)) + geom_point(aes(col = dc, shape =
														taxa)) +
								geom_line(aes(col = dc, shape = taxa)) +
								scale_x_continuous(breaks = scales::breaks_pretty(n=pmin(length(unique(dat$annee)),10))) +
								theme_bw()
						print(g)
						assign("g", g, envir_stacomi)
						if (!silent)
							funout(
									gettext(
											"Writing the graphical object into envir_stacomi environment : write g=get('g',envir_stacomi)\n",
											domain = "R-stacomiR"
									)
							)
						
					} else if (length(lesdic) == 1) {
						g <-
								ggplot(dat, aes(x = annee, y = effectif)) + geom_point(aes(col = taxa, shape =
														stage)) +
								geom_line(aes(col = taxa, shape = stage)) +
								scale_x_continuous(breaks = scales::breaks_pretty(n=pmin(length(unique(dat$annee)),10))) +
								theme_bw()
						print(g)
						assign("g", g, envir_stacomi)
						if (!silent)
							funout(
									gettext(
											"Writing the graphical object into envir_stacomi environment : write g=get('g',envir_stacomi)\n",
											domain = "R-stacomiR"
									)
							)
						
					} else {
						if (length(lesdic) < 3) {
							g <-
									ggplot(dat, aes(x = annee, y = effectif)) + geom_point(aes(col = taxa, shape =
															stage)) +
									geom_line(aes(col = taxa, shape = stage)) +
									scale_x_continuous(breaks = scales::breaks_pretty(n=pmin(length(unique(dat$annee)),10))) +
									facet_wrap( ~ dc) +
									theme_bw()
							print(g)
							assign("g", g, envir_stacomi)
						} else {
							g <- ggplot(dat, aes(x = annee, y = effectif)) + geom_point(aes(col = stage)) +
									geom_line(aes(col = stage)) +
									facet_grid(dc ~ stage) +
									scale_x_continuous(breaks = scales::breaks_pretty(n=pmin(length(unique(dat$annee)),10))) +
									theme_bw()
							print(g)
							
							assign("g", g, envir_stacomi)
							if (!silent)
								funout(
										gettext(
												"Writing the graphical object into envir_stacomi environment : write g=get('g',envir_stacomi)\n",
												domain = "R-stacomiR"
										)
								)
						}
					}
				}
				
			}    else     {
				funout(gettext("No data", domain = "R-stacomiR"))
			}
			return(invisible(NULL))
		}

)

