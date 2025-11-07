

#' Class "report_dc" report du fonctionnement du dispositif de
#' comptage
#'
#' The counting device is not always working. It may me stopped either
#' following a monitoring protocol, or due to malfunction of the device, this
#' class allows to draw graphics allowing an overview of the device operation
#' @slot data A data frame
#' @slot dc An object of class \code{ref_dc-class}
#' @slot horodatedebut An object of class \code{ref_horodate-class}
#' @slot horodatefin An object of class \code{ref_horodate-class}
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("report_dc", ...)}.
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @example inst/examples/report_dc-example.R
#' @family report Objects
#' @keywords classes
#' @aliases report_dc
#' @export
setClass(
		Class = "report_dc",
		representation = representation(
				data = "data.frame",
				dc = "ref_dc",
				horodatedebut = "ref_horodate",
				horodatefin = "ref_horodate"
		),
		prototype = prototype(
				data = data.frame(),
				dc = new("ref_dc"),
				horodatedebut = new("ref_horodate"),
				horodatefin = new("ref_horodate")
		)
)




#' connect method for report_dc
#'
#' loads the working periods and type of arrest or disfunction of the DC
#' @param object An object of class \link{report_dc-class}
#' @param silent boolean, default FALSE, if TRUE messages are not displayed
#' @return  An object of class \link{report_dc-class} with slot data filled from the database
#' @aliases connect.report_dc
#' @author cedric.briand
setMethod(
		"connect",
		signature = signature("report_dc"),
		definition = function(object, silent = FALSE) {
			#object<-report_dc
			if (length(object@dc@dc_selected)==0) stop("No selected dc in repor_dc@dc@dc_selected, did you forget to use the method charge ?")
			req <- new("RequeteDBwheredate")
			req@select = sql <- paste(
					"SELECT",
					" per_dis_identifiant,",
					" per_date_debut,",
					" per_date_fin,",
					" per_commentaires,",
					" per_etat_fonctionnement,",
					" per_tar_code,",
					" tar_libelle AS libelle",
					" FROM  ",
					get_schema(),
					"t_periodefonctdispositif_per per",
					" INNER JOIN ref.tr_typearretdisp_tar tar ON tar.tar_code=per.per_tar_code",
					sep = ""
			)
			req@colonnedebut <- "per_date_debut"
			req@colonnefin <- "per_date_fin"
			req@datedebut <- object@horodatedebut@horodate
			req@datefin <- object@horodatefin@horodate
			req@order_by <- "ORDER BY per_date_debut"
			req@and <-
					paste("AND per_dis_identifiant in ",
							vector_to_listsql(object@dc@dc_selected))
			#req@where=#defini dans la methode DBwheredate
			req <-
					stacomirtools::query(req) # appel de la methode connect de l'object DBWHEREDATE
			object@data <- req@query
			if (!silent)
				funout(gettext("Time steps loaded for this counting device\n", domain =
										"R-stacomiR"))
			return(object)
		}
)

#' charge method for report_dc
#'
#' used by the graphical interface to retrieve the objects of referential classes
#' assigned to envir_stacomi
#' @param object An object of class \link{report_dc-class}
#' @param silent boolean, default FALSE, if TRUE messages are not displayed.
#' @aliases charge.report_dc
#' @return object An object of class \link{report_dc-class} with data set from values assigned in \code{envir_stacomi} environment

#' @keywords internal
setMethod(
		"charge",
		signature = signature("report_dc"),
		definition = function(object, silent = FALSE) {
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
			
			if (exists("report_dc_date_debut", envir_stacomi)) {
				object@horodatedebut@horodate <-
						get("report_dc_date_debut", envir_stacomi)
			} else {
				funout(gettext("You need to choose the starting date\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			
			if (exists("report_dc_date_fin", envir_stacomi)) {
				object@horodatefin@horodate <- get("report_dc_date_fin", envir_stacomi)
			} else {
				funout(gettext("You need to choose the ending date\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			return(object)
		}
)


#' command line interface for report_dc class
#'
#' The choice_c method fills in the data slot for ref_dc, and then
#' uses the choice_c methods of these object to "select" the data.
#' @param object An object of class \link{ref_dc-class}
#' @param dc The dc to set
#' @param horodatedebut A POSIXt or Date or character to fix the date of beginning of the report
#' @param horodatefin A POSIXt or Date or character to fix the last date of the report
#' @param silent Should program be silent or display messages
#' @aliases choice_c.report_dc
#' @return An object of class \link{ref_dc-class} with data selected
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @export
setMethod(
		"choice_c",
		signature = signature("report_dc"),
		definition = function(object,
				dc,
				horodatedebut,
				horodatefin,
				silent = FALSE) {
			# report_dc<-r_dc;dc=5;horodatedebut="2000-01-01";horodatefin="2015-12-31";silent=TRUE
			report_dc <- object
			assign("report_dc", report_dc, envir = envir_stacomi)
			if (!silent)
				funout(
						gettext(
								"Loading of the list for fishways and choice of the time step\n",
								domain = "R-stacomiR"
						)
				)
			report_dc@dc <- charge(report_dc@dc)
			report_dc@dc <- choice_c(report_dc@dc, dc)
			# assigns the parameter (horodatedebut) of the method to the object using choice_c method for ref_dc
			report_dc@horodatedebut <- choice_c(
					object = report_dc@horodatedebut,
					nomassign = "report_dc_date_debut",
					funoutlabel = gettext("Beginning date has been chosen\n", domain = "R-stacomiR"),
					horodate = horodatedebut,
					silent = silent
			)
			report_dc@horodatefin <- choice_c(
					report_dc@horodatefin,
					nomassign = "report_dc_date_fin",
					funoutlabel = gettext("Ending date has been chosen\n", domain = "R-stacomiR"),
					horodate = horodatefin,
					silent = silent
			)
			assign("report_dc", report_dc, envir = envir_stacomi)
			return(report_dc)
		}
)

#' Different plots for report_dc
#'
#' \describe{
#' \item{plot.type=1}{A barplot of the operation time per month}
#' \item{plot.type=2}{Barchat giving the time per type of operation }
#' \item{plot.type=2}{Rectangle plots drawn along a line}
#' \item{plot.type=4}{Plots per day drawn over the period to show the operation of a df, days in x, hours in y}
#' 	}
#'
#' @note The program cuts periods which overlap between two month.
#' The splitting of different periods into month is
#' assigned to the \code{envir_stacomi} environment.
#' @param x An object of class \link{report_dc-class}.
#' @param plot.type 1 to 4, barplot, barchart, rectangle plot and box showing details of daily operation,
#' a plot with adjacent rectangles.
#' @param silent Stops displaying the messages default to FALSE 
#' @param main The title of the graph, if NULL a default title will be plotted
#' with the number of the DF.
#' @param color_type_oper Named vector of color for the graph, must match type operation default to 
#' c("Fonc normal" = "#76BEBE",
#'  "Arr ponctuel" = "#FF6700",
#'  "Arr maint" = "#9E0142",
#'  "Dysfonc" = "#EE1874",
#'  "Non connu" = "#999999").
#' @param color_etat Named vector state value (must match the names "TRUE", "FALSE").
#' @return Nothing but prints the different plots.
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases plot.report_dc
#' @importFrom utils setTxtProgressBar
#' @export
setMethod(
		"plot",
		signature(x = "report_dc", y = "missing"),
		definition =
				function(x,
						plot.type = 1,
						silent = FALSE,
						main = NULL,
						color_type_oper = 	c("Fonc normal" = "#76BEBE",
						                     "Arr ponctuel" = "#FF6700", 
						                     "Arr maint" = "#9E0142",
						                     "Dysfonc" = "#EE1874",
						                     "Non connu" = "#999999"),
						color_etat=c("TRUE"="#0F313A","FALSE"="#CEB99A")) {
			#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			#           PLOT OF TYPE BARCHART (plot.type=1 (true/false) or plot.type=2)
			#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			#report_dc<-r_dc; require(RGtk2); require(lubridate);require(ggplot2);main=NULL;silent=FALSE;plot.type="1"
			
			report_dc <- x
			plot.type <- as.character(plot.type)# to pass also characters
			if (!plot.type %in% c("1", "2", "3", "4"))
				stop('plot.type must be 1,2,3 or 4')
			if (nrow(report_dc@data) == 0)
				funout(gettext("No data for this counting device\n", domain = "R-stacomiR"),
						arret = TRUE)
			if (plot.type == "1" | plot.type == "2") {
				t_periodefonctdispositif_per = report_dc@data # on recupere le data.frame
				tempsdebut <- t_periodefonctdispositif_per$per_date_debut
				tempsfin <- t_periodefonctdispositif_per$per_date_fin
				tempsdebut[tempsdebut < report_dc@horodatedebut@horodate] <-
						report_dc@horodatedebut@horodate
				tempsfin[tempsfin > report_dc@horodatefin@horodate] <-
						report_dc@horodatefin@horodate
				t_periodefonctdispositif_per = cbind(t_periodefonctdispositif_per, tempsdebut, tempsfin)
				seqmois = seq(
						from = tempsdebut[1],
						to = tempsfin[nrow(t_periodefonctdispositif_per)],
						by = "month",
						tz = "GMT"
				)
				seqmois = as.POSIXlt(round_date(seqmois, unit = "month"))
				# adding one month at the end to get a complete coverage of the final month
				seqmois <- c(seqmois,
						seqmois[length(seqmois)] %m+% months(1))
				t_periodefonctdispositif_per_mois = t_periodefonctdispositif_per[1, ]
				############################
				# progress bar
				###########################
				
				progress_bar <- utils::txtProgressBar()
				# this function assigns
				z = 0 # compteur tableau t_periodefonctdispositif_per_mois
				for (j in 1:nrow(t_periodefonctdispositif_per)) {
					#cat( j
					setTxtProgressBar(progress_bar,j / nrow(t_periodefonctdispositif_per))
					if (j > 1)
						t_periodefonctdispositif_per_mois = rbind(t_periodefonctdispositif_per_mois,
								t_periodefonctdispositif_per[j, ])
					lemoisnext = seqmois[seqmois > tempsdebut[j]][1] # le premier mois superieur a tempsdebut
					while (tempsfin[j] > lemoisnext) {
						# on est a cheval sur deux periodes
						
						#if (z>0) stop("erreur")
						z = z + 1
						t_periodefonctdispositif_per_mois = rbind(t_periodefonctdispositif_per_mois,
								t_periodefonctdispositif_per[j, ])
						t_periodefonctdispositif_per_mois[j + z, "tempsdebut"] = as.POSIXct(lemoisnext)
						t_periodefonctdispositif_per_mois[j + z - 1, "tempsfin"] = as.POSIXct(lemoisnext)
						lemoisnext = seqmois[match(as.character(lemoisnext), as.character(seqmois)) +
										1] # on decale de 1 mois avant de rerentrer dans la boucle
						#if (is.na(lemoisnext) ) break
					}
					#if (is.na(lemoisnext)) break
				}
				t_periodefonctdispositif_per_mois$sumduree <-
						as.numeric(
								difftime(
										t_periodefonctdispositif_per_mois$tempsfin,
										t_periodefonctdispositif_per_mois$tempsdebut,
										units = "hours"
								)
						)
				t_periodefonctdispositif_per_mois$mois1 = strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),
						"%b")
				t_periodefonctdispositif_per_mois$mois = strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),
						"%m")
				t_periodefonctdispositif_per_mois$annee = strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),
						"%Y")
				cat("All done.\n")
				close(progress_bar)
				if (is.null(main))
					main <-
							gettextf("Operation of the counting device %s",
									report_dc@dc@dc_selected)
				
				# graphic
				#modification of the order
				
				t_periodefonctdispositif_per_mois = t_periodefonctdispositif_per_mois[order(t_periodefonctdispositif_per_mois$per_etat_fonctionnement,
								decreasing = TRUE), ]
				g <- ggplot(t_periodefonctdispositif_per_mois,
								aes(x = mois, y = sumduree, fill = libelle)) +
						facet_grid(annee ~ .) +
						ggtitle(main) +
						ylab(gettext("duration", domain = "R-stacomiR")) +
						xlab(gettext("month", domain = "R-stacomiR")) +
						geom_bar(stat = 'identity') +
						scale_fill_manual(
								gettext("type_oper.", domain = "R-stacomiR"),
								values = color_type_oper
						) +
						theme(
								plot.background = element_rect(fill = "white"),
								panel.background = element_rect(fill = "white"),
								legend.background = element_rect(fill = "white"),
								strip.background = element_rect(colour = "pink", fill = "brown"),
								strip.text = element_text(colour = "white"),
								panel.grid.major = element_blank(),
								panel.grid.minor = element_blank(),
								text = element_text(colour = "navyblue"),
								line = element_line(colour = "black"),
								legend.key = element_rect(fill = "white", colour = "black"),
								axis.text = element_text(colour = "black")
						)
				
				t_periodefonctdispositif_per_mois = t_periodefonctdispositif_per_mois[order(t_periodefonctdispositif_per_mois$per_etat_fonctionnement), ]
				t_periodefonctdispositif_per_mois$per_etat_fonctionnement = as.factor(t_periodefonctdispositif_per_mois$per_etat_fonctionnement)
				g1 <-
						ggplot(t_periodefonctdispositif_per_mois,
								aes(x = mois, y = sumduree)) +
						facet_grid(annee ~ .) +
						ggtitle(main) +
						ylab(gettext("duration", domain = "R-stacomiR")) +
						xlab(gettext("month", domain = "R-stacomiR")) +
						geom_bar(stat = 'identity', aes(fill = per_etat_fonctionnement)) +
						scale_fill_manual(gettext("operation", domain = "R-stacomiR"),
								values = color_etat) +
						theme(
								plot.background = element_rect(fill = "white"),
								panel.background = element_rect(fill = "white"),
								legend.background = element_rect(fill = "white"),
								strip.background = element_rect(colour = "#C07C44", fill = "#A07C68"),
								strip.text = element_text(colour = "#41DADE"),
								panel.grid.major = element_blank(),
								panel.grid.minor = element_blank(),
								text = element_text(colour = "#482E21"),
								line = element_line(colour = "black"),
								legend.key = element_rect(fill = "white", colour = "black"),
								axis.text = element_text(colour = "black")
						)
				
				if (plot.type == "1") {
					print(g)
					assign(x = "g_report_dc_1",
							value = g,
							envir = envir_stacomi)
					if (!silent){
						funout(text =
										gettext(
												"Writing the ggplot into envir_stacomi environment : g_report_dc_1=get('g_report_dc_1',envir_stacomi)\n",
												domain = "R-stacomiR"
										)
						)
					}
				} # end if plot 1
				if (plot.type == "2"){
					print(g1) 
					assign("g_report_dc_2",
							g1,
							envir = envir_stacomi)			
					if (!silent){
						funout(
								gettext(
										"Writing the ggplot into envir_stacomi environment : g_report_dc_2=get('g_report_dc_2',envir_stacomi)\n",
										domain = "R-stacomiR"
								)
						)
					}
				} # end if plot 2
				assign("periodeDC",
						t_periodefonctdispositif_per_mois,
						envir = envir_stacomi)
				if (!silent)
					funout(
							gettext(
									"Writing the table into envir_stacomi environment : write periodeDC=get('periodeDC',envir_stacomi)\n",
									domain = "R-stacomiR"
							)
					)
				#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
				#           PLOT OF TYPE BOX (plot.type=3)
				#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			} else if (plot.type == "3") {
				#report_dc<-r_dc; require(RGtk2); require(lubridate);require(ggplot2);main=NULL;silent=FALSE;plot.type="3"
				t_periodefonctdispositif_per = report_dc@data
				graphdate <- function(vectordate) {
					vectordate <- as.POSIXct(vectordate)
					attributes(vectordate) <- NULL
					unclass(vectordate)
					return(vectordate)
				}
				time.sequence = seq.POSIXt(
						from = report_dc@horodatedebut@horodate,
						to = report_dc@horodatefin@horodate,
						by = "day"
				)
				debut = graphdate(time.sequence[1])
				fin = graphdate(time.sequence[length(time.sequence)])
				
				
				
				# creation d'un graphique vide
				if (is.null(main))
					main <- ""
				plot(
						graphdate(time.sequence),
						seq(0, 1, length.out = length(time.sequence)),
						xlim = c(debut, fin),
						type = "n",
						xlab = "",
						xaxt = "n",
						yaxt = "n",
						ylab = gettext("Counting device", domain = "R-stacomiR"),
						main = main,
						#bty="n",
						cex = 0.8
				)
				r <- round(range(time.sequence), "day")
				graphics::axis(1,
						at = graphdate(seq(r[1], r[2], by = "month")),
						labels = strftime(as.POSIXlt(seq(r[1], r[2], by = "month")), format = "%d-%b"))
				if (dim(t_periodefonctdispositif_per)[1] == 0) {
					rect(
							xleft = debut,
							ybottom = 0.6,
							xright = fin,
							ytop = 0.9,
							col = "grey",
							border = NA,
							lwd = 1
					)
					rect(
							xleft = debut,
							ybottom = 0.1,
							xright = fin,
							ytop = 0.4,
							col = color_type_oper["Non connu"],
							border = NA,
							lwd = 1
					)
					legend(
							x = "bottom",
							legend = gettext("Func.", "Stop", "Normal func", domain = "R-stacomiR"),
							pch = c(16, 16),
							col = c("grey", color_type_oper["Non connu"]),
							#horiz=TRUE,
							ncol = 3,
							bty = "n"
					)
				} else {
					if (sum(t_periodefonctdispositif_per$per_etat_fonctionnement == 1) > 0) {
						rect(
								xleft = graphdate(t_periodefonctdispositif_per$per_date_debut[t_periodefonctdispositif_per$per_etat_fonctionnement ==
														1]),
								ybottom = 0.6,
								xright = graphdate(t_periodefonctdispositif_per$per_date_fin[t_periodefonctdispositif_per$per_etat_fonctionnement ==
														1]),
								ytop = 0.9,
								col = color_etat["TRUE"],
								border = NA,
								lwd = 1
						)
					}
					if (sum(t_periodefonctdispositif_per$per_etat_fonctionnement == 0) >
							0)   {
						rect(
								xleft = graphdate(t_periodefonctdispositif_per$per_date_debut[t_periodefonctdispositif_per$per_etat_fonctionnement ==
														0]),
								ybottom = 0.6,
								xright = graphdate(t_periodefonctdispositif_per$per_date_fin[t_periodefonctdispositif_per$per_etat_fonctionnement ==
														0]),
								ytop = 0.9,
								col = color_etat["FALSE"],
								border = NA,
								lwd = 1
						)
					}
				}
				listeperiode <-
						fun_table_per_dis(
								typeperiode = t_periodefonctdispositif_per$per_tar_code,
								tempsdebut = t_periodefonctdispositif_per$per_date_debut,
								tempsfin = t_periodefonctdispositif_per$per_date_fin,
								libelle = t_periodefonctdispositif_per$libelle,
								color = color_type_oper[t_periodefonctdispositif_per$libelle],
								date = FALSE
						)
				
				for (j in 1:length(listeperiode)) {
					
					
					rect(
							xleft = graphdate(listeperiode[[j]]$debut),
							ybottom = 0.1,
							xright = graphdate(listeperiode[[j]]$fin),
							ytop = 0.4,
							col = listeperiode[[j]]$color,
							border = NA,
							lwd = 1
					)
				}
				legend  (
						x = debut,
						y = 0.6,
						legend = gettext(
								"Normal",
								"Stop",
								domain = "R-stacomiR"
						),
						pch = c(15, 15),
						col = color_etat,
						bty = "n",
						horiz = TRUE,
						text.width = (fin - debut) / 6 ,
						cex = 0.8
				)
				legend  (
						x = debut,
						y = 0.1,
						legend = names(color_type_oper),
						pch = c(15, 15),
						col = color_type_oper,
						bty = "n",
						horiz = TRUE,
						text.width = (fin - debut) / 6,
						cex = 0.8
				)
				graphics::text(
						x = debut,
						y = 0.95,
						label = gettext("Operation of the counting device", domain = "R-stacomiR"),
						font = 4,
						pos = 4
				)
				graphics::text(
						x = debut,
						y = 0.45,
						label = gettext("Shutdowns types for this counting device", domain = "R-stacomiR"),
						font = 4,
						pos = 4
				)
				
				#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
				#           PLOT OF TYPE BOX (plot.type=4)
				#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			} else if (plot.type == "4") {
				if (is.null(main))
					main <-
							gettext("Working of the counting device",
									report_dc@dc@dc_selected)
				
				#report_dc<-r_dc; require(RGtk2); require(lubridate);require(ggplot2);main=NULL;silent=FALSE;plot.type="4"
				t_periodefonctdispositif_per = report_dc@data
				tpp <-
						split_per_day(
								t_periodefonctdispositif_per,
								horodatedebut = "per_date_debut",
								horodatefin = "per_date_fin"
						)
				
				g <- ggplot(tpp) +
						geom_rect(aes(
										xmin = xmin,
										xmax = xmax,
										ymin = Hdeb,
										ymax = Hfin,
										fill = libelle
								),
								alpha = 0.8) +
						scale_fill_manual(
								"type",
								values = c(
										color_type_oper
								),
								labels = gettext(
										"Normal oper",
										"Operational stop",
										"Stop",
										"Dysfunct",
										"Unknown",
										domain = "R-stacomiR"
								)
						) +
						#scale_colour_manual("type",values=c("1"="#40CA2C","2"="#C8B22D","3"="#AB3B26","4"="#B46BED","5"="#B8B8B8"),
						#		labels = gettext("Normal oper","Operational stop","Stop","Dysfunct","Unknown")+		)
						ggtitle(main) +
						ylab("Heure") + theme(
								plot.background = element_rect(fill = "black"),
								panel.background = element_rect(fill = "black"),
								legend.background = element_rect(fill = "black"),
								panel.grid.major = element_blank(),
								panel.grid.minor = element_blank(),
								text = element_text(colour = "white"),
								line = element_line(colour = "grey50"),
								legend.key = element_rect(fill = "black", colour = "black"),
								axis.text = element_text(colour = "white")
						)
				
				print(g)
				assign("g_report_dc_4",
						g,
						envir = envir_stacomi)			
				if (!silent)
					funout(
							gettext(
									"Writing the ggplot into envir_stacomi environment : g_report_dc_4 <- get('g_report_dc_4',envir_stacomi)\n",
									domain = "R-stacomiR"
							)
					)
				
			}
			return(invisible(NULL))
		}
)





#' Method to print the command line of the object.
#' @param x An object of class report_dc
#' @param ... Additional parameters passed to print
#' @return Nothing, called for its side effect
#' @author cedric.briand
#' @aliases print.report_dc
#' @export
setMethod(
		"print",
		signature = signature("report_dc"),
		definition = function(x, ...) {
			sortie1 <- "report_dc=new('report_dc')\n"
			sortie2 <- stringr::str_c(
					"report_dc=choice_c(report_dc,",
					"dc=",
					x@dc@dc_selected,
					",",
					"horodatedebut=",
					shQuote(as.character(x@horodatedebut@horodate)),
					",",
					"horodatefin=",
					shQuote(as.character(x@horodatefin@horodate)),
					")"
			)
			# removing backslashes
			funout(stringr::str_c(sortie1, sortie2), ...)
			return(invisible(NULL))
		}
)



#' summary for report_dc, write csv and html output, and prints summary statistics
#' @param object An object of class \code{\link{report_dc-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters (not used there)
#' @return Nothing, called for its side effect of writing html, csv files and printing summary
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases summary.report_dc
#' @export
setMethod(
		"summary",
		signature = signature(object = "report_dc"),
		definition = function(object, silent = FALSE, ...) {
			#report_dc<-r_dc
			report_dc <- object
			t_periodefonctdispositif_per <-
					report_dc@data # on recupere le data.frame
			t_periodefonctdispositif_per$per_date_debut <-
					as.character(t_periodefonctdispositif_per$per_date_debut)
			t_periodefonctdispositif_per$per_date_fin <-
					as.character(t_periodefonctdispositif_per$per_date_fin)
			annee = paste(unique(strftime(
									as.POSIXlt(t_periodefonctdispositif_per$per_date_debut),
									"%Y"
							)), collapse = "+")
			path1 = file.path(
					path.expand(get("datawd", envir = envir_stacomi)),
					paste(
							"t_periodefonctdispositif_per_DC_",
							report_dc@dc@dc_selected,
							"_",
							annee,
							".csv",
							sep = ""
					),
					fsep = "\\"
			)
			res <- tryCatch(
					write.table(
							t_periodefonctdispositif_per,
							file = path1,
							row.names = FALSE,
							col.names = TRUE,
							sep = ";"
					), error = function(e) e,
					finally =
							if (!silent)	funout(gettextf("Writing of %s \n", path1, domain = "R-stacomiR"))
			)
			if (inherits(res, "simpleError")) {
				warnings("The table could not be reported, please modify datawd with options(stacomiR.path='path/to/directory'")
			} else {
				# reports works anyways, write html
				path1html <-
						file.path(
								path.expand(get("datawd", envir = envir_stacomi)),
								paste(
										"t_periodefonctdispositif_per_DC_",
										report_dc@dc@dc_selected,
										"_",
										annee,
										".html",
										sep = ""
								),
								fsep = "\\"
						)
				funout(gettextf(
								"Writing of %s this might take a while, please be patient ...\n",
								path1html
						))
				funhtml(
						t_periodefonctdispositif_per,
						caption = gettextf(
								"t_periodefonctdispositif_per_DC_%s_%s",
								report_dc@dc@dc_selected,
								annee
						),
						top = TRUE,
						outfile = path1html,
						clipboard = FALSE,
						append = FALSE,
						digits = 2
				)
			}
			print(gettextf("summary statistics for CD=%s", report_dc@dc@dc_selected),
					domain = "R-stacomiR")
			print(gettextf("dc_code=%s", report_dc@dc@data[report_dc@dc@data$dc ==
											report_dc@dc@dc_selected, "dc_code"], domain = "R-stacomiR"))
			duree <-
					difftime(
							t_periodefonctdispositif_per$per_date_fin,
							t_periodefonctdispositif_per$per_date_debut,
							units = "day"
					)
			sommes <-
					tapply(duree, t_periodefonctdispositif_per$per_tar_code, sum)
			perc <- round(100 * sommes / as.numeric(sum(duree)))
			sommes <- round(sommes, 2)
			funout(gettext("Duration in days (operation type):", domain = "R-stacomiR"))
			funout(paste(
							gettext(
									"Normal oper",
									"Operational stop",
									"Stop",
									"Dysfunct",
									"Unknown",
									gettext("Func.", "Stop", "Normal func", domain = "R-stacomiR")
							),
							" :",
							sommes,
							"(",
							perc,
							"%)",
							sep = ""
					))
			sommes <-
					tapply(duree,
							t_periodefonctdispositif_per$per_etat_fonctionnement,
							sum)
			perc <- round(100 * sommes / as.numeric(sum(duree)))
			sommes <- round(sommes, 2)
			funout(gettext("Duration in days (operation):", domain = "R-stacomiR"))
			funout(paste(rev(
									gettext("Func.", "Stop", domain = "R-stacomiR")
							),
							" :",
							sommes, "(", perc, "%)", sep = ""))
			return(invisible(NULL))
		}
)
