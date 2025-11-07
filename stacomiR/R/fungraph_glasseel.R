#' Graph function for glass eel migration. Differs from fungraph as it does not
#' draw the ggplot graph for month
#'
#' This graph will also plot numbers and bars according to whether the glass
#' eel have been counted through weight or numbers
#'
#'
#' @param report_mig an object of class \link{report_mig-class} or an
#' object of class \link{report_mig_mult-class}
#' @param table a data frame with the results
#' @param time.sequence a vector POSIXt
#' @param taxa the species
#' @param stage the stage
#' @param dc the counting device, default to null, only necessary for \link{report_mig_mult-class}
#' @param silent Message displayed or not
#' @param color Default NULL, a vector of length 11 of color in the following order, numbers, weight, working, stopped, 1...5 types of operation,
#' the 2 latest colors are not used but kept for consistency with fungraph
#' for the fishway, if null will be set to brewer.pal(12,"Paired")[c(4,6,1,2,3,5,7,8,10,11,12)]
#' @param color_ope Default NULL, a vector of color for the operations. Default to brewer.pal(4,"Paired")
#' @param ... additional parameters passed to plot, main, ylab, cex.main, font.main, type, xlim, ylim, lty, bty, pch
#' it is not possible to change xlim
#' @return No return value, called for side effects
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
fungraph_glasseel = function(report_mig,
		table,
		time.sequence,
		taxa,
		stage,
		dc = null,
		silent,
		color = NULL,
		color_ope = NULL,
		...) {
	oldpar <- par(no.readonly = TRUE) 
	on.exit(par(oldpar))  
	# color=null
	# color calculation
	if (is.null(color)) {
		tp <- RColorBrewer::brewer.pal(12, "Paired")
		mypalette = c(
				"working" = tp[4],
				# green
				"stopped" = tp[6],
				# red
				"Fonc normal" =  tp[1],
				"Arr ponctuel" = tp[2], 
				"Arr maintenance" = tp[3],
				"Dysfonc" = tp[5], 						
				"Non connu" =  tp[7],
				"eff" = tp[8],
				#orange
				"weight" = tp[10],
				#purple
				"unused1" = tp[11],
				"unused1" = tp[12]
		)
	} else {
		if (length(color) != 11)
			stop("The length of color must be 11")
		mypalette = c(
				"working" = color[1],
				"stopped" = color[2],
				"Fonc normal" = color[3],
				"Arr ponctuel" = color[4],
				"Arr maintenance" = color[5],
				"Dysfonc" = color[6],
				"Non connu" = color[7],
				"eff" = color[8],
				"weight" = color[9],
				"unused1" = color[10],
				"unused2" = color[11]
		)
	}
	
	
	if (is.null(color_ope)) {
		if (stacomirtools::is.odd(dc))
			brew = "Paired"
		else
			brew = "Accent"
		color_ope = RColorBrewer::brewer.pal(8, brew)
	}
	
	if (is.null(dc))
		dc = report_mig@dc@dc_selected[1]
	annee = paste(unique(strftime(as.POSIXlt(time.sequence), "%Y")), collapse =
					",")
	mois = months(time.sequence)
	jour = strftime(as.POSIXlt(time.sequence), "%j")
	index = table$No.pas + 1
	eff = table$Effectif_total
	eff.p = table$Effectif_total.p
	debut = unclass(as.Date(time.sequence[min(index)]))[[1]]
	fin = unclass(as.Date(time.sequence[max(index)]))[[1]]
	eff[eff == 0] <- NA #for graph need
	eff.p[eff.p == 0] <- NA
	dis_commentaire =  as.character(report_mig@dc@data$dis_commentaires[report_mig@dc@data$dc %in%
							dc])
	if (!silent)
		funout(gettextf("Glass eels graph %s\n", dis_commentaire))
	###################################
	# Graph annuel couvrant sequence >0
	####################################
	
	vec <- c(rep(1, 15), rep(2, 2), rep(3, 2), 4, rep(5, 6))
	mat <- matrix(vec, length(vec), 1)
	layout(mat)
	#par("bg"=grDevices::gray(0.8))
	graphics::par("mar" = c(3, 4, 3, 2) + 0.1)
	dots <- list(...)
	if (!"main" %in% names(dots))
		main = gettextf("Glass eels graph %s, %s, %s, %s",
				dis_commentaire,
				taxa,
				stage,
				annee,
				domain = "R-stacomiR")
	else
		main = dots[["main"]]
	if (!"ylab" %in% names(dots))
		ylab = gettext("Number of glass eels (x1000)", domain = "R-stacomiR")
	else
		ylab = dots[["ylab"]]
	if (!"cex.main" %in% names(dots))
		cex.main = 1
	else
		cex.main = dots[["cex.main"]]
	if (!"font.main" %in% names(dots))
		font.main = 1
	else
		font.main = dots[["font.main"]]
	if (!"type" %in% names(dots))
		type = "h"
	else
		type = dots[["type"]]
	if (!"xlim" %in% names(dots))
		xlim = c(debut, fin)
	else
		xlim = dots[["xlim"]]
	if (!"ylim" %in% names(dots))
		ylim = c(0, max(eff / 1000, na.rm = TRUE)) * 1.2
	else
		xlim = c(debut, fin)#dots[["xlim"]] # currently this argument is ignored
	if (!"cex" %in% names(dots))
		cex = 1
	else
		cex = dots[["cex"]]
	if (!"lty" %in% names(dots))
		lty = 1
	else
		lty = dots[["lty"]]
	if (!"pch" %in% names(dots))
		pch = 16
	else
		pch = dots[["pch"]]
	if (!"bty" %in% names(dots))
		bty = "l"
	else
		bty = dots[["bty"]]
	plot(
			x = as.Date(time.sequence, "Europe/Paris"),
			y = eff / 1000,
			col = mypalette["eff"],
			type = type,
			xlim = xlim,
			ylim = ylim,
			lty = lty,
			xaxt = "n",
			ylab = ylab,
			#xlab="date",
			cex.main = cex.main,
			font.main = font.main,
			main = main,
			cex = cex,
			pch = pch,
			bty = bty
	)
	#print(plot,position = c(0, .3, 1, .9), more = TRUE)
	r <- as.Date(round(range(time.sequence), "day"))
	axis.Date(1, at = seq(r[1], r[2], by = "weeks"), format = "%d-%b")
	
	points(
			as.Date(time.sequence, "Europe/Paris"),
			eff.p / 1000,
			type = type,
			lty = lty,
			col = mypalette["weight"]
	)
	
	legend(
			x = "topright",
			inset = 0.01,
			legend = gettext("weighted", "counted", domain = "R-stacomiR"),
			pch = c(16, 16),
			col = mypalette[c("weight", "eff")]
	)
	######################################
	# text labels for numbers and weights
	######################################
	text(
			x = debut + (fin - debut) / 8,
			y = max(eff / 1000, na.rm = TRUE) * 1.15,
			labels = paste(round(
							sum(table$poids_depuis_effectifs, na.rm = TRUE) / 1000, 2
					), " kg"),
			col = mypalette["eff"],
			adj = 1,
			cex = cex
	)
	text(
			x = debut + 3 * (fin - debut) / 8 ,
			y = max(eff / 1000, na.rm = TRUE) * 1.15,
			labels = paste("N=", round(
							sum(table$Effectif_total.e, na.rm = TRUE)
					)),
			col = mypalette["eff"],
			adj = 1,
			cex = cex
	)
	text(
			x = debut + (fin - debut) / 8,
			y = max(eff / 1000, na.rm = TRUE) * 1.2,
			labels = paste(round(
							sum(table$Poids_total, na.rm = TRUE) / 1000, 2
					), " kg"),
			col = mypalette["weight"],
			adj = 1,
			cex = cex
	)
	text(
			x = debut + 3 * (fin - debut) / 8,
			y = max(eff / 1000, na.rm = TRUE) * 1.2,
			labels = paste("N=", round(sum(eff.p, na.rm = TRUE))),
			col = mypalette["weight"],
			adj = 1,
			cex = cex
	)
	text(
			x = debut + 3 + (fin - debut) / 8,
			y = max(eff / 1000, na.rm = TRUE) * 1.1,
			labels = paste(round(
							sum(table$Poids_total, table$poids_depuis_effectifs, na.rm = TRUE) / 1000,
							2
					), " kg"),
			col = "black",
			adj = 1,
			cex = cex
	)
	text(
			x = debut + 3 * (fin - debut) / 8,
			y = max(eff / 1000, na.rm = TRUE) * 1.1,
			labels = paste("N=", round(sum(eff, na.rm = TRUE))),
			col = "black",
			adj = 1,
			cex = cex
	)
	segments(
			x0 = debut,
			y0 = max(eff / 1000, na.rm = TRUE) * 1.125,
			x1 = debut + 3 * (fin - debut) / 8,
			y1 = max(eff / 1000, na.rm = TRUE) * 1.125
	)
	
	
	report_ope <- get("report_ope", envir = envir_stacomi)
	t_operation_ope <-
			report_ope@data[report_ope@data$ope_dic_identifiant == dc, ]
	dif = difftime(t_operation_ope$ope_date_fin,
			t_operation_ope$ope_date_debut,
			units = "days")
	
	if (!silent) {
		funout(gettextf(
						"number of operations =%s\n",
						nrow(t_operation_ope),
						domain = "R-stacomiR"
				))
		funout(gettextf("average trapping time = %sdays\n", round(mean(
										as.numeric(dif)
								), 2), domain = "R-stacomiR"))
		funout(gettextf("maximum term = %sdays\n", round(max(
										as.numeric(dif)
								), 2), domain = "R-stacomiR"))
		funout(gettextf("minimum term = %sdays\n", round(min(
										as.numeric(dif)
								), 2), domain = "R-stacomiR"))
	}
	
	df <- report_mig@dc@data$df[report_mig@dc@data$dc == dc]
	report_df <- get("report_df", envir = envir_stacomi)
	report_dc <- get("report_dc", envir = envir_stacomi)
	report_df@data <-
			report_df@data[report_df@data$per_dis_identifiant == df, ]
	report_dc@data <-
			report_dc@data[report_dc@data$per_dis_identifiant == dc, ]
	
	
	graphdate <- function(vectordate) {
		attributes(vectordate) <- NULL
		unclass(vectordate)
	}
	
	
	###################################
	# creation d'un graphique vide (2)
	###################################
	
	graphics::par("mar" = c(0, 4, 0, 2) + 0.1)
	plot(
			as.Date(time.sequence),
			seq(0, 3, length.out = length(eff)),
			xlim = xlim,
			type = "n",
			xlab = "",
			xaxt = "n",
			yaxt = "n",
			ylab = gettext("Fishway", domain = "R-stacomiR"),
			bty = "n",
			cex = cex + 0.2
	)
	
	###################################
	# Time for dc operation
	###################################
	
	if (dim(report_df@data)[1] == 0) {
		rect(
				xleft = debut,
				ybottom = 2.1,
				xright = fin,
				ytop = 3,
				col = "grey",
				border = NA,
				lwd = 1
		)
		rect(
				xleft = debut,
				ybottom = 1.1,
				xright = fin,
				ytop = 2,
				col = "grey40",
				border = NA,
				lwd = 1
		)
		legend(
				x = "bottom",
				legend = gettext("Unknown working", "Unknow operation type", domain =
								"R-stacomiR"),
				pch = c(16, 16),
				col = c("grey", "grey40"),
				horiz = TRUE,
				bty = "n"
		)
		
		
	} else {
		# si il sort quelque chose
		if (sum(report_df@data$per_etat_fonctionnement == 1) > 0) {
			rect(
					xleft = graphdate(as.Date(report_df@data$per_date_debut[report_df@data$per_etat_fonctionnement ==
													1])),
					ybottom = 2.1,
					xright = graphdate(as.Date(report_df@data$per_date_fin[report_df@data$per_etat_fonctionnement ==
													1])),
					ytop = 3,
					col = mypalette["working"],
					border = NA,
					lwd = 1
			)
		}
		if (sum(report_df@data$per_etat_fonctionnement == 0) > 0) {
			rect(
					xleft = graphdate(as.Date(report_df@data$per_date_debut[report_df@data$per_etat_fonctionnement ==
													0])),
					ybottom = 2.1,
					xright = graphdate(as.Date(report_df@data$per_date_fin[report_df@data$per_etat_fonctionnement ==
													0])),
					ytop = 3,
					col = mypalette["stopped"],
					border = NA,
					lwd = 1
			)
		}
		#creation d'une liste par categorie d'arret contenant vecteurs dates
		listeperiode <-
				fun_table_per_dis(
						typeperiode = report_df@data$per_tar_code,
						tempsdebut = report_df@data$per_date_debut,
						tempsfin = report_df@data$per_date_fin,
						libelle = report_df@data$libelle,
						color= mypalette[report_df@data$libelle]
				)
		nomperiode <- vector()
		color_periodes <- vector()
		for (j in 1:length(listeperiode)) {
			nomperiode[j] <- substr(listeperiode[[j]]$nom, 1, 17)
			#ecriture pour chaque type de periode			
			color_periode = listeperiode[[j]]$color
			rect(
					xleft = graphdate(listeperiode[[j]]$debut),
					ybottom = 1.1,
					xright = graphdate(listeperiode[[j]]$fin),
					ytop = 2,
					col = color_periode,
					border = NA,
					lwd = 1
			)
			color_periodes <- c(color_periodes, color_periode)
		}
		# below the colors for operation are from 4 to 3+ntypeoperation
		legend  (
				x = debut,
				y = 1.2,
				legend = gettext("working", "stopped", nomperiode, domain = "R-stacomiR"),
				pch = c(15, 15),
				col = c(mypalette["working"], mypalette["stopped"], color_periodes),
				bty = "n",
				ncol = length(listeperiode) + 2,
				text.width = (fin - debut) / 10
		)
	}
	
	###################################
	# creation d'un graphique vide (3=DC)
	###################################
	
	
	graphics::par("mar" = c(0, 4, 0, 2) + 0.1)
	plot(
			as.Date(time.sequence),
			seq(0, 3, length.out = length(eff)),
			xlim = xlim,
			type = "n",
			xlab = "",
			xaxt = "n",
			yaxt = "n",
			ylab = gettext("CD", domain = "R-stacomiR"),
			bty = "n",
			cex = cex + 0.2
	)
	###################################
	# time for DC (counting device) operation
	###################################
	
	
	if (dim(report_dc@data)[1] == 0) {
		rect(
				xleft = debut,
				ybottom = 2.1,
				xright = fin,
				ytop = 3,
				col = "grey",
				border = NA,
				lwd = 1
		)
		
		rect(
				xleft = debut,
				ybottom = 1.1,
				xright = fin,
				ytop = 2,
				col = "grey40",
				border = NA,
				lwd = 1
		)
		legend(
				x = "bottom",
				legend = gettext("Unknown working", "Unknow operation type", domain =
								"R-stacomiR"),
				pch = c(16, 16),
				col = c("grey", "grey40"),
				horiz = TRUE,
				#ncol=5,
				bty = "n"
		)
		
		
	} else {
		if (sum(report_dc@data$per_etat_fonctionnement == 1) > 0) {
			rect(
					xleft = graphdate(as.Date(report_dc@data$per_date_debut[report_dc@data$per_etat_fonctionnement ==
													1])),
					ybottom = 2.1,
					xright = graphdate(as.Date(report_dc@data$per_date_fin[report_dc@data$per_etat_fonctionnement ==
													1])),
					ytop = 3,
					col = mypalette["working"],
					border = NA,
					lwd = 1
			)
		}
		if (sum(report_dc@data$per_etat_fonctionnement == 0) > 0)
		{
			rect(
					xleft = graphdate(as.Date(report_dc@data$per_date_debut[report_dc@data$per_etat_fonctionnement ==
													0])),
					ybottom = 2.1,
					xright = graphdate(as.Date(report_dc@data$per_date_fin[report_dc@data$per_etat_fonctionnement ==
													0])),
					ytop = 3,
					col = mypalette["stopped"],
					border = NA,
					lwd = 1
			)
		}
		listeperiode <-
				fun_table_per_dis(
						typeperiode = report_dc@data$per_tar_code,
						tempsdebut = report_dc@data$per_date_debut,
						tempsfin = report_dc@data$per_date_fin,
						libelle = report_dc@data$libelle,
						color= mypalette[report_df@data$libelle]
				)
		nomperiode <- vector()
		color_periodes <- vector()
		for (j in 1:length(listeperiode)) {
			nomperiode[j] <- substr(listeperiode[[j]]$nom, 1, 17)
			color_periode = listeperiode[[j]]$color
			rect(
					xleft = graphdate(listeperiode[[j]]$debut),
					ybottom = 1.1,
					xright = graphdate(listeperiode[[j]]$fin),
					ytop = 2,
					col = color_periode,
					border = NA,
					lwd = 1
			)
			color_periodes <- c(color_periodes, color_periode)
		}
		
		legend  (
				x = debut,
				y = 1.2,
				legend = gettext("working", "stopped", nomperiode, domain = "R-stacomiR"),
				pch = c(15, 15),
				col = c(mypalette["working"], mypalette["stopped"], color_periodes),
				bty = "n",
				ncol = length(listeperiode) + 2,
				text.width = (fin - debut) / 10
		)
	}
	
	###################################
	# creation d'un graphique vide (4=OP)
	###################################
	
	
	graphics::par("mar" = c(0, 4, 0, 2) + 0.1)
	plot(
			as.Date(time.sequence),
			seq(0, 1, length.out = length(eff)),
			xlim = xlim,
			type = "n",
			xlab = "",
			xaxt = "n",
			yaxt = "n",
			ylab = gettext("Op", domain = "R-stacomiR"),
			bty = "n",
			cex = cex + 0.2
	)
	###################################
	# operations
	###################################
	rect(
			xleft = graphdate(as.Date(t_operation_ope$ope_date_debut)),
			ybottom = 0,
			xright = graphdate(as.Date(t_operation_ope$ope_date_fin)),
			ytop = 1,
			col = color_ope,
			border = NA,
			lwd = 1
	)
	
	
	###################################
	# Graph mensuel
	####################################
	graphics::par("mar" = c(4, 4, 1, 2) + 0.1)
	petitmois = substr(as.character(mois), 1, 3)
	effmois = tapply(eff, mois, sum, na.rm = TRUE)[c(5, 4, 9, 2, 8, 7, 6, 1, 12, 11, 10, 3)]
	effmois.p = tapply(eff.p, mois, sum, na.rm = TRUE)[c(5, 4, 9, 2, 8, 7, 6, 1, 12, 11, 10, 3)]
	effmois <- data.frame("eff" = effmois)
	effmois.p <- data.frame("eff" = effmois.p)
	tablemens <-
			rbind(
					cbind(
							"eff" = effmois - effmois.p,
							"type" = 2,
							"mois" = 1:12
					),
					cbind(effmois.p, "type" = "1", "mois" = 1:12)
			)
	
	
	superpose.polygon <- lattice::trellis.par.get("superpose.polygon")
	superpose.polygon$col =   mypalette[c("weight", "eff")]
	superpose.polygon$border = rep("transparent", 6)
	lattice::trellis.par.set("superpose.polygon", superpose.polygon)
	fontsize <- lattice::trellis.par.get("fontsize")
	fontsize$text = 10
	lattice::trellis.par.set("fontsize", fontsize)
	par.main.text <- lattice::trellis.par.get("par.main.text")
	par.main.text$cex = cex
	par.main.text$font = 1
	lattice::trellis.par.set("par.main.text", par.main.text)
	
	
	par.ylab.text <- lattice::trellis.par.get("par.ylab.text")
	par.ylab.text$cex = cex - 0.2
	lattice::trellis.par.set("par.ylab.text", par.ylab.text)
	par.xlab.text <- lattice::trellis.par.get("par.xlab.text")
	par.xlab.text$cex = cex - 0.2
	lattice::trellis.par.set("par.xlab.text", par.xlab.text)
	
	
	bar <- lattice::barchart(
			eff / 1000 ~ as.factor(mois),
			groups = as.factor(type),
			xlab = gettext("Month", domain = "R-stacomiR"),
			ylab = gettext("Number (x1000)", domain = "R-stacomiR"),
			#    main=list(label=paste("Donnees mensuelles")),
			data = tablemens,
			allow.multiple = FALSE,
			#			key=lattice::simpleKey(text=c(gettext("weight of monthly number"),gettext("monthly number counted",domain="R-stacomiR")),
			#					rectangles = TRUE,
			#					points=FALSE,
			#					space="right",
			#					cex=0.8),
			strip = FALSE,
			stack = TRUE
	)
	print(bar, position = c(0, 0, 1, .25), newpage = FALSE)
	return(invisible(NULL))
}
