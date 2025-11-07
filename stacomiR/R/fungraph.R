#' Function for report_mig graphs including numbers DF DC operations
#'
#' This graph is for species other than glass eel
#'
#'
#' @param report_mig An object of class \code{\linkS4class{report_mig}}
#' @param tableau A data frame with the with the following columns : No.pas,debut_pas,fin_pas,
#' ope_dic_identifiant,lot_tax_code,lot_std_code,type_de_quantite,MESURE,CALCULE,
#' EXPERT,PONCTUEL,Effectif_total,taux_d_echappement,coe_valeur_coefficient
#' @note this function is intended to be called from the plot method in report_mig_mult and report_mig
#' @param time.sequence A vector POSIXt
#' @param taxa The species
#' @param stage The stage
#' @param dc The DC
#' @param silent Message displayed or not
#' @param color Default NULL, a vector of color in the following order, working, stopped, 1...5 types of operation
#' for the fishway or DC, measured, calculated, expert, direct observation. If null will be set to brewer.pal(12,"Paired")[c(8,10,4,6,1,2,3,5,7)]
#' @param color_ope Default NULL, a vector of color for the operations. Default to brewer.pal(4,"Paired")
#' @param ... additional parameters passed to matplot, main, ylab, ylim, lty, pch, bty, cex.main,
#' it is currenly not a good idea to change xlim (numbers are wrong, the month plot covers all month, and legend placement is wrong
#' @return No return value, called for side effects
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
fungraph = function(report_mig,
                    tableau,
                    time.sequence,
                    taxa,
                    stage,
                    dc = NULL,
                    silent,
                    color = NULL,
                    color_ope = NULL,
                    ...) {
  #mat <- matrix(1:6,3,2)
  #layout(mat)
  #browser()
  #cat("fungraph")
  # color=null
  # color calculation
  
	oldpar <- par(no.readonly = TRUE) 
	on.exit(par(oldpar))  
  if (is.null(color)) {
    tp <- RColorBrewer::brewer.pal(12, "Paired")
    mypalette = c(
      "working" = tp[4],
      "stopped" = tp[6],
			"Fonc normal" =  tp[1],
			"Arr ponctuel" = tp[2], 
			"Arr maint" = tp[3],
			"Dysfonc" = tp[5], 						
			"Non connu" =  tp[7],
      "ponctuel" = "indianred",
      "expert" = "chartreuse2",
      "calcule" = "deepskyblue",
      "mesure" = "black"
    )
  } else {
    if (length(color) != 11)
      stop("The length of color must be 11")
    mypalette = c(
      "working" =		color[1],
      "stopped" =		color[2],
			"Fonc normal" = color[3],
			"Arr ponctuel" = color[4],
			"Arr maint" = color[5],
			"Dysfonc" = color[6],
			"Non connu" = color[7],
      "mesure" =		color[8],
      "calcule" =		color[9],
      "expert" =		color[10],
      "ponctuel" =		color[11]
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
  annee = unique(strftime(as.POSIXlt(time.sequence), "%Y"))[1]
  mois = months(time.sequence)
  jour = strftime(as.POSIXlt(time.sequence), "%j")
  jmois = strftime(as.POSIXlt(time.sequence), "%d")
  mois = unique(mois)
  mois = paste("15", substr(as.character(mois), 1, 3))
  index = as.vector(tableau$No.pas[jmois == 15])
  x = 1:nrow(tableau)
  debut = unclass(as.POSIXct((min(time.sequence))))[[1]] # attention arrondit e un jour de moins
  fin = unclass(as.POSIXct(max(time.sequence)))[[1]]
  dis_commentaire =  as.character(report_mig@dc@data$dis_commentaires[report_mig@dc@data$dc %in%
                                                                        dc]) # commentaires sur le DC
  ###################################
  # Definition du layout
  ####################################
  vec <- c(rep(1, 15), rep(2, 2), rep(3, 2), 4, rep(5, 6))
  mat <- matrix(vec, length(vec), 1)
  layout(mat)
  
  #par("bg"=grDevices::gray(0.8))
  graphics::par("mar" = c(3, 4, 3, 2) + 0.1)
  ###################################
  # Graph annuel couvrant sequence >0
  ####################################
  dots <- list(...)
  if (!"main" %in% names(dots))
    main = gettextf("Migration graph %s, %s, %s, %s",
                    dis_commentaire,
                    taxa,
                    stage,
                    annee,
                    domain = "R-stacomiR")
  else
    main = dots[["main"]]
  if (!"ylab" %in% names(dots))
    ylab = gettext("Number", domain = "R-stacomiR")
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
    xlim = c(debut, fin)#dots[["xlim"]] # currently this argument is ignored
  if (!"ylim" %in% names(dots))
    ylim = NULL
  else
    ylim = dots[["ylim"]]
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
  matplot(
    time.sequence,
    cbind(
      tableau$MESURE + tableau$CALCULE + tableau$EXPERT + tableau$PONCTUEL,
      tableau$MESURE + tableau$CALCULE + tableau$EXPERT,
      tableau$MESURE + tableau$CALCULE,
      tableau$MESURE
    ),
    col = mypalette[c("ponctuel", "expert", "calcule", "mesure")],
    type = type,
    pch = pch,
    lty = lty,
    xaxt = "n",
    bty = bty,
    ylab = ylab,
    xlab = NULL,
    main = main,
    xlim = c(debut, fin),
    cex.main = cex.main,
    font.main = font.main
  )
  if (report_mig@timestep@step_duration == "86400") {
    # pas de temps journalier
    index = as.vector(x[jmois == 15])
    axis(
      side = 1,
      at = index,
      tick = TRUE,
      labels = mois
    )
    #axis(side=1,at=as.vector(x[jmois==1]),tick=TRUE,labels=FALSE)
    
  } else {
    axis(side = 1)
  }
  mtext(
    text = gettextf("Sum of numbers =%s",
                    round(
                      sum(
                        tableau$MESURE,
                        tableau$CALCULE,
                        tableau$EXPERT,
                        tableau$PONCTUEL,
                        na.rm = TRUE
                      )
                    ), domain = "R-stacomiR"),
    side = 3,
    col = mypalette["expert"],
    cex = 0.8
  )
  
  legend(
    x = 0,
    y = max(
      tableau$MESURE,
      tableau$CALCULE,
      tableau$EXPERT,
      tableau$PONCTUEL,
      na.rm = TRUE
    ),
    legend = gettext("measured", "calculated", "expert", "direct", domain =
                       "R-stacomiR"),
    pch = c(16),
    col = mypalette[c("mesure", "calcule", "expert", "ponctuel")]
  )
  report_ope <- get("report_ope", envir = envir_stacomi)
  t_operation_ope <-
    report_ope@data[report_ope@data$ope_dic_identifiant == dc, ]
  dif = difftime(t_operation_ope$ope_date_fin,
                 t_operation_ope$ope_date_debut,
                 units = "days")
  
  if (!silent) {
    funout(ngettext(
      nrow(t_operation_ope),
      "%d operation \n",
      "%d operations \n",
      domain = "R-stacomiR"
    ))
    funout(gettextf("average trapping time = %s days\n", round(mean(
      as.numeric(dif)
    ), 2), domain = "R-stacomiR"))
    funout(gettextf("maximum term = %s", round(max(
      as.numeric(dif)
    ), 2), domain = "R-stacomiR"))
    funout(gettextf("minimum term = %s", round(min(
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
    vectordate = unclass(vectordate)
    vectordate[vectordate < debut] <- debut
    vectordate[vectordate > fin] <- fin
    return(vectordate)
  }
  
  
  ###################################
  # creation d'un graphique vide (2)
  ###################################
  graphics::par("mar" = c(0, 4, 0, 2) + 0.1)
  plot(
    as.POSIXct(time.sequence),
    seq(0, 3, length.out = nrow(tableau)),
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
  # time for DF (fishway) operation
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
        xleft = graphdate(as.POSIXct(report_df@data$per_date_debut[report_df@data$per_etat_fonctionnement ==
                                                                     1])),
        ybottom = 2.1,
        xright = graphdate(as.POSIXct(report_df@data$per_date_fin[report_df@data$per_etat_fonctionnement ==
                                                                    1])),
        ytop = 3,
        col = mypalette["working"],
        border = NA,
        lwd = 1
      )
    }
    if (sum(report_df@data$per_etat_fonctionnement == 0) > 0) {
      rect(
        xleft = graphdate(as.POSIXct(report_df@data$per_date_debut[report_df@data$per_etat_fonctionnement ==
                                                                     0])),
        ybottom = 2.1,
        xright = graphdate(as.POSIXct(report_df@data$per_date_fin[report_df@data$per_etat_fonctionnement ==
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
				color= mypalette[report_df@data$libelle],
        date = FALSE
      )
    nom_periodes <- vector()
    color_periodes <- vector() # a vector of colors, one per period type in listeperiode
    for (j in 1:length(listeperiode)) {
      #recuperation du vecteur de noms (dans l'ordre) e partir de la liste
      nom_periodes[j] <- substr(listeperiode[[j]]$nom, 1, 17)
      #ecriture pour chaque type de periode
      color_periodes[j] = listeperiode[[j]]$color
      rect(
        xleft = graphdate(listeperiode[[j]]$debut),
        ybottom = 1.1,
        xright = graphdate(listeperiode[[j]]$fin),
        ytop = 2,
        col = color_periodes[j],
        border = NA,
        lwd = 1
      )
 
    }
    
    legend  (
      x = debut,
      y = 1.2,
      legend = c(gettext("working", domain = "R-stacomiR"),gettext("stop", domain = "R-stacomiR"), nom_periodes),
      pch = c(15, 15),
      col = c(mypalette["working"], mypalette["stopped"], color_periodes),
      bty = "n",
      ncol = 7,
      text.width = (fin - debut) / 10
    )
  }
  
  ###################################
  # creation d'un graphique vide (3)
  ###################################
  
  graphics::par("mar" = c(0, 4, 0, 2) + 0.1)
  plot(
    as.POSIXct(time.sequence),
    seq(0, 3, length.out = nrow(tableau)),
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
      #horiz=TRUE,
      ncol = 5,
      bty = "n"
    )
    
    
  } else {
    if (sum(report_dc@data$per_etat_fonctionnement == 1) > 0) {
      rect(
        xleft = graphdate(as.POSIXct(report_dc@data$per_date_debut[report_dc@data$per_etat_fonctionnement ==
                                                                     1])),
        ybottom = 2.1,
        xright = graphdate(as.POSIXct(report_dc@data$per_date_fin[report_dc@data$per_etat_fonctionnement ==
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
        xleft = graphdate(as.POSIXct(report_dc@data$per_date_debut[report_dc@data$per_etat_fonctionnement ==
                                                                     0])),
        ybottom = 2.1,
        xright = graphdate(as.POSIXct(report_dc@data$per_date_fin[report_dc@data$per_etat_fonctionnement ==
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
				color= mypalette[report_df@data$libelle],
        date = FALSE
      )
    nom_periodes <- vector()
    color_periodes <- vector()
    for (j in 1:length(listeperiode)) {
      nom_periodes[j] <- substr(listeperiode[[j]]$nom, 1, 17)
      color_periodes[j] <- listeperiode[[j]]$color
      rect(
        xleft = graphdate(listeperiode[[j]]$debut),
        ybottom = 1.1,
        xright = graphdate(listeperiode[[j]]$fin),
        ytop = 2,
        col = color_periodes[j],
        border = NA,
        lwd = 1
      )
    }
    
    legend  (
      x = debut,
      y = 1.2,
      legend = gettext("working", "stopped", nom_periodes, domain = "R-stacomiR"),
      pch = c(15, 15),
      col = c(mypalette["working"], mypalette["stopped"], color_periodes),
      bty = "n",
      ncol = length(listeperiode) + 2,
      text.width = (fin - debut) / 10
    )
  }
  
  ###################################
  # creation d'un graphique vide (4=op)
  ###################################
  
  
  graphics::par("mar" = c(0, 4, 0, 2) + 0.1)
  plot(
    as.POSIXct(time.sequence),
    seq(0, 1, length.out = nrow(tableau)),
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
    xleft = graphdate(as.POSIXct(t_operation_ope$ope_date_debut)),
    ybottom = 0,
    xright = graphdate(as.POSIXct(t_operation_ope$ope_date_fin)),
    ytop = 1,
    col = color_ope,
    border = NA,
    lwd = 1
  )
  
  
  ###################################
  # Graph mensuel
  ####################################
  graphics::par("mar" = c(4, 4, 1, 2) + 0.1)
  tableau$mois = factor(months(tableau$debut_pas, abbreviate = TRUE),
                        levels = unique(months(tableau$debut_pas, abbreviate = TRUE)))
  tableaum <-
    reshape2::melt(
      data = tableau[, c("MESURE", "CALCULE", "EXPERT", "PONCTUEL", "mois")],
      id.vars = c("mois"),
      measure.vars = c("MESURE", "CALCULE", "EXPERT", "PONCTUEL"),
      variable.name = "type",
      value.name = "number"
    )
  levels(tableaum$type) <-
    gettext("measured", "calculated", "expert", "direct", domain = "R-stacomiR")
  superpose.polygon <- lattice::trellis.par.get("plot.polygon")
  superpose.polygon$col =  mypalette[c("mesure", "calcule", "expert", "ponctuel")]
  superpose.polygon$border = rep("transparent", 6)
  lattice::trellis.par.set("superpose.polygon", superpose.polygon)
  fontsize <- lattice::trellis.par.get("fontsize")
  fontsize$text = 10
  lattice::trellis.par.set("fontsize", fontsize)
  par.main.text <- lattice::trellis.par.get("par.main.text")
  par.main.text$cex = 1
  par.main.text$font = 1
  lattice::trellis.par.set("par.main.text", par.main.text)
  # lattice::show.settings()
  
  par.ylab.text <- lattice::trellis.par.get("par.ylab.text")
  par.ylab.text$cex = 0.8
  lattice::trellis.par.set("par.ylab.text", par.ylab.text)
  par.xlab.text <- lattice::trellis.par.get("par.xlab.text")
  par.xlab.text$cex = 0.8
  lattice::trellis.par.set("par.xlab.text", par.xlab.text)
  
  bar <- lattice::barchart(
    number / 1000 ~ mois,
    groups = type,
    xlab = gettext("Month", domain = "R-stacomiR"),
    ylab = gettext("Number (x1000)", domain = "R-stacomiR"),
    data = tableaum,
    allow.multiple = FALSE,
    strip = FALSE,
    stack = TRUE,
    origin = 0
  )
  print(bar, position = c(0, 0, 1, .25), newpage = FALSE)
  
  return(invisible(NULL))
}
