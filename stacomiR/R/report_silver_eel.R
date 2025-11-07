#' Class "report_silver_eel"
#'
#' the report_silver_eel class is used to calculate various statistics about the silver eel run. It comprises calculation
#' of various maturation index such as Durif's stages and Pankhurst eye index. The objective is to provide standardized
#' output to the stations monitoring the silver eel run.
#' @include create_generic.R
#' @include ref_dc.R
#' @include ref_taxa.R
#' @include ref_stage.R
#' @include ref_horodate.R
#' @include ref_par.R
#' @note This class is displayed by interface_report_silver_eel
#' @slot data A data frame with data generated from the database
#' @slot calcdata A list of dc with processed data. Each dc contains a data frame with
#' \itemize{
#' \item (1) qualitative data on body contrast (CONT), presence of punctuation on the lateral line (LINP)
#' \item (2) quantitative data "BL" Body length,"W" weight,"Dv" vertical eye diameter,"Dh" horizontal eye diameter,"FL" pectoral fin length
#' \item (3) calculated durif stages, Pankhurst's index, Fulton's body weight coefficient K_ful
#' \item (4) other columns containing data pertaining to the sample and the control operation:  lot_identifiant,ope_identifiant,
#' ope_dic_identifiant,ope_date_debut,ope_date_fin,dev_code (destination code of fish),
#' dev_libelle (text for destination of fish)
#' }
#' @slot dc Object of class \link{ref_dc-class}: the control devices
#' @slot taxa An object of class \link{ref_taxa-class}: the species
#' @slot stage An object of class \link{ref_stage-class} : the stages of the fish
#' @slot par An object of class \link{ref_par-class}: the parameters used
#' @slot horodatedebut An object of class \link{ref_horodate-class}
#' @slot horodatefin An object of class \link{ref_horodate-class}
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("report_silver_eel", ...)}
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @family report Objects
#' @keywords classes
#' @example inst/examples/report_silver_eel-example.R
#' @aliases report_silver_eel
#' @export
setClass(
  Class = "report_silver_eel",
  representation = representation(
    data = "data.frame",
    calcdata = "list",
    dc = "ref_dc",
    taxa = "ref_taxa",
    stage = "ref_stage",
    par = "ref_par",
    horodatedebut = "ref_horodate",
    horodatefin = "ref_horodate"
  ),
  prototype = prototype(
    data = data.frame(),
    calcdata = list(),
    dc = new("ref_dc"),
    taxa = new("ref_taxa"),
    stage = new("ref_stage"),
    par = new("ref_par"),
    horodatedebut = new("ref_horodate"),
    horodatefin = new("ref_horodate")
  )
)
setValidity("report_silver_eel", function(object)
{
  rep1 = object@taxa@taxa_selected[1] == '2038'
  label1 <-
    'report_silver_eel should only be for eel (tax_code=2038)'
  rep2 = all(object@stage@stage_selected %in% c('AGG', 'AGJ'))
  label2 <-
    'Only stages silver (AGG) and yellow (AGJ) should be used in report_silver_eel'
  return(ifelse(rep1 &
                  rep2 , TRUE , c(label1, label2)[!c(rep1, rep2)]))
})
#' connect method for report_silver_eel
#'
#' @param object An object of class \link{report_silver_eel-class}
#' @param silent Boolean if TRUE messages are not displayed
#' @return An object of class \link{report_silver_eel-class} with slot data \code{@data} filled
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases connect.report_silver_eel
setMethod(
  "connect",
  signature = signature("report_silver_eel"),
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


#' charge method for report_silver_eel class
#'
#' this method is used by the graphical interface load the user's choice and get the objects pasted in
#' envir_stacomi. It is not necessary to run this method when loading from the command line using the
#' choice_c method
#' @param object An object of class \link{report_silver_eel-class}
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class \link{report_silver_eel-class}  with slots filled from values assigned in \code{envir_stacomi} environment
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @return An object of the class
#' @aliases charge.report_silver_eel
#' @keywords internal
setMethod(
  "charge",
  signature = signature("report_silver_eel"),
  definition = function(object,
      silent = FALSE) {
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
    if (exists("report_silver_eel_date_debut", envir_stacomi)) {
      object@horodatedebut@horodate <-
        get("report_silver_eel_date_debut", envir_stacomi)
    } else {
      funout(gettext("You need to choose the starting date\n", domain = "R-stacomiR"),
             arret = TRUE)
    }
    if (exists("report_silver_eel_date_fin", envir_stacomi)) {
      object@horodatefin@horodate <-
        get("report_silver_eel_date_fin", envir_stacomi)
    } else {
      funout(gettext("You need to choose the ending date\n", domain = "R-stacomiR"),
             arret = TRUE)
    }
    validObject(object)
    if (!silent)
      funout(
          gettext(
              "Writing report_silver_eel in the environment envir_stacomi : write r_silver<-get('r_silver',envir_stacomi) ",
              domain = "R-stacomiR"
          )
      )    
    assign("r_silver", object, envir_stacomi)
    return(object)
  }
)


#' command line interface for report_silver_eel class
#' 
#' #' The choice_c method fills in the data slot for classes \link{ref_dc-class}, \link{ref_taxa-class}, \link{ref_stage-class}, \link{ref_par-class} and two slots of \link{ref_horodate-class} and then
#' uses the choice_c methods of these object to select the data.
#' @param object An object of class \link{report_silver_eel-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,ref_dc-method}
#' @param taxa '2038=Anguilla anguilla',
#' these should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,ref_taxa-method}
#' @param stage 'AGG'
#' @param par Parameters chosen for the report are body size (1786), vertical eye diameter (BBBB), horizontal eye diameter (CCCC),
#' body contrast (CONT), presence of punctuation on the lateral line (LINP), length of the pectoral fin (PECT)
#' @param horodatedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param horodatefin The finishing date of the report, for this class this will be used to calculate the number of daily steps.
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class \link{report_mig-class}
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases choice_c.report_silver_eel
setMethod(
  "choice_c",
  signature = signature("report_silver_eel"),
  definition = function(object,
                        dc,
                        taxa = 2038,
                        stage = 'AGG',
                        par = c('1786', 'CCCC', 'BBBB', 'CONT', 'LINP', 'A111', 'PECT'),
                        horodatedebut,
                        horodatefin,
                        silent = FALSE) {
    # code for debug using example
    #r_silver<-b_carlothorodatedebut="2010-01-01";horodatefin="2015-12-31"
    r_silver <- object
    r_silver@dc = charge(r_silver@dc)
    # loads and verifies the dc
    # this will set dc_selected slot
    r_silver@dc <- choice_c(object = r_silver@dc, dc)
    # only taxa present in the report_mig are used
    r_silver@taxa <-
      charge_with_filter(object = r_silver@taxa, r_silver@dc@dc_selected)
    r_silver@taxa <- choice_c(r_silver@taxa, taxa)
    r_silver@stage <-
      charge_with_filter(object = r_silver@stage,
                         r_silver@dc@dc_selected,
                         r_silver@taxa@taxa_selected)
    r_silver@stage <- choice_c(r_silver@stage, stage)
    r_silver@par <-
      charge_with_filter(
        object = r_silver@par,
        r_silver@dc@dc_selected,
        2038,
        'AGG'
      )
    r_silver@par <- choice_c(r_silver@par, par, silent = silent)
    r_silver@horodatedebut <- choice_c(
      object = r_silver@horodatedebut,
      nomassign = "reportArg_date_debut",
      funoutlabel = gettext("Beginning date has been chosen\n", domain = "R-stacomiR"),
      horodate = horodatedebut,
      silent = silent
    )
    r_silver@horodatefin <- choice_c(
      r_silver@horodatefin,
      nomassign = "reportArg_date_fin",
      funoutlabel = gettext("Ending date has been chosen\n", domain = "R-stacomiR"),
      horodate = horodatefin,
      silent = silent
    )
    validObject(r_silver)
    return(r_silver)
  }
)

#' Calculate individual silver eel parameters.
#'
#' This calcule method for report_silver_eel, will transform data from long (one line per size characteristic,
#' size, weight, eye diameter, pectoral fin measurement, lateral line and constrast) to wide format (one
#' line per silver eel). It will also calculate Durif silvering index and Pankhurst and Fulton's K.
#'
#' @param object An object of class \link{report_silver_eel-class}
#' @param silent Boolean, if TRUE, information messages are not displayed, only warnings and errors
#' @return An object of class \link{report_silver_eel-class} with slot calcdata filled, as a list
#' for each counting device
#' @aliases calcule.report_silver_eel
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
setMethod(
  "calcule",
  signature = signature("report_silver_eel"),
  definition = function(object, silent) {
    r_silver <- object
    if (nrow(r_silver@data) == 0) {
      funout(
        gettext("No data of silver or yellow eel on the selected period", domain =
                  "R-stacomiR"),
        arret = TRUE
      )
    }
    arg = r_silver@data
    lesdc <- r_silver@dc@dc_selected
    parquant <- c("1786", "A111", "BBBB", "CCCC", "PECT")
    parqual <- c("CONT", "LINP")
    for (i in 1:length(lesdc)) {
      dc <- lesdc[i]
      other <-
        dplyr::select(
          arg,
          lot_identifiant,
          ope_dic_identifiant,
          ope_identifiant,
          ope_date_debut,
          ope_date_fin,
          dev_code,
          dev_libelle
        )
      other <- dplyr::filter(other, ope_dic_identifiant == dc)
      other <-
        dplyr::group_by(
          other,
          lot_identifiant,
          ope_identifiant,
          ope_dic_identifiant,
          ope_date_debut,
          ope_date_fin,
          dev_code,
          dev_libelle
        )
      other <- dplyr::summarize(other)
      other <- as.data.frame(other)
      other <-
        fun_date_extraction(other,
                            "ope_date_debut",
                            jour_an = TRUE,
                            jour_mois = FALSE)
      # extracting the dc from the array
      # all parms are there but some are null,i.e.val_libelle is null for quantitative parm and
      # car_valeur_quantitatif is null for for qualitative parms
      matqual <- reshape2::acast(
        arg[arg$ope_dic_identifiant == lesdc[i], ],
        lot_identifiant ~ car_par_code + car_val_identifiant,
        value.var = "val_libelle",
        drop = TRUE
      )
      matquant <- reshape2::acast(
        arg[arg$ope_dic_identifiant == lesdc[i], ],
        lot_identifiant ~ car_par_code + car_val_identifiant,
        value.var = "car_valeur_quantitatif",
        drop = TRUE
      )
      
      # this function will select the parameters one by one
      # test them for pattern against column name
      # and return the column. So a data frame of quantitative or qualitative parm are returned
      fn <- function(X, mat) {
        veccol <- grepl(X, dimnames(mat)[[2]])
        return(mat[, veccol])
      }
      matquant2 <- sapply(X = parquant, FUN = fn, mat = matquant)
      colnames(matquant2) <- c("BL", "W", "Dv", "Dh", "FL")
      
      matqual2 <- sapply(
        X = parqual,
        FUN = fn,
        mat = matqual,
        simplify = FALSE
      )
      # now matquant2 only contain the correct columns
      # matqual has two column for a single qualitative variable, which is wrong
      # we will merge them
      
      # however there is a bug if only one value is present
      # depending on the data structure there might a bug
      # when there is only one dimension (ie on instance of factor where there should be two)
      for (z in 1:length(matqual2)) {
        if (is.null(dim(matqual2[[z]])[2]))
          matqual2[[z]] <- cbind(matqual2[[z]], NA)
      }
      matqual3 <- matrix(NA, nrow = nrow(matqual2[[1]]), ncol = length(parqual))
      # below if the data in  the first column is NA we choose the second
      # which migh also be NA in which case the result becomes a NA
      
      for (j in 1:length(parqual)) {
        theparqual = parqual[j]
        matqual3[, j] <-
          apply(matqual2[[theparqual]], 1, function(X)
            ifelse(is.na(X[1]), X[2], X[1]))
      }
      dd <- as.data.frame(matqual3)
      rownames(dd) <- rownames(matquant2)
      colnames(dd) <- parqual
      dd$stage <- as.vector(fun_stage_durif(matquant2))
      dd <- cbind(dd, as.data.frame(matquant2))
      dd$MD <- rowMeans(dd[, c("Dv", "Dh")], na.rm = TRUE)
      dd$Pankhurst = 100 * (dd$MD / 2) ^ 2 * pi / dd$BL
      #K = 100 Wt /TL3 with Wt in g and TL in cm	(Cone 1989). (Acou, 2009)
      dd$K_ful = 100 * dd$W / (dd$BL / 10) ^ 3
      ddd <- cbind(other, dd)
      r_silver@calcdata[[as.character(dc)]] <- ddd
    }
    assign("r_silver", r_silver, envir_stacomi)
    return(r_silver)
  }
)


#' Plots of various type for report_silver_eel
#'
#' @param x An object of class \link{report_silver_eel-class}
#' @param plot.type Default "1"
#'  \describe{
#' 		\item{plot.type="1"}{Lattice plot of Durif's stages according to Body Length and Eye Index (average of vertical and horizontal diameters).
#' If several DC are provided then a comparison of data per dc is provided}
#' 		\item{plot.type="2"}{Lattice plot giving a comparison of Durif's stage proportion over time, if several DC are provided an annual comparison
#' is proposed, if only one DC is provided then the migration is split into month.}
#' 		\item{plot.type="3"}{ Series of graphs showing  mean Fulton's coefficient, Pankhurst eye index,	along
#' with a size weight analysis and regression using robust regression (rlm more robust to the presence of outliers)}
#' 			\item{plot.type="4"}{ Lattice cloud plot of Pankurst~ Body Length ~ weight)}
#' }
#' @param silent Stops displaying the messages
#' @return A lattice xy.plot if \code{plot.type =1}, a lattice barchart if \code{plot.type=2}, nothing but plots a series of graphs in 
#' a single plot if \code{plot.type=3}, a lattice cloud object if \code{plot.type=4}
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases plot.report_silver_eel
#' @importFrom stats update
#' @export
setMethod(
  "plot",
  signature(x = "report_silver_eel", y = "missing"),
  definition = function(x,
                        plot.type = c("1","2","3","4"),
                        silent = FALSE) {
    #r_silver<-r_sample_char;require(ggplot2);plot.type="1"
    #browser()
			oldpar <- par(no.readonly = TRUE) 
			on.exit(par(oldpar))  
    r_silver <- x
		plot.type <- as.character(plot.type)# to pass also characters
		plot.type <- match.arg(plot.type)
    if (exists("r_silver", envir_stacomi)) {
      r_silver <- get("r_silver", envir_stacomi)
    } else {
      if (!silent)
        funout(
          gettext("You need to launch computation first, clic on calc\n", domain =
                    "R-stacomiR"),
          arret = TRUE
        )
    }
    dat <- r_silver@calcdata
    # cols are using viridis::inferno(6,alpha=0.9)
    blue_for_males <- grDevices::adjustcolor("#008490", alpha.f = 0.8)
    
    datdc <- data.frame()
    
    
    for (i in 1:length(dat)) {
      datdc <- rbind(datdc, dat[[i]])
    }
    
    
    
    # trellis.par.get()
    datdc$stage <-
      factor(datdc$stage, levels = c("I", "FII", "FIII", "FIV", "FV", "MII"))
    datdc$ope_dic_identifiant <- as.factor(datdc$ope_dic_identifiant)
    datdc$ouv <- NA
    for (i in 1:length(r_silver@dc@dc_selected)) {
      datdc$ouv[datdc$ope_dic_identifiant == r_silver@dc@dc_selected[i]] <-
        r_silver@dc@data[r_silver@dc@data$dc == r_silver@dc@dc_selected[i], "ouv_libelle"]
    }
    
    
    
    
    #################################################
    # plot.type =1 Eye, length category durif stages
    #################################################
    
    if (plot.type == "1") {
      my.settings <- list(
        superpose.symbol = list(
          col = c(
            "Lime green",
            "#420A68E6",
            "#932667E6",
            "#DD513AE6",
            "#FCA50AE6",
            blue_for_males
          ),
          pch = c(3, 4, 8, 15, 16, 17),
          cex = c(1, 1, 1, 1, 1, 1),
          alpha = c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9)
        ),
        superpose.line = list(
          col = c(
            "#FBA338",
            "#420A68E6",
            "#932667E6",
            "#DD513AE6",
            "#FCA50AE6",
            blue_for_males
          )
        ),
        strip.background = list(col = "#932667E6"),
        strip.border = list(col = "black")
      )
      lattice::trellis.par.set(my.settings)
      # show.settings()
      if (length(dat) > 1) {
        form <- as.formula(MD ~ BL | ouv)
      } else {
        form <- as.formula(MD ~ BL)
      }
      
      xy.plot <- lattice::xyplot(
        form,
        data = datdc,
        group = stage,
        type = c("p"),
        par.settings = my.settings,
        xlab = gettext("size (BL mm)", domain = "R-stacomiR"),
        ylab = gettext("Mean eye diameter (MD mm)", domain = "R-stacomiR"),
        par.strip.text = list(col = "white", font = 2),
        auto.key = list(
          title = gettext("Silvering stages (Durif et al. 2009)", domain = "R-stacomiR"),
          cex.title = 1.2,
          space = "top",
          columns = 6,
          between.columns = 1
        )
      )
      # draw lines in lattice
      xy.plot <- update(
        xy.plot,
        panel = function(...) {
          lattice::panel.abline(
            h = c(6.5, 8),
            v = c(300, 450, 500) ,
            lty = "dotted",
            col = "light grey"
          )
          lattice::panel.xyplot(...)
        }
      )
      
      return(xy.plot)
      
    }
    ######################################
    # Migration according to stage, month and year
    # !! throws a warning calling par(new=TRUE) with no plot, no dev.new()
    ######################################
    if (plot.type == "2") {
      datdc1 <- dplyr::select(datdc, ouv, annee, mois, stage)
      datdc1 <- dplyr::group_by(datdc1, ouv, annee, mois, stage)
      datdc1 <- dplyr::summarize(datdc1, N = dplyr::n())
      datdc1 <- as.data.frame(datdc1)
      # show.settings()
      my.settings <- list(
        superpose.polygon = list(
          col = c(
            "Lime green",
            "#420A68E6",
            "#932667E6",
            "#DD513AE6",
            "#FCA50AE6",
            blue_for_males
          ),
          alpha = c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9)
        ),
        superpose.line = list(
          col = c(
            "#FBA338",
            "#420A68E6",
            "#932667E6",
            "#DD513AE6",
            "#FCA50AE6",
            blue_for_males
          )
        ),
        #colfn<-colorRampPalette(c("#1C4587", "#BBC7DB"),space = "Lab")
        #colfn(7)
        strip.background = list(
          col = c(
            "#1B4586",
            "#3E5894",
            "#596DA2",
            "#7282B0",
            "#8A98BE",
            "#A2AFCC",
            "#BAC6DA"
          )
        ),
        strip.border = list(col = "black")
      )
      lattice::trellis.par.set(my.settings)
      
      # show.settings()
      if (length(dat) > 1) {
        form <- as.formula(N ~ annee | ouv)
      } else {
        form <- as.formula(N ~ mois | annee)
      }
      
      bb <- lattice::barchart(
        form,
        data = datdc1,
        group = stage,
        xlab = gettext("Month", domain = "R-stacomiR"),
        ylab = gettext("Number", domain = "R-stacomiR"),
        par.strip.text = list(col = "white", font = 2),
        auto.key = list(
          title = gettext("Number by silvering stage", domain = "R-stacomiR"),
          cex.title = 1.2,
          space = "top",
          columns = 6,
          between.columns = 0.5
        )
      )
      return(bb)
      
    }
    ######################################
    # Series of graphs showing proportion of stage, mean Fulton's coefficient, Pankhurst eye index,
    # body weight, body size, sex ratio.
    ######################################
    if (plot.type == "3") {
      layout(
        matrix(c(1, 2, 3, 4, 4, 5, 6, 6, 7), 3, 3, byrow = TRUE),
        widths = c(3, 3, 1),
        heights = c(3, 1, 3)
      )
      # width 331 sets the last column relative width
      # same for rows
      par(mar = c(3, 4.1, 4.1, 2.1))# ressetting to default
      datdc <- chnames(datdc, "ope_dic_identifiant", "dc")
      lesdc <- unique(datdc$dc)
      datdc$sex <- "F"
      datdc$sex[datdc$BL < 450] <- "M"
      
      #############
      # Fulton
      #############
      moy <- tapply(datdc$K_ful, list(datdc$dc, datdc$sex), mean, na.rm = TRUE)
      sd <-
        tapply(datdc$K_ful, list(datdc$dc, datdc$sex), sd, na.rm = TRUE) # sample standard deviation
      n <- tapply(datdc$K_ful, list(datdc$dc, datdc$sex), length)
      SE = sd / sqrt(n)
      plotTop = max(moy + 3 * SE, na.rm = TRUE)
      
      
      bp <- barplot(
        moy,
        beside = TRUE,
        las = 1,
        ylim = c(0, plotTop),
        cex.names = 0.75,
        main = "Fulton coefficient (+-2SE)",
        ylab = "Fulton K",
        xlab = "",
        border = "black",
        axes = TRUE,
        #legend.text = TRUE,
        #args.legend = list(title = "DC",
        #		x = "topright",
        #		cex = .7)
      )
      graphics::segments(bp, moy - SE * 2, bp,
                         moy + SE * 2, lwd = 2)
      
      graphics::arrows(
        bp,
        moy - SE * 2,
        bp,
        moy + SE * 2,
        lwd = 2,
        angle = 90,
        code = 3,
        length = 0.05
      )
      
      
      #############
      # Pankhurst
      #############
      moy <-
        tapply(datdc$Pankhurst, list(datdc$dc, datdc$sex), mean, na.rm = TRUE)
      sd <-
        tapply(datdc$Pankhurst, list(datdc$dc, datdc$sex), sd, na.rm = TRUE) # sample standard deviation
      n <- tapply(datdc$Pankhurst, list(datdc$dc, datdc$sex), length)
      SE = sd / sqrt(n)
      plotTop = max(moy + 3 * SE, na.rm = TRUE)
      
      
      bp <- barplot(
        moy,
        beside = TRUE,
        las = 1,
        ylim = c(0, plotTop),
        cex.names = 0.75,
        main = "Pankhurst (+-2SE)",
        ylab = "Pankhurst eye index",
        xlab = "",
        border = "black",
        axes = TRUE,
        #legend.text = TRUE,
        #args.legend = list(title = "DC",
        #		x = "topright",
        #		cex = .7)
      )
      segments(bp, moy - SE * 2, bp,
               moy + SE * 2, lwd = 2)
      
      arrows(
        bp,
        moy - SE * 2,
        bp,
        moy + SE * 2,
        lwd = 2,
        angle = 90,
        code = 3,
        length = 0.05
      )
      
      #############
      # empty plot
      #############
      op <- par(mar = c(1, 1, 1, 1))
      plot(
        1,
        type = "n",
        axes = F,
        xlab = "",
        ylab = ""
      )
      legend("center",
             fill = grDevices::grey.colors(nrow(moy)),
             legend = unique(datdc$dc))
      # grey.colors is the default color generation for barplot
      #############
      # size hist
      #############
      par(mar = c(1, 4.1, 1, 1))
      for (i in 1:length(lesdc)) {
        indexdc <- datdc$dc == lesdc[i]
        histxn <-
          graphics::hist(datdc$BL[indexdc],
                         breaks = seq(250, 1100, by = 50),
                         plot = FALSE)$density
        if (i == 1)
          histx <- histxn
        else
          histx <- cbind(histx, histxn)
        
      }
      if (length(lesdc) > 1)
        colnames(histx) <- lesdc
      barplot(
        height = t(histx),
        space = 0,
        beside = FALSE,
        las = 1,
        horiz = FALSE,
        legend.text = FALSE,
        axes = FALSE
      )
      #############
      # empty plot
      #############
      op <- par(mar = c(1, 1, 1, 1))
      plot(
        1,
        type = "n",
        axes = F,
        xlab = "",
        ylab = ""
      )
      
      #############
      # size -weight
      #############
      par(mar = c(5.1, 4.1, 1, 1)) # blur bottom left up right
      plot(
        datdc$BL,
        datdc$W,
        type = "n",
        xlab = gettext("Size (mm)", domain = "R-stacomiR"),
        ylab = gettext("Weight(g)", domain = "R-stacomiR"),
        xlim = c(250, 1000),
        ylim = c(0, 2000)
      )
      abline(v = seq(250, 1000, by = 50),
             col = "lightgray",
             lty = 2)
      abline(h = seq(0, 2000, by = 100),
             col = "lightgray",
             lty = 2)
      # some alpha blending to better see the points :
      lescol <- ggplot2::alpha(grDevices::grey.colors(nrow(moy)), 0.8)
      for (i in 1:length(lesdc)) {
        indexdc <- datdc$dc == lesdc[i]
        points(
          datdc$BL[indexdc],
          datdc$W[indexdc],
          pch = 16,
          col = lescol[i],
          cex = 0.8
        )
        
      }
      ######################"
      # Size - weight model using robust regression
      ######################
      subdatdc <- datdc[, c("BL", "W")]
      subdatdc$BL3 <- (subdatdc$BL / 1000) ^ 3
      # plot(subdatdc$W~subdatdc$BL3)
      
      rlmmodb <- MASS::rlm(W ~ 0 + BL3, data = subdatdc)
      #summary(rlmmodb)
      newdata <-
        data.frame("BL" = seq(250, 1000, by = 50),
                   "BL3" = (seq(250, 1000, by = 50) / 1000) ^ 3)
      pred <-
        predict(
          rlmmodb,
          newdata = newdata,
          se.fit = TRUE,
          type = "response",
          interval = "prediction"
        )
      newdata$predlm <- pred$fit[, 1]
      newdata$predlowIC <- pred$fit[, 2]
      newdata$predhighIC <- pred$fit[, 3]
      
      points(newdata$BL, newdata$predlm, type = "l")
      points(
        newdata$BL,
        newdata$predlowIC,
        type = "l",
        lty = 2,
        col = "grey50"
      )
      points(
        newdata$BL,
        newdata$predhighIC,
        type = "l",
        lty = 2,
        col = "grey50"
      )
      
      text(400, 1500, stringr::str_c("W=", round(coefficients(rlmmodb), 1), " BL^3"))
      
      #############
      # weight hist rotate
      #############
      par(mar = c(5.1, 1, 1, 1))
      for (i in 1:length(lesdc)) {
        indexdc <- datdc$dc == lesdc[i]
        histyn <-
          hist(datdc$W[indexdc],
               plot = FALSE,
               breaks = seq(0, 2500, by = 100))$density
        if (i == 1)
          histy <- histyn
        else
          histy <- cbind(histy, histyn)
        
      }
      if (length(lesdc) > 1)
        colnames(histy) <- lesdc
      barplot(
        height = t(histy),
        space = 0,
        beside = FALSE,
        las = 1,
        horiz = TRUE,
        legend.text = FALSE,
        axes = FALSE
      )
      
    }
    if (plot.type == "4") {
      #creating a shingle with some overlaps
      my.settings <- list(
        superpose.polygon = list(
          col = c(
            "Lime green",
            "#420A68E6",
            "#932667E6",
            "#DD513AE6",
            "#FCA50AE6",
            blue_for_males
          ),
          alpha = c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9)
        ),
        superpose.line = list(
          col = c(
            "#FBA338",
            "#420A68E6",
            "#932667E6",
            "#DD513AE6",
            "#FCA50AE6",
            blue_for_males
          )
        ),
        #colfn<-colorRampPalette(c("#1C4587", "#BBC7DB"),space = "Lab")
        #colfn(7)
        strip.background = list(
          col = c(
            "#1B4586",
            "#3E5894",
            "#596DA2",
            "#7282B0",
            "#8A98BE",
            "#A2AFCC",
            "#BAC6DA"
          )
        ),
        strip.border = list(col = "black")
      )
      lattice::trellis.par.set(my.settings)
      datdc <-
        datdc[complete.cases(datdc[, c("Pankhurst", "W", "BL", "ouv", "stage")]), ]
      ccc <-
        lattice::cloud(
          Pankhurst ~ W * BL | ouv,
          data = datdc,
          group = stage,
          screen = list(x = -90, y = 70),
          distance = .4,
          zoom = .6,
          strip = lattice::strip.custom(par.strip.text = list(col = "white"))
        )
      return(ccc)
    }
    
    
  }
)

#' summary for report_silver_eel
#' @param object An object of class \code{\link{report_silver_eel-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters
#' @return A list per DC with statistic for Durif stages, Pankhurst, MD Eye diameter, BL body length and weight W
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases summary.report_silver_eel
#' @export
setMethod(
  "summary",
  signature = signature(object = "report_silver_eel"),
  definition = function(object, silent = FALSE, ...) {
    r_silver <- object
    if (exists("r_silver", envir_stacomi)) {
      r_silver <- get("r_silver", envir_stacomi)
    } else {
      if (!silent)
        funout(
          gettext("You need to launch computation first, clic on calc\n", domain =
                    "R-stacomiR"),
          arret = TRUE
        )
    }
    dat <- r_silver@calcdata
    # cols are using viridis::inferno(6,alpha=0.9)
    
    printstat <- function(vec, silent) {
      moy <- mean(vec, na.rm = TRUE)
      sd <- sd(vec, na.rm = TRUE) # sample standard deviation
      n <- length(vec[!is.na(vec)])
      SE = sd / sqrt(n)
      if (!silent)
        print(noquote(
          stringr::str_c(
            "mean=",
            round(moy, 2),
            ",SD=",
            round(sd, 2),
            ",N=",
            n,
            ",SE=",
            round(SE, 2)
          )
        ))
      return(list(
        "mean" = moy,
        "SD" = sd,
        "N" = n,
        "SE" = SE
      ))
    }
    result <- list()
    for (i in 1:length(dat)) {
      datdc <-	dat[[i]]
      ouvrage <-
        r_silver@dc@data[r_silver@dc@data$dc == r_silver@dc@dc_selected[i], "ouv_libelle"]
      dc <- as.character(unique(datdc$ope_dic_identifiant))
      result[[dc]] <- list()
      result[[dc]][["ouvrage"]] <- ouvrage
      if (!silent) {
        print(noquote(stringr::str_c("Statistics for dam : ", ouvrage)))
        print(noquote("========================"))
        print(noquote("Stages Durif"))
        print(table(datdc$stage))
      }
      result[[dc]][["Stages"]] <- table(datdc$stage)
      if (!silent) {
        print(noquote("-----------------------"))
        print(noquote("Pankhurst"))
        print(noquote("-----------------------"))
      }
      result[[dc]][["Pankhurst"]] <-
        printstat(datdc$Pankhurst, silent = silent)
      if (!silent) {
        print(noquote("-----------------------"))
        print(noquote('Eye diameter (mm)'))
        print(noquote("-----------------------"))
      }
      result[[dc]][["MD"]] <- printstat(datdc$MD, silent = silent)
      if (!silent) {
        print(noquote("-----------------------"))
        print(noquote('Length (mm)'))
        print(noquote("-----------------------"))
      }
      result[[dc]][["BL"]] <- printstat(datdc$BL, silent = silent)
      if (!silent) {
        print(noquote("-----------------------"))
        print(noquote('Weight (g)'))
        print(noquote("-----------------------"))
      }
      result[[dc]][["W"]] <- printstat(datdc$W, silent = silent)
    }
    return(result)
  }
)

#' Method to print the command line of the object
#' @param x An object of class report_silver_eel
#' @param ... Additional parameters passed to print
#' @return NULL, prints data in the console
#' @author cedric.briand
#' @aliases print.report_silver_eel
#' @export
setMethod(
  "print",
  signature = signature("report_silver_eel"),
  definition = function(x, ...) {
    sortie1 <- "r_silver=new('report_silver_eel')"
    sortie2 <- stringr::str_c(
      "r_silver=choice_c(r_silver,",
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


#' funplotreport_silver_eel
#'
#' assigns an object g in envir_stacomi for eventual modification of the plot
#' @param action, action 1,2,3 or 4 corresponding to plot
#' @param ... Additional parameters
#' @return Nothing
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @keywords internal
funplotreport_silver_eel = function(action, ...) {
  r_silver <- get(x = "report_silver_eel", envir = envir_stacomi)
  r_silver <- charge(r_silver)
  r_silver <- connect(r_silver)
  r_silver <- calcule(r_silver)
  #plot.type is determined by button in h$action
  the_plot <- plot(r_silver, plot.type = action)
  print(the_plot)
}



#' Function to calculate the stages from Durif
#'
#' @param data A dataset with columns BL, W, Dv, Dh, FL corresponding to body length (mm),
#' Weight (g), vertical eye diameter (mm), vertical eye diameter (mm), and pectoral fin length (mm)
#' @returns A data.frame with durif stages per individual
#' @author Laurent Beaulaton \email{laurent.beaulaton@ofb.fr}
#' @export
fun_stage_durif = function(data) {
  # see section Good Practise in ? data
  data(coef_durif, envir = environment())
  stopifnot(colnames(data) == c("BL", "W", "Dv", "Dh", "FL"))
  data <-
    cbind(1, data[, c(1, 2, 5)], rowMeans(data[, c("Dv", "Dh")], na.rm = TRUE))
  colnames(data) <- c("Constant", "BL", "W", "FL", "MD")
  data <- data[, c(1, 2, 3, 5, 4)]
  indices <- data %*% coef_durif
  return(unlist(apply(indices, 1, function(X)
    ifelse(is.na(X[1]), NA, names(which.max(X))))))
}
