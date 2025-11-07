#' Class 'report_sample_char'
#' 
#' The report_sample_char class is used to load and display sample characteristics, which can be either
#' continuous or discrete variable, for instance, it can be used to analyze size or sex structure during
#' a given period. 
#' 
#' @note This class is displayed by interface_report_sample_char, in the database, the class calls the content
#' of the view vue_lot_ope_car
#' @slot data A data frame
#' @slot dc An object of class \link{ref_dc-class}: the control devices
#' @slot taxa An object of class \link{ref_taxa-class}: the species
#' @slot stage An object of class \link{ref_stage-class} : the stages of the fish
#' @slot par An object of class \link{ref_par-class}: the parameters used
#' @slot horodatedebut An object of class \link{ref_horodate-class}
#' @slot horodatefin An object of class \link{ref_horodate-class}
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new('report_sample_char', ...)}
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @family report Objects
#' @keywords classes
#' @example inst/examples/report_sample_char-example.R
#' @aliases report_sample_char
#' @export
setClass(Class = "report_sample_char", representation = representation(data = "ANY",
    dc = "ref_dc", taxa = "ref_taxa", stage = "ref_stage", par = "ref_par", horodatedebut = "ref_horodate",
    horodatefin = "ref_horodate"), prototype = prototype(data = data.frame(), dc = new("ref_dc"),
    taxa = new("ref_taxa"), stage = new("ref_stage"), par = new("ref_par"), horodatedebut = new("ref_horodate"),
    horodatefin = new("ref_horodate")))

#' connect method for report_sample_char
#' 
#' @param object An object of class \link{report_sample_char-class}
#' @param silent Boolean if TRUE messages are not displayed
#' @return An object of class \link{report_sample_char-class} with slot data \code{@data} filled
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases connect.report_sample_char
setMethod("connect", signature = signature("report_sample_char"), definition = function(object,
    silent = FALSE) {
    requete <- new("RequeteDBwheredate")
    requete@select = paste("SELECT * FROM ", get_schema(),
        "vue_lot_ope_car", sep = "")
    requete@colonnedebut = "ope_date_debut"
    requete@colonnefin = "ope_date_fin"
    requete@datedebut <- object@horodatedebut@horodate
    requete@datefin <- object@horodatefin@horodate
    requete@order_by = "ORDER BY ope_date_debut"
    requete@and = paste(" AND ope_dic_identifiant in ", vector_to_listsql(object@dc@dc_selected),
        " AND lot_tax_code in ", vector_to_listsql(object@taxa@taxa_selected), " AND lot_std_code in ",
        vector_to_listsql(object@stage@stage_selected), " AND car_par_code in ", vector_to_listsql(object@par@par_selected),
        sep = "")
    requete <- stacomirtools::query(requete)
    object@data <- requete@query
    if (!silent)
        funout(gettext("Sample characteristics have been loaded from the database\n",
            domain = "R-stacomiR"))
    return(object)
})


#' charge method for report_sample_char class
#' 
#' this method verifies that boxes have been clicked in the user interface and gets the objects pasted in 
#' envir_stacomi
#' @param object An object of class \link{report_sample_char-class} 
#' @return An object of class \link{report_sample_char-class} with slots filled with user choice
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @return An object of the class \link{report_sample_char-class} with slots filled from values assigned in \code{envir_stacomi} environment
#' @aliases charge.report_sample_char
#' @keywords internal
setMethod("charge", signature = signature("report_sample_char"), definition = function(object) {
    if (exists("ref_dc", envir_stacomi)) {
        object@dc <- get("ref_dc", envir_stacomi)
    } else {
        funout(gettext("You need to choose a counting device, clic on validate\n",
            domain = "R-stacomiR"), arret = TRUE)
    }
    if (exists("ref_taxa", envir_stacomi)) {
        object@taxa <- get("ref_taxa", envir_stacomi)
    } else {
        funout(gettext("You need to choose a taxa, clic on validate\n", domain = "R-stacomiR"),
            arret = TRUE)
    }
    if (exists("ref_stage", envir_stacomi)) {
        object@stage <- get("ref_stage", envir_stacomi)
    } else {
        funout(gettext("You need to choose a stage, clic on validate\n", domain = "R-stacomiR"),
            arret = TRUE)
    }
    if (exists("ref_par", envir_stacomi)) {
        object@par <- get("ref_par", envir_stacomi)
    } else {
        funout(gettext("You need to choose a parameter, clic on validate\n", domain = "R-stacomiR"),
            arret = TRUE)
    }
    # rem pas tres satisfaisant car ce nom est choisi dans l'interface
    if (exists("report_sample_char_date_debut", envir_stacomi)) {
        object@horodatedebut@horodate <- get("report_sample_char_date_debut", envir_stacomi)
    } else {
        funout(gettext("You need to choose the starting date\n", domain = "R-stacomiR"),
            arret = TRUE)
    }
    # rem id
    if (exists("report_sample_char_date_fin", envir_stacomi)) {
        object@horodatefin@horodate <- get("report_sample_char_date_fin", envir_stacomi)
    } else {
        funout(gettext("You need to choose the ending date\n", domain = "R-stacomiR"),
            arret = TRUE)
    }
    assign("report_sample_char", object, envir_stacomi)
    return(object)
})


#' command line interface for report_sample_char class
#' 
#' #' The choice_c method fills in the data slot for classes \link{ref_dc-class}, \link{ref_taxa-class}, \link{ref_stage-class}, \link{ref_par-class} and two slots of \link{ref_horodate-class} and then 
#' uses the choice_c methods of these object to select the data.
#' @param object An object of class \link{report_sample_char-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,ref_dc-method}
#' @param taxa Either a species name in latin or the SANDRE code for species (ie 2038=Anguilla anguilla),
#' these should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,ref_taxa-method}
#' @param stage A stage code matching the ref.tr_stadedeveloppement_std table in the stacomi database, see \link{choice_c,ref_stage-method}
#' @param par A parameter matching th ref.tg_parametre_par table in the stacomi database, see \link{choice_c,ref_par-method}
#' @param horodatedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param horodatefin The finishing date of the report, for this class this will be used to calculate the number of daily steps.
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class \link{report_mig-class} with data selected
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases choice_c.report_sample_char
setMethod("choice_c", signature = signature("report_sample_char"), definition = function(object,
    dc, taxa, stage, par, horodatedebut, horodatefin, silent = FALSE) {
    # code for debug using example
    # report_sample_char<-r_sample_char;dc=c(5,6);taxa='Anguilla anguilla'
    # stage=c('CIV','AGJ');par=c(1785,1786,1787,'C001');horodatedebut='2010-01-01';horodatefin='2015-12-31'
    report_sample_char <- object
    report_sample_char@dc = charge(report_sample_char@dc)
    # loads and verifies the dc this will set dc_selected slot
    report_sample_char@dc <- choice_c(object = report_sample_char@dc, dc)
    # only taxa present in the report_mig are used
    report_sample_char@taxa <- charge_with_filter(object = report_sample_char@taxa,
        report_sample_char@dc@dc_selected)
    report_sample_char@taxa <- choice_c(report_sample_char@taxa, taxa)
    report_sample_char@stage <- charge_with_filter(object = report_sample_char@stage,
        report_sample_char@dc@dc_selected, report_sample_char@taxa@taxa_selected)
    report_sample_char@stage <- choice_c(report_sample_char@stage, stage)
    report_sample_char@par <- charge_with_filter(object = report_sample_char@par,
        report_sample_char@dc@dc_selected, report_sample_char@taxa@taxa_selected,
        report_sample_char@stage@stage_selected)
    report_sample_char@par <- choice_c(report_sample_char@par, par, silent = silent)
    report_sample_char@horodatedebut <- choice_c(object = report_sample_char@horodatedebut,
        nomassign = "report_sample_char_date_debut", funoutlabel = gettext("Beginning date has been chosen\n",
            domain = "R-stacomiR"), horodate = horodatedebut, silent = silent)
    report_sample_char@horodatefin <- choice_c(report_sample_char@horodatefin, nomassign = "report_sample_char_date_fin",
        funoutlabel = gettext("Ending date has been chosen\n", domain = "R-stacomiR"),
        horodate = horodatefin, silent = silent)
    return(report_sample_char)
})

#' Calculation for report_sample_char
#' 
#' In that class, most treatments are done in the query, this method checks that data are available and fills information for year, month, two weeks, week, doy 
#' @param object An object of class \code{\link{report_sample_char-class}} 
#' @param silent Boolean, if TRUE, information messages are not displayed, only warnings and errors
#' @return An object of class \code{\link{report_sample_char-class}} with slot \code{@data} filled
#' @aliases calcule.report_sample_char
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
setMethod("calcule", signature = signature("report_sample_char"), definition = function(object,
    silent = FALSE) {
    # report_sample_char<-r_sample_char
    report_sample_char <- object
    if (nrow(report_sample_char@data) == 0) {
        funout(gettext("No information for these samples during the selected period\n",
            domain = "R-stacomiR"), arret = TRUE)
    }
    vue_ope_lot = report_sample_char@data  # on recupere le data.frame
    nom_variable = report_sample_char@par@data$par_nom[report_sample_char@par@data$par_code %in%
        report_sample_char@par@par_selected]
    # stopifnot(length(nom_variable)==1)
    vue_ope_lot$ope_dic_identifiant = as.factor(vue_ope_lot$ope_dic_identifiant)
    vue_ope_lot$dev_code = as.factor(vue_ope_lot$dev_code)
    vue_ope_lot$car_val_identifiant = as.factor(vue_ope_lot$car_val_identifiant)
    vue_ope_lot$car_par_code = as.factor(vue_ope_lot$car_par_code)
    vue_ope_lot$ope_identifiant = as.factor(vue_ope_lot$ope_identifiant)
    vue_ope_lot$lot_pere = as.factor(vue_ope_lot$lot_pere)
    vue_ope_lot$val_libelle = as.factor(vue_ope_lot$val_libelle)
    vue_ope_lot$lot_tax_code = as.factor(vue_ope_lot$lot_tax_code)
    vue_ope_lot <- fun_date_extraction(data = vue_ope_lot, nom_coldt = "ope_date_debut",
        annee = TRUE, mois = TRUE, quinzaine = TRUE, semaine = TRUE, jour_an = TRUE,
        jour_mois = FALSE, heure = FALSE)
    #   vue_ope_lot=stacomirtools::chnames(vue_ope_lot,
    #     c('ope_identifiant','lot_identifiant','ope_dic_identifiant','lot_pere',
    #       'ope_date_debut','ope_date_fin','lot_effectif','lot_quantite','lot_tax_code','lot_std_code','tax_nom_latin','std_libelle','dev_code','dev_libelle','par_nom','car_par_code','car_methode_obtention','car_val_identifiant',
    # 'car_valeur_quantitatif','val_libelle',
    # 'annee','mois','quinzaine','semaine','jour_365'),
    #     c('ope','lot','dic','lot_pere',
    #       'date','date_fin','effectif','quantite','lot_tax_code','lot_std_code','tax','std','dev_code','dev','par','car_par_code','meth','val','val_quant','val_libelle',
    # 'annee','mois','quinzaine','semaine','jour'))
    # vue_ope_lot=vue_ope_lot[,c('ope','lot','dic','lot_pere','date','effectif','quantite','tax','std','dev','par','meth','val','val_quant','val_libelle',
    # 'annee','mois','quinzaine','semaine','jour')]
    report_sample_char@data <- vue_ope_lot
    assign("report_sample_char", report_sample_char, envir_stacomi)  #assign('report_sample_char',vue_ope_lot,envir_stacomi)
    if (!silent)
        funout(gettext("To obtain the table, type : report_sample_char=get('report_sample_char',envir_stacomi)\n",
            domain = "R-stacomiR"))
    return(report_sample_char)
})


#' Plots of various type for reportcarlot
#' @param x An object of class report_sample_char
#' @param plot.type One of '1','violin plot'. Defaut to \code{1} , can also be \code{2} boxplot or 
#' \code{3} points. 
#' @param silent Stops displaying the messages
#' @return Nothing, called for its side effect, plotting
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases plot.report_sample_char
#' @export
setMethod("plot", signature(x = "report_sample_char", y = "missing"), definition = function(x,
    plot.type = "1", silent = FALSE) {
    # report_sample_char<-r_sample_char;require(ggplot2);plot.type='1'
    # browser()
    report_sample_char <- x
    plot.type <- as.character(plot.type)  # to pass also characters
    if (!plot.type %in% c("1", "2", "3"))
        stop("plot.type must be 1,2,3")
    if (!exists("report_sample_char", envir_stacomi)) {
        if (!silent)
            funout(gettext("No report_sample_char in envir_stacomi, you need to launch calcule() first \n",
                domain = "R-stacomiR"), arret = TRUE)
    }
    name_param <- report_sample_char
    if (plot.type == 1) {
        g <- ggplot(report_sample_char@data, aes(x = car_valeur_quantitatif))
        g <- g + stat_density(aes(ymax = after_stat(density), ymin = -after_stat(density)), fill = "grey50",
            colour = "grey10", geom = "ribbon", position = "identity") + facet_grid(. ~
            annee) + coord_flip()
        print(g)
        assign("g1", g, envir_stacomi)
        if (!silent)
            funout(gettext("To obtain the graphical object, type :  g<-get(\"g1\",envir_stacomi)\n",
                domain = "R-stacomiR"))
    } else if (plot.type == 2) {
        g <- ggplot(report_sample_char@data)
        g <- g + geom_boxplot(aes(x = mois, y = car_valeur_quantitatif, fill = std_libelle)) +
            facet_grid(annee ~ .)
        print(g)
        assign("g2", g, envir_stacomi)
        if (!silent)
            funout(gettext("To obtain the graphical object, type :  g<-get(\"g2\",envir_stacomi)\n",
                domain = "R-stacomiR"))

    } else if (plot.type == 3) {
        g <- ggplot(report_sample_char@data)
        g <- g + geom_point(aes(x = ope_date_debut, y = car_valeur_quantitatif))
        print(g)
        assign("g3", g, envir_stacomi)
        if (!silent)
            funout(gettext("To obtain the graphical object, type :  g<-get(\"g3\",envir_stacomi)\n",
                domain = "R-stacomiR"))
    }
    return(invisible(NULL))
})

#' summary for report_sample_char 
#' 
#' @param object An object of class \code{\link{report_sample_char-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters
#' @return Nothing, called for its side effect of printing a summary
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases summary.report_sample_char
#' @export
setMethod("summary", signature = signature(object = "report_sample_char"), definition = function(object,
    silent = FALSE, ...) {
    Hmisc::describe(object@data)
		return(invisible(NULL))
})

#' Method to print the command line of the object
#' @param x An object of class report_sample_char
#' @param ... Additional parameters passed to print
#' @return NULL
#' @author cedric.briand
#' @aliases print.report_sample_char
#' @export
setMethod("print", signature = signature("report_sample_char"), definition = function(x,
    ...) {
    sortie1 <- "report_sample_char=new('report_sample_char')"
    sortie2 <- stringr::str_c("report_sample_char <- choice_c(report_sample_char,",
        "dc=c(", stringr::str_c(x@dc@dc_selected, collapse = ","), "),", "taxa=c(",
        stringr::str_c(shQuote(x@taxa@data$tax_nom_latin), collapse = ","), "),",
        "stage=c(", stringr::str_c(shQuote(x@stage@stage_selected), collapse = ","),
        "),", "par=c(", stringr::str_c(shQuote(x@par@par_selected), collapse = ","),
        "),", "horodatedebut=", shQuote(strftime(x@horodatedebut@horodate, format = "%d/%m/%Y %H-%M-%S")),
        ",horodatefin=", shQuote(strftime(x@horodatefin@horodate, format = "%d/%m/%Y %H-%M-%S")),
        ")")
    # removing backslashes
    funout(sortie1)
    funout(stringr::str_c(sortie2, ...))
    return(invisible(NULL))
})

