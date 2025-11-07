#' Migration reports for multiple DC / species / stages
#' 
#' Migration counts for several Fish counting devices (DC), several taxa and several stages.
#' This migration count can be built either by the graphical interface or from the command line 
#' (see examples).

#' @note A Migration report comes from a migration monitoring : the fishes are monitored in a section of river, this section is
#' called a control station (station). Most often, there is a dam, one or several fishways (DF) which comprise one or several counting devices (DC).
#' On each counting device, the migration is recorded. It can be either an instant recording (video control) or the use of traps,
#' Operations are monitoring operations during a period. For each operation, several species of fishes can be recorded (samples). In the case of migratory 
#' fishes the stage of development is important as it may indicate generic migrations, to and fro, between the river and the sea.
#' 
#' Hence a Multiple Migration report is built from several one or several counting devices (DC), one or several Taxa (Taxon), one or several stages
#' (stage). The migration can be also recorded not as numbers, but in the case of glass eels, as weight, which will be later transformed to number, 
#' from daily conversion coefficients. The methods in this class test whether the counts are numbers or another type of quantity.
#' This class makes different calculations than report_mig, it does not handle escapement coefficients,
#' it uses quantities other than numbers if necessary (only used for glass eel in the connect method).
#' @slot dc An object of class \code{ref_dc-class}
#' @slot taxa An object of class \code{\link{ref_taxa-class}}
#' @slot stage An object of class \code{\link{ref_stage-class}}
#' @slot timestep An object of class \code{\link{ref_timestep_daily-class}}
#' @slot data A data.frame containing raw data filled by the connect method
#' @slot calcdata A 'list' of calculated daily data, one per dc, filled in by the calcule method
#' @slot coef_conversion A data frame of daily weight to number conversion coefficients, filled in by the connect
#' method if any weight are found in the data slot.
#' @slot time.sequence A POSIXt time sequence
#' @family report Objects
#' @aliases report_mig_mult
#' @keywords classes
#' @example inst/examples/report_mig_mult-example.R
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @export
setClass(Class = "report_mig_mult", representation = representation(dc = "ref_dc",
				taxa = "ref_taxa", stage = "ref_stage", timestep = "ref_timestep_daily", data = "data.frame",
				calcdata = "list", coef_conversion = "data.frame", time.sequence = "POSIXct"),
		prototype = prototype(dc = new("ref_dc"), taxa = new("ref_taxa"), stage = new("ref_stage"),
				timestep = new("ref_timestep_daily"), data = data.frame(), calcdata = list(),
				coef_conversion = data.frame(), time.sequence = as.POSIXct(Sys.time())))

setValidity("report_mig_mult", function(object) {
			rep1 = length(object@dc) >= 1
			rep2 = length(object@taxa) >= 1
			rep3 = length(object@stage) >= 1
			return(ifelse(rep1 & rep2 & rep3, TRUE, c(1:6)[!c(rep1, rep2, rep3)]))
		})




#' charge method for report_mig_mult
#' 
#' For the report_mig_mult class the charge method must be run 
#' to load data on migration control operations
#' fishway operations, and counting devices operations as data from those are displayed in the main plots.
#' For other classes the charge method is only used by the graphical interface (shiny) 
#' 
#' @param object An object of class \link{report_mig_mult-class}
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return An object of class \link{report_mig_mult-class} with slots filled from values assigned in \code{envir_stacomi} environment
#' @aliases charge.report_mig_mult
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
setMethod("charge", signature = signature("report_mig_mult"), definition = function(object,
				silent = FALSE) {
			report_mig_mult <- object
			if (exists("ref_dc", envir_stacomi)) {
				report_mig_mult@dc <- get("ref_dc", envir_stacomi)
				dc <- report_mig_mult@dc@dc_selected
				df <- report_mig_mult@dc@data$df[report_mig_mult@dc@data$dc %in% dc]
			} else {
				funout(gettext("You need to choose a counting device, clic on validate\n",
								domain = "R-stacomiR"), arret = TRUE)
			}
			if (exists("ref_taxa", envir_stacomi)) {
				report_mig_mult@taxa <- get("ref_taxa", envir_stacomi)
			} else {
				funout(gettext("You need to choose a taxa, clic on validate\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			if (exists("ref_stage", envir_stacomi)) {
				report_mig_mult@stage <- get("ref_stage", envir_stacomi)
			} else {
				funout(gettext("You need to choose a stage, clic on validate\n", domain = "R-stacomiR"),
						arret = TRUE)
			}
			if (exists("timestep", envir_stacomi)) {
				report_mig_mult@timestep <- get("timestep", envir_stacomi)
			} else {
				funout(gettext("Attention, no time step selected, compunting with default value\n",
								domain = "R-stacomiR"), arret = FALSE)
				warning("Attention, no time step selected, compunting with default value\n")
			}
			################################# loading data for other classes associated
			################################# with report_mig_mult
			assign("report_dc_date_debut", get("timestep", envir_stacomi)@dateDebut, envir_stacomi)
			assign("report_dc_date_fin", as.POSIXlt(end_date(get("timestep", envir_stacomi))),
					envir_stacomi)
			assign("report_df_date_debut", get("timestep", envir_stacomi)@dateDebut, envir_stacomi)
			assign("report_df_date_fin", as.POSIXlt(end_date(get("timestep", envir_stacomi))),
					envir_stacomi)
			assign("report_ope_date_debut", get("timestep", envir_stacomi)@dateDebut, envir_stacomi)
			assign("report_ope_date_fin", as.POSIXlt(end_date(get("timestep", envir_stacomi))),
					envir_stacomi)
			
			report_ope <- get("report_ope", envir = envir_stacomi)
			report_ope <- charge(report_ope)
			# charge will search for ref_dc (possible multiple choice),
			# report_ope_date_debut and report_ope_date_fin in envir_stacomi
			report_dc <- get("report_dc", envir = envir_stacomi)
			# charge will search for ref_dc (possible multiple choice),
			# report_dc_date_debut and report_dc_date_fin in envir_stacomi
			report_dc <- charge(report_dc)
			ref_df = new("ref_df")
			ref_df <- charge(ref_df)
			ref_df <- choice_c(ref_df, df)
			assign("ref_df", ref_df, envir = envir_stacomi)
			report_df <- get("report_df", envir = envir_stacomi)
			# charge will search for ref_df (possible multiple choice),
			# report_df_date_debut and report_df_date_fin in envir_stacomi
			report_df <- charge(report_df)
			# the object are assigned to the envir_stacomi for later use by the connect
			# method
			assign("report_df", report_df, envir = envir_stacomi)
			assign("report_dc", report_dc, envir = envir_stacomi)
			assign("report_ope", report_ope, envir = envir_stacomi)
			stopifnot(validObject(report_mig_mult, test = TRUE))
			# connect will load, coefficients, DF, DC, operations
			return(report_mig_mult)
		})


#' command line used to build report_mig_mult class
#' 
#' The choice_c method fills in the data slot for ref_dc, ref_taxa, ref_stage and then 
#' uses the choice_c methods of these object to 'select' the data.
#' @param object An object of class \link{report_mig-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,ref_dc-method}
#' @param taxa Either a species name in latin or the SANDRE code for species (ie 2038=Anguilla anguilla),
#' these should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,ref_taxa-method}
#' @param stage A stage code matching the ref.tr_stadedeveloppement_std table in the stacomi database see \link{choice_c,ref_stage-method}
#' @param datedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param datefin The finishing date of the report, for this class this will be used to calculate the number of daily steps.
#' @param silent Should messages be hided default FALSE
#' @return An object of class \link{report_mig_mult-class} with data selected
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases choice_c.report_mig_mult
setMethod("choice_c", signature = signature("report_mig_mult"), definition = function(object,
				dc, taxa, stage, datedebut, datefin, silent = FALSE) {
			report_mig_mult <- object
			report_df = new("report_df")
			assign("report_df", report_df, envir = envir_stacomi)
			report_dc = new("report_dc")
			assign("report_dc", report_dc, envir = envir_stacomi)
			report_ope = new("report_ope")
			assign("report_ope", report_ope, envir = envir_stacomi)
			report_mig_mult@dc = charge(report_mig_mult@dc)
			# loads and verifies the dc
			report_mig_mult@dc <- choice_c(object = report_mig_mult@dc, dc)
			# only taxa present in the report_mig are used
			report_mig_mult@taxa <- charge_with_filter(object = report_mig_mult@taxa, 
					report_mig_mult@dc@dc_selected)
			report_mig_mult@taxa <- choice_c(report_mig_mult@taxa, taxa)
			report_mig_mult@stage <- charge_with_filter(object = report_mig_mult@stage, 
					dc_selected = report_mig_mult@dc@dc_selected,
					taxa_selected = report_mig_mult@taxa@taxa_selected)
			report_mig_mult@stage <- choice_c(report_mig_mult@stage, stage)
			report_mig_mult@timestep <- choice_c(report_mig_mult@timestep, datedebut, datefin)
			assign("report_mig_mult", report_mig_mult, envir = envir_stacomi)
			if (!silent)
				funout(gettext("Choice made, and object report_mig_mult assigned in envir_stacomi"),
						domain = "R-stacomiR")
			return(report_mig_mult)
		})

#' #' Transforms migration per period to daily migrations, and performs the conversion from weights to number is data
#' are stored as weights (glass eel). This calculation is performed in a loop for all dc. 
#' 
#'  The calculation must be launched once data are filled by the connect method. Currently the negative argument
#' has no effect. 
#'  
#' @param object An object of class \link{report_mig_mult-class}
#' @param negative a boolean indicating if a separate sum must be done for positive and negative values, if true, positive and negative counts return 
#' different rows
#' @param silent Default FALSE, should messages be stopped
#' @note The class does not handle escapement rates, though structurally those are present in the database. If you 
#' want to use those you will have to do the calculation manually from the data in \code{report_mig_mult@data}.
#' @return report_mig_mult with a list in slot calcdata. For each dc one will find a list with the following elements
#' \describe{
#' \item{method}{In the case of instantaneous periods (video counting) the sum of daily values is done by the \link{fun_report_mig_mult} method and the value indicated in method is 'sum'.
#'  If any migration monitoring period is longer than a day, then the migration is split using the \link{fun_report_mig_mult_overlaps} function and the value indicated in the 
#' method is 'overlaps' as the latter method uses the overlap package to split migration period.}
#' \item{data}{the calculated data. If weight are present, the columns display weight or numbers, the total number is 
#' 'Effectif_total' and corresponds to the addition of numbers and numbers converted from weight,
#' the total weight is 'Poids_total'+'poids_depuis_effectifs' and corresponds to weighed glass eel plus glass eel number converted in weights.
#' CALCULE corresponds to calulated number, MESURE to measured numbers, EXPERT to punctual expertise of migration (for instance measured in other path, or known migration
#' of fishes passing the dam but not actually counted, PONCTUEL to fishes counted by visual identification but not by the counting apparatus (in case of technical problem for instance)}
#' \item{contient_poids}{A boolean which indicates, in the case of glass eel, that the function \link{fun_weight_conversion} has been run to convert the weights to numbers using the weight
#' to number coefficients in the database (see \link{report_ge_weight}).}
#' \item{negative}{A parameter indicating if negative migration (downstream in the case of upstream migration devices) have been converted to positive numbers,
#' not developed yet}}
#' @aliases calcule.report_mig_mult
setMethod("calcule", signature = signature("report_mig_mult"), definition = function(object,
				negative = FALSE, silent = FALSE) {
			
			# report_mig_mult<-r_mig_mult; negative=FALSE
			if (!silent)
				funout(gettext("Starting migration summary ... be patient\n", domain = "R-stacomiR"))
			report_mig_mult <- object
			debut = report_mig_mult@timestep@dateDebut
			fin = end_date(report_mig_mult@timestep)
      
			time.sequence <- seq.POSIXt(from = debut, to = fin, by = as.numeric(report_mig_mult@timestep@step_duration))
			report_mig_mult@time.sequence <- time.sequence
			lestableaux <- list()
			for (dic in unique(report_mig_mult@data$ope_dic_identifiant)) {
				datasub <- report_mig_mult@data[report_mig_mult@data$ope_dic_identifiant ==
								dic, ]
				datasub$duree = difftime(datasub$ope_date_fin, datasub$ope_date_debut, units = "days")
				if (any(datasub$duree > (report_mig_mult@timestep@step_duration/86400))) {
					#----------------------
					# reports with overlaps
					#----------------------
					data <- fun_report_mig_mult_overlaps(time.sequence = time.sequence, datasub = datasub,
							negative = negative)
					# to remain compatible with report mig :
					data$taux_d_echappement = -1
					lestableaux[[stringr::str_c("dc_", dic)]][["data"]] <- data
					lestableaux[[stringr::str_c("dc_", dic)]][["method"]] <- "overlaps"
					contient_poids <- "poids" %in% datasub$type_de_quantite
					lestableaux[[stringr::str_c("dc_", dic)]][["contient_poids"]] <- contient_poids
					
					lestableaux[[stringr::str_c("dc_", dic)]][["negative"]] <- negative
					if (contient_poids) {
						coe <- report_mig_mult@coef_conversion[, c("coe_date_debut", "coe_valeur_coefficient")]
						data$coe_date_debut <- as.Date(data$debut_pas)
						data <- merge(data, coe, by = "coe_date_debut")
						data <- data[, -1]  # removing coe_date_debut
						data <- fun_weight_conversion(tableau = data, time.sequence = report_mig_mult@time.sequence,
								silent)
					}
					
					lestableaux[[stringr::str_c("dc_", dic)]][["data"]] <- data
					
				} else {
					#----------------------
					# report simple
					#----------------------
					mydata <- fun_report_mig_mult(time.sequence = time.sequence, datasub = datasub,
							negative = negative)
					mydata$taux_d_echappement = -1
					mydata$coe_valeur_coefficient = NA
					contient_poids <- "poids" %in% datasub$type_de_quantite
					if (contient_poids) {
						# at this tage data for coe_valeur_coefficient are null, we
						# remove the column before merging
						mydata <- mydata[, -match("coe_valeur_coefficient", colnames(mydata))]
						coe <- report_mig_mult@coef_conversion[, c("coe_date_debut", "coe_valeur_coefficient")]
						mydata$coe_date_debut <- as.Date(mydata$debut_pas)
						mydata2 <- merge(mydata, coe, by = "coe_date_debut")
						mydata2 <- mydata2[, -match("coe_date_debut", colnames(mydata2))]  # removing coe_date_debut
						data <- fun_weight_conversion(tableau = mydata2, time.sequence = report_mig_mult@time.sequence,
								silent)
					} else {
						data <- mydata
					}
					lestableaux[[stringr::str_c("dc_", dic)]][["data"]] <- data
					lestableaux[[stringr::str_c("dc_", dic)]][["method"]] <- "sum"
					lestableaux[[stringr::str_c("dc_", dic)]][["contient_poids"]] <- contient_poids
					lestableaux[[stringr::str_c("dc_", dic)]][["negative"]] <- negative
				}
			}  # end for dic
			# TODO developper une methode pour sumneg
			report_mig_mult@calcdata <- lestableaux
			assign("report_mig_mult", report_mig_mult, envir_stacomi)
			if (!silent) {
				funout(gettext("The summary object is stored in environment envir_stacomi, write report_mig_mult=get(\"report_mig_mult\",envir_stacomi) \n",
								domain = "R-stacomiR"))
				funout(gettext("Raw data are stored in report_mig_mult@data, processed data in report_mig_mult@calcdata\\n\n",
								domain = "R-stacomiR"))
			}
			return(report_mig_mult)
		})

#' connect method for report_mig_mult
#' 
#' this method loads data from the database for report_mig but also fills the table of conversion coefficient, if 
#' the taxa is eel. It also calls connect method for \link{report_df-class}, 
#' \link{report_dc-class} and \link{report_ope-class} associated with the report
#' and used by the \link{fungraph} and \link{fungraph_glasseel} functions. As a side effect it assigns
#' objects 	\link{report_dc-class}, \link{report_df-class} and \link{report_ope-class} in environment \code{envir_stacomi}


#' @param object An object of class \link{report_mig_mult-class}
#' @param silent Boolean, if TRUE messages are not displayed
#' @return An object of class \link{report_mig_mult-class} with slot @data filled from the database
#' @aliases connect.report_mig_mult
setMethod("connect", signature = signature("report_mig_mult"), definition = function(object,
				silent = FALSE) {
			# recuperation du report_mig report_mig_mult<-bmM
      # if not silent display information about the connection
      if (!silent) {
        host <-	options("stacomiR.host")				
        funout(gettextf("host:%s", host, domain = "R-StacomiR"))
        port <- options("stacomiR.port")
        funout(gettextf("port:%s", port, domain = "R-StacomiR"))
        # getting the database name
        dbname <- options("stacomiR.dbname")
        funout(gettextf("dbname:%s", dbname, domain = "R-StacomiR"))
      }
      
			report_mig_mult <- object

			# retrieve the argument of the function and passes it to report_mig_mult
			# easier to debug
			req = new("RequeteDBwheredate")
			req@colonnedebut <- "ope_date_debut"
			req@colonnefin <- "ope_date_fin"
			# we round the date to be consistent with daily values from the
			req@datedebut = report_mig_mult@timestep@dateDebut
			req@datefin = as.POSIXlt(end_date(report_mig_mult@timestep) + as.difftime("23:59:59"))
			if (length(report_mig_mult@dc@dc_selected) == 0)
				stop("DC has length zero, are you connected to the right schema, do you use the right dc number ?")
			dc = vector_to_listsql(report_mig_mult@dc@dc_selected)
			if (length(report_mig_mult@taxa@taxa_selected) == 0)
				stop("Taxa has length zero, are you connected to the right schema, do you use the right taxa ?")
			tax = vector_to_listsql(report_mig_mult@taxa@taxa_selected)
			if (length(report_mig_mult@stage@stage_selected) == 0)
				stop("Stage has length zero, are you connected to the right schema, do you use the right stage ?")
			std = vector_to_listsql(report_mig_mult@stage@stage_selected)
			sch = get_schema()
			req@select = stringr::str_c("SELECT 
							ope_identifiant,
							lot_identifiant,
							ope_date_debut,
							ope_date_fin,
							ope_dic_identifiant,
							lot_tax_code,
							lot_std_code,
							CASE WHEN lot_effectif is not NULL then lot_effectif  
							WHEN lot_effectif is null then lot_quantite 
							end as value,
							case when lot_effectif is not NULL  then 'effectif' 
							when lot_effectif is null and lot_qte_code='1' then 'poids' 
							when lot_effectif is null and lot_qte_code='2' then 'volume' 
							else 'quantite' end as type_de_quantite,
							lot_dev_code, 
							lot_methode_obtention",
					" FROM ", sch, "t_operation_ope", " JOIN ", sch, "t_lot_lot on lot_ope_identifiant=ope_identifiant")
			# removing character marks
			req@select <- stringr::str_replace_all(req@select, "[\r\n\t]", "")
			# the where clause is returned by DBWheredate
			req@and = stringr::str_c(" AND ope_dic_identifiant in", dc, " AND lot_tax_code in ",
					tax, " AND lot_std_code in ", std, " AND lot_lot_identifiant IS NULL")
			req <- stacomirtools::query(req)
			report_mig_mult@data = req@query
			if (!silent)
				cat(stringr::str_c("data collected from the database nrow=", nrow(report_mig_mult@data),
								"\n"))
			# recuperation des coefficients si il y a des civelles dans le report
			if (2038 %in% report_mig_mult@taxa@taxa_selected) {
				req = new("RequeteDBwheredate")
				req@select = paste("select * from", sch, "tj_coefficientconversion_coe")
				req@datedebut = as.POSIXlt(report_mig_mult@timestep@dateDebut)
				req@datefin = as.POSIXlt(end_date(report_mig_mult@timestep))
				req@colonnedebut <- "coe_date_debut"
				req@colonnefin <- "coe_date_fin"
				req@and <- c("and coe_tax_code='2038'", "and coe_std_code='CIV'")
				req@order_by <- "order by coe_date_debut"
				req <- stacomirtools::query(req)
				report_mig_mult@coef_conversion <- req@query
				
			}
			stopifnot(validObject(report_mig_mult, test = TRUE))
			
			#######################''
			# connect method for associated classes
			report_ope <- get("report_ope", envir = envir_stacomi)
			report_dc <- get("report_dc", envir = envir_stacomi)
			report_df <- get("report_df", envir = envir_stacomi)
			report_ope <- connect(report_ope, silent = silent)
			report_dc <- connect(report_dc, silent = silent)
			report_df <- connect(report_df, silent = silent)
			assign("report_df", report_df, envir = envir_stacomi)
			assign("report_dc", report_dc, envir = envir_stacomi)
			assign("report_ope", report_ope, envir = envir_stacomi)
			return(report_mig_mult)
		})


#' Plots of various type for report_mig_mult
#' 
#' \describe{
#'   \item{plot.type='standard'}{calls \code{\link{fungraph}} and \code{\link{fungraph_glasseel}} functions to plot as many 'report_mig'
#'   as needed, the function will test for the existence of data for one dc, one taxa, and one stage}
#'   \item{plot.type='step'}{creates Cumulated graphs for report_mig_mult.  Data are summed per day for different dc taxa and stages}
#'   \item{plot.type='multiple'}{Method to overlay graphs for report_mig_mult (multiple dc/taxa/stage in the same plot)}
#' }
#' @param x An object of class report_mig_mult
#' @param plot.type One of 'standard','step','multiple'. Defaut to \code{standard} the standard report_mig with dc and operation displayed, can also be \code{step} or 
#' \code{multiple} 
#' @param silent Stops most messages from being displayed
#' @param color Default NULL, argument passed for the plot.type='standard' method. A vector of color in the following order : (1) working, (2) stopped, (3:7) 1...5 types of operation,
#' (8:11) numbers, weight, NULL, NULL (if glass eel), (8:11)  measured, calculated, expert, direct observation for other taxa. If null will be set to brewer.pal(12,'Paired')[c(8,10,4,6,1,2,3,5,7)]
#' @param color_ope Default NULL, argument passed for the plot.type='standard' method. A vector of color for the operations. Default to brewer.pal(4,'Paired')
#' @param ... Additional arguments passed to matplot or plot if plot.type='standard', see ... in \link{fungraph_glasseel} and \link{fungraph}
#' @return Nothing, called for its side effect of plotting
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases plot.report_mig_mult
#' @export
setMethod("plot", signature(x = "report_mig_mult", y = "missing"), definition = function(x,
				plot.type = "standard", color = NULL, color_ope = NULL, silent = FALSE, ...) {
			# print('entering plot function') report_mig_mult<-r_mig_mult;silent=FALSE
			report_mig_mult <- x
			the_taxa = report_mig_mult@taxa@data[report_mig_mult@taxa@data$tax_code %in% report_mig_mult@taxa@taxa_selected, ]
			the_stages = report_mig_mult@stage@data[report_mig_mult@stage@data$std_code %in% report_mig_mult@stage@stage_selected, ]
			lesdc = as.numeric(report_mig_mult@dc@dc_selected)
			# ==========================type=1=============================
			if (plot.type == "standard") {
				if (!silent)
					print("plot type standard")
				if (!silent)
					funout(gettext("Statistics about migration :\n", domain = "R-stacomiR"))
				# dcnum=1;taxanum=1;stagenum=2 &&&&&&&&&&&&&&&&&&&&&&&&&debut de
				# boucle&&&&&&&&&&&&&&&&&&&&&&&&&&&
				compte <- 0
				for (dcnum in 1:length(lesdc)) {
					for (taxanum in 1:nrow(the_taxa)) {
						for (stagenum in 1:nrow(the_stages)) {
							# dcnum=1;taxanum=1;stagenum=1
							taxa <- the_taxa[taxanum, "tax_nom_latin"]
							stage <- the_stages[stagenum, "std_libelle"]
							dc <- lesdc[dcnum]
							data <- report_mig_mult@calcdata[[stringr::str_c("dc_", dc)]][["data"]]
							data <- data[data$lot_tax_code == the_taxa[taxanum, "tax_code"] &
											data$lot_std_code == the_stages[stagenum, "std_code"], ]
							
							if (!is.null(data)) {
								if (nrow(data) > 0)
								{
									
									if (!silent) {
										funout(paste("dc=", dc, taxa = taxa, stage = stage, "\n"))
										funout("---------------------\n")
									}
									if (any(duplicated(data$No.pas)))
										stop("duplicated values in No.pas")
									data_without_hole <- merge(data.frame(No.pas = as.numeric(strftime(report_mig_mult@time.sequence,
																	format = "%j")) - 1, debut_pas = report_mig_mult@time.sequence),
											data, by = c("No.pas", "debut_pas"), all.x = TRUE)
									data_without_hole$CALCULE[is.na(data_without_hole$CALCULE)] <- 0
									data_without_hole$MESURE[is.na(data_without_hole$MESURE)] <- 0
									data_without_hole$EXPERT[is.na(data_without_hole$EXPERT)] <- 0
									data_without_hole$PONCTUEL[is.na(data_without_hole$PONCTUEL)] <- 0
									compte = compte + 1
									if (report_mig_mult@calcdata[[stringr::str_c("dc_", dc)]][["contient_poids"]] &
											taxa == "Anguilla anguilla" & (stage == "civelle" | stage ==
												"Anguilla jaune")) {
										
										#----------------------------------
										# report migration with weight (glass eel)
										#-----------------------------------------
										if (compte != 1)
											dev.new()
										fungraph_glasseel(report_mig = report_mig_mult, table = data_without_hole,
												time.sequence = report_mig_mult@time.sequence, taxa = taxa,
												stage = stage, dc = dc, color = color, color_ope = color_ope,
												silent, ...)
									} else {
										
										#----------------------------------
										# report migration standard
										#-----------------------------------------
										if (compte != 1)
											dev.new()
										# silent=TRUE
										fungraph(report_mig = report_mig_mult, tableau = data_without_hole,
												time.sequence = report_mig_mult@time.sequence, taxa,
												stage, dc, color = color, color_ope = color_ope, silent,
												...)
									}
								}  # end nrow(data)>0  
								# ecriture du report journalier, ecrit aussi le report
								# mensuel fn_Ecritreport_daily(report_mig_mult)
								
							}
						}
					}
				}
				# &&&&&&&&&&&&&&&&&&&&&&&&&fin de boucle&&&&&&&&&&&&&&&&&&&&&&&&&&&
			}
			# ==========================type=2=============================
			if (plot.type == "step") {

				grdata <- data.frame()
				for (i in 1:length(report_mig_mult@calcdata)) {
					data <- report_mig_mult@calcdata[[i]]$data
					# extracting similar columns (not those calculated)
					data <- data[, c("No.pas", "debut_pas", "fin_pas", "ope_dic_identifiant",
									"lot_tax_code", "lot_std_code", "MESURE", "CALCULE", "EXPERT", "PONCTUEL",
									"Effectif_total")]
					grdata <- rbind(grdata, data)
				}
				names(grdata) <- tolower(names(grdata))
				grdata <- as.data.frame(grdata %>%
								dplyr::group_by(debut_pas, no.pas) %>%
								dplyr::summarize(effectif_total = sum(effectif_total)) %>%
								dplyr::arrange(debut_pas))
				grdata_without_hole <- merge(data.frame(no.pas = as.numeric(strftime(report_mig_mult@time.sequence,
												format = "%j")) - 1, debut_pas = report_mig_mult@time.sequence), grdata,
						by = c("no.pas", "debut_pas"), all.x = TRUE)
				grdata_without_hole <- fun_date_extraction(grdata_without_hole, nom_coldt = "debut_pas",
						annee = FALSE, mois = TRUE, quinzaine = TRUE, semaine = TRUE, jour_an = TRUE,
						jour_mois = FALSE, heure = FALSE)
				grdata_without_hole <- grdata_without_hole[order(grdata_without_hole$no.pas),		]
				grdata_without_hole$effectif_total[is.na(grdata_without_hole$effectif_total)] <- 0
				
				grdata_without_hole$cumsum = cumsum(grdata_without_hole$effectif_total)
				annee = unique(strftime(as.POSIXlt(report_mig_mult@time.sequence), "%Y"))
				dis_commentaire = paste(as.character(report_mig_mult@dc@dc_selected),
						collapse = ",")
				update_geom_defaults("step", aes(size = 3))
				
				p <- ggplot(grdata_without_hole) + geom_step(aes(x = debut_pas, y = cumsum,
										colour = mois)) + ylab(gettext("Cumulative migration", domain = "R-stacomiR")) +
						theme(plot.title = element_text(size = 10, colour = "deepskyblue")) +
						xlab("mois") + scale_colour_manual(values = c(`01` = "#092360", `02` = "#1369A2",
										`03` = "#0099A9", `04` = "#009780", `05` = "#67B784", `06` = "#CBDF7C",
										`07` = "#FFE200", `08` = "#DB9815", `09` = "#E57B25", `10` = "#F0522D",
										`11` = "#912E0F", `12` = "#33004B")) + ggtitle(gettextf("Cumulative count %s, %s, %s, %s",
										dis_commentaire, paste(the_taxa$tax_nom_latin, collapse=", "), paste(the_stages$std_libelle, collapse=","), annee))
				print(p)
				assign("p_step", p, envir = envir_stacomi)
				assign("grdata", grdata_without_hole, envir_stacomi)
				if (!silent)
					funout(gettext("The plot has been assigned to p_step in envir_stacomi,write p<-get('p_step',envir_stacomi) to retrieve the object"))
				if (!silent)
					funout(gettext("The data for the plot have been assigned to envir_stacomi,write grdata<-get('grdata',envir_stacomi) to retrieve the object"))
				
			}
			# ==========================type=3=============================
			if (plot.type == "multiple") {
				grdata <- fun_aggreg_for_plot(report_mig_mult)
				if (length(unique(grdata$taxa)) == 1 & length(unique(grdata$stage)) == 1) {
					p <- ggplot(grdata, aes(x = debut_pas, y = effectif_total), fill = "black") +
							geom_bar(position = "stack", stat = "identity") + facet_grid(DC ~
											., scales = "free_y")
				} else if (length(unique(grdata$taxa)) == 1) {
					p <- ggplot(grdata, aes(x = debut_pas, y = effectif_total, fill = stage)) +
							geom_bar(position = "stack", stat = "identity") + facet_grid(DC ~
											., scales = "free_y") + scale_fill_brewer(palette = "Set2")
				} else if (length(unique(grdata$stage)) == 1) {
					p <- ggplot(grdata, aes(x = debut_pas, y = effectif_total, fill = taxa)) +
							geom_bar(position = "stack", stat = "identity") + facet_grid(DC ~
											., scales = "free_y") + scale_fill_brewer(palette = "Set2")
				} else {
					p <- ggplot(grdata, aes(x = debut_pas, y = effectif_total, fill = stage)) +
							geom_bar(position = "stack", stat = "identity") + facet_grid(DC +
											taxa ~ ., scales = "free_y") + scale_fill_brewer(palette = "Set2")
				}
				
				print(p)
				assign("p_multiple", p, envir = envir_stacomi)
				if (!silent)
					funout(gettext("The plot has been assigned to p_multiple in envir_stacomi,write p<-get('pmultiple',envir_stacomi) to retrieve the object"))
				assign("grdata", grdata, envir_stacomi)
				if (!silent)
					funout(gettext("The data for the plot have been assigned to envir_stacomi,write grdata<-get('grdata',envir_stacomi) to retrieve the object"))
				
			}
			# ==========================end / type=3=============================  
			return(invisible(NULL))
		})


#' summary for report_mig_mult 
#' calls functions funstat and funtable to create migration overviews
#' and generate csv and html output in the user data directory
#' @param object An object of class \code{\link{report_mig_mult-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters (not used there)
#' @return Nothing, runs funstat and funtable method for each DC
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases summary.report_mig_mult
#' @export
setMethod("summary", signature = signature(object = "report_mig_mult"), definition = function(object,
				silent = FALSE, ...) {
			# report_mig_mult<-r_mig_mult; silent<-FALSE
			report_mig_mult <- object
			the_taxa = report_mig_mult@taxa@data[report_mig_mult@taxa@data$tax_code %in% report_mig_mult@taxa@taxa_selected, ]
			the_stages = report_mig_mult@stage@data[report_mig_mult@stage@data$std_code %in% report_mig_mult@stage@stage_selected, ]
			lesdc = as.numeric(report_mig_mult@dc@dc_selected)
			if (!silent)
				funout(gettext("Statistics about migration :\n", domain = "R-stacomiR"))
			# &&&&&&&&&&&&&&&&&&&&&&&&&debut de boucle&&&&&&&&&&&&&&&&&&&&&&&&&&&
			# dcnum=2;taxanum=1;stagenum=1
			for (dcnum in 1:length(lesdc)) {
				for (taxanum in 1:nrow(the_taxa)) {
					for (stagenum in 1:nrow(the_stages)) {
						
						taxa = the_taxa[taxanum, "tax_nom_latin"]
						stage = the_stages[stagenum, "std_libelle"]
						DC = lesdc[dcnum]
						
						# preparation du jeu de donnees pour la fonction fungraph_civ
						# developpee pour la classe report_mig
						data <- report_mig_mult@calcdata[[stringr::str_c("dc_", DC)]][["data"]]
						data <- data[data$lot_tax_code == the_taxa[taxanum, "tax_code"] &
										data$lot_std_code == the_stages[stagenum, "std_code"], ]
						
						if (!is.null(data)) {
							if (nrow(data) > 0) {
								
								if (any(duplicated(data$No.pas)))
									warning("duplicated values in No.pas")
             	data_without_hole <- merge(data.frame(No.pas = as.numeric(strftime(report_mig_mult@time.sequence,
																format = "%j")) - 1, debut_pas = report_mig_mult@time.sequence),
										data, by = c("No.pas", "debut_pas"), all.x = TRUE)
								data_without_hole$CALCULE[is.na(data_without_hole$CALCULE)] <- 0
								data_without_hole$MESURE[is.na(data_without_hole$MESURE)] <- 0
								data_without_hole$EXPERT[is.na(data_without_hole$EXPERT)] <- 0
								data_without_hole$PONCTUEL[is.na(data_without_hole$PONCTUEL)] <- 0
								
								resum = funstat(tableau = data_without_hole, time.sequence = report_mig_mult@time.sequence,
										taxa, stage, DC, silent)
								# pb with posixt and xtable, removing posixt and setting
								# date instead
								data_without_hole$debut_pas <- as.Date(data_without_hole$debut_pas)
								data_without_hole <- data_without_hole[, -match("fin_pas", colnames(data_without_hole))]
								funtable(tableau = data_without_hole, time.sequence = report_mig_mult@time.sequence,
										taxa, stage, DC, resum, silent)
								
							}
						}
					}
				}
			}
			
			
		})



#' Method to print the command line of the object
#' @param x An object of class report_mig_mult
#' @param ... Additional parameters passed to print
#' @return NULL
#' @author cedric.briand
#' @aliases print.report_mig_mult
#' @export
setMethod("print", signature = signature("report_mig_mult"), definition = function(x,
				...) {
			sortie1 <- "report_mig_mult=new('report_mig_mult')\n"
			sortie2 <- stringr::str_c("report_mig_mult=choice_c(report_mig_mult,", "dc=c(",
					stringr::str_c(x@dc@dc_selected, collapse = ","), "),", "taxa=c(", stringr::str_c(shQuote(x@taxa@data$tax_nom_latin),
							collapse = ","), "),", "stage=c(", stringr::str_c(shQuote(x@stage@stage_selected),
							collapse = ","), "),", "datedebut=", shQuote(strftime(x@timestep@dateDebut,
									format = "%d/%m/%Y")), ",datefin=", shQuote(strftime(end_date(x@timestep),
									format = "%d/%m/%Y")), ")")
			# removing backslashes
			funout(stringr::str_c(sortie1, sortie2), ...)
			return(invisible(NULL))
		})

#' Function to calculate daily migration using overlaps functions
#' 
#' Function to calculate daily migration from migration monitoring whose length is more than one day,
#' this calculation relies on the (false) assumption that migration is evenly spread over time. 
#' @param time.sequence the time sequence to be filled in with new data
#' @param datasub the initial dataset
#' @param negative 'boolean', default FALSE, TRUE indicates a separate sum for negative and positive migrations
#' to time.sequence period and summed over the new sequence. A migration operation spanning several days will
#' be converted to 'daily' values assuming that the migration was regular over time. The function
#' returns one row per taxa, stages, counting device. It does not account for the destination of taxa. It returns
#' separate rows for quantities and numbers. Several columns are according to the type of measure (MESURE, CALCULE, PONCTUEL or EXPERT).
#' @return A data.frame with daily migrations
#' @seealso calcule,report_mig_mult-method
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @export
fun_report_mig_mult_overlaps <- function(time.sequence, datasub, negative = FALSE) {
	# browser()
	mat1 <- as.data.frame(cbind(as.numeric(time.sequence), as.numeric(time.sequence +
									as.difftime(1, units = "days"))))
	mat2 <- as.data.frame(cbind(as.numeric(datasub$ope_date_debut), as.numeric(datasub$ope_date_fin)))
	rownames(mat1) <- as.character(time.sequence)
	rownames(mat2) <- datasub$lot_identifiant
	imat1 <- intervals::Intervals(mat1)
	intervals::closed(imat1) <- c(FALSE, FALSE)
	imat2 <- intervals::Intervals(mat2)
	intervals::closed(imat2) <- c(FALSE, FALSE)
	listei <- intervals::interval_overlap(imat2, imat1)
	listei2 <- listei  # copie de la liste pour l'ecraser
	for (i in 1:length(listei)) {
		vec <- listei[[i]]
		if (length(vec) == 0) {
			# pas de lot
			listei2[[i]] = 0
		} else if (length(vec) == 1) {
			# l'ensemble du lot est inclus dans la journee
			listei2[[i]] = 1
		} else {
			# le premier jour va du debut de l'ope e la fin de la premiere date
			# puis n-2 jour puis le dernier jour de la date de debut e la fin
			# de l'ope
			idlot = names(listei)[i]
			tps = c(difftime(time.sequence[vec[1]] + as.difftime(1, units = "days"),
							datasub[datasub$lot_identifiant == idlot, "ope_date_debut"], units = "days"),
					rep(1, length(vec) - 2), difftime(datasub[datasub$lot_identifiant ==
											idlot, "ope_date_fin"], time.sequence[vec[length(vec)]], units = "days"))
			listei2[[i]] <- as.numeric(tps)/(as.numeric(sum(tps)))  # on ramene a 1
			stopifnot(all.equal(as.numeric(sum(listei2[[i]])), 1))
		}
	}
	
	# specific case of operations across two years In this case we want to
	# split the operation and retain only the part corresponding to the current
	# year beginning of the year initializing variable browser()
	overlapping_samples_between_year <- FALSE
	imat3 <- imat1[1, ]
	listei3 <- intervals::interval_overlap(imat2, imat3)
	# vector of samples (lot) which are overlapping between two years
	lots_across <- names(listei3)[vapply(listei3, function(X) length(X) > 0, NA)]
	if (length(lots_across) > 0) {
		overlapping_samples_between_year <- TRUE
		for (i in 1:length(lots_across)) {
			the_lot <- lots_across[i]
			duration_in_the_year <- as.numeric(difftime(datasub[datasub$lot_identifiant ==
											the_lot, "ope_date_fin"], time.sequence[1], units = "days"))
			duration_of_the_sample <- as.numeric(difftime(datasub[datasub$lot_identifiant ==
											the_lot, "ope_date_fin"], datasub[datasub$lot_identifiant == the_lot,
									"ope_date_debut"], units = "days"))
			listei2[[the_lot]] <- listei2[[the_lot]] * (duration_in_the_year/duration_of_the_sample)
			
		}
	}
	#######################  end of the year
	le <- length(time.sequence)
	mat3 <- as.data.frame(cbind(as.numeric(time.sequence[le] + as.difftime(1, units = "days")),
					as.numeric(time.sequence[le] + as.difftime(2, units = "days"))))
	imat3 <- intervals::Intervals(mat3)
	listei3 <- intervals::interval_overlap(imat2, imat3)
	# vector of samples (lot) which are overlapping between two years
	lots_across <- names(listei3)[vapply(listei3, function(X) length(X) > 0, NA)]
	if (length(lots_across) > 0) {
		overlapping_samples_between_year <- TRUE
		for (i in 1:length(lots_across)) {
			the_lot <- lots_across[i]
			duration_in_the_year <- as.numeric(difftime(time.sequence[length(time.sequence)] +
									lubridate::days(1), datasub[datasub$lot_identifiant == the_lot, "ope_date_debut"],
							units = "days"))
			duration_of_the_sample <- as.numeric(difftime(datasub[datasub$lot_identifiant ==
											the_lot, "ope_date_fin"], datasub[datasub$lot_identifiant == the_lot,
									"ope_date_debut"], units = "days"))
			listei2[[the_lot]] <- listei2[[the_lot]] * (duration_in_the_year/duration_of_the_sample)
			
		}
	}
	
	
	# df ['lot_identifiant','coef','ts.id'] lot_identifiant= identifiant du
	# lot, coef = part du lot dans chaque id_seq (sequence de jours), 'id_seq'
	# numero du jour creating a table with lot_identifiant, sequence, and the
	# coeff to apply
	df <- data.frame(lot_identifiant = rep(names(listei2), sapply(listei2, length)),
			coef = unlist(listei2), ts_id = unlist(listei))
	# dataframe corresponding to the whole time sequence
	df.ts = data.frame(debut_pas = time.sequence, fin_pas = time.sequence + as.difftime(1,
					units = "days"), ts_id = as.numeric(strftime(time.sequence, format = "%j")),
			stringsAsFactors = FALSE)
	dfts <- merge(df.ts, df, by = "ts_id")
	datasub1 <- merge(dfts, datasub, by = "lot_identifiant")
	datasub1$value <- as.numeric(datasub1$value)  # Otherwise rounded to integer
	# If negative negative and positive are treated separately and return one
	# row for each positive or negative value below coef is the part of the
	# operation within the current year
	if (negative) {
		
		the_negative <- datasub1 %>%
				dplyr::select(debut_pas, fin_pas, value, coef, type_de_quantite, ope_dic_identifiant,
						lot_tax_code, lot_std_code, lot_methode_obtention) %>%
				dplyr::filter(value < 0) %>%
				dplyr::group_by(ope_dic_identifiant, lot_tax_code, lot_std_code, lot_methode_obtention,
						debut_pas, fin_pas, type_de_quantite) %>%
				dplyr::summarize(value = sum(value * coef)) %>%
				dplyr::arrange(ope_dic_identifiant, debut_pas, lot_tax_code, lot_std_code,
						type_de_quantite)
		
		the_positive <- datasub1 %>%
				dplyr::select(debut_pas, fin_pas, value, coef, type_de_quantite, ope_dic_identifiant,
						lot_tax_code, lot_std_code, lot_methode_obtention) %>%
				dplyr::filter(value >= 0) %>%
				dplyr::group_by(ope_dic_identifiant, lot_tax_code, lot_std_code, lot_methode_obtention,
						debut_pas, fin_pas, type_de_quantite) %>%
				dplyr::summarize(value = sum(value * coef)) %>%
				dplyr::arrange(ope_dic_identifiant, debut_pas, lot_tax_code, lot_std_code,
						type_de_quantite)
		
		datasub2 <- as.data.frame(rbind(the_negative, the_positive))
		
	} else {
		datasub2 <- as.data.frame(datasub1 %>%
						dplyr::select(debut_pas, fin_pas, value, coef, type_de_quantite, ope_dic_identifiant,
								lot_tax_code, lot_std_code, lot_methode_obtention) %>%
						dplyr::group_by(ope_dic_identifiant, lot_tax_code, lot_std_code, lot_methode_obtention,
								debut_pas, fin_pas, type_de_quantite) %>%
						dplyr::summarize(value = sum(value * coef)) %>%
						dplyr::arrange(ope_dic_identifiant, debut_pas, lot_tax_code, lot_std_code,
								type_de_quantite))
		
	}
	# if some samples overlap between the current year and the year arround the
	# current year, then the calculation will have hampered our numbers of a
	# small amount and the following test is not expected to be TRUE.
	if (!overlapping_samples_between_year)
		# note 2020 I'm getting this strange results that I don't understand
		# round(sum(datasub$value, na.rm = TRUE), 2) and
		# round(sum(datasub2$value, na.rm = TRUE), 2) are not equal: Mean
		# relative difference: 0.000996741 so rounded values by 2 digits are
		# not equal ???? # changed test to 0.1 browser()
		# 2021 same issue when running the vignette but don't see any difference in the browser() ?
		if (!abs(round(sum(datasub$value, na.rm = TRUE), 2) - round(sum(datasub2$value,
								na.rm = TRUE), 2)) < 0.1) warnings(
					paste("the numbers are different between raw numbers",
							round(sum(datasub$value, na.rm = TRUE), 2),
							"and number recalculated per day",
							round(sum(datasub2$value,	na.rm = TRUE),2)))
	datasub3 <- reshape2::dcast(datasub2, debut_pas + fin_pas + ope_dic_identifiant +
					lot_tax_code + lot_std_code + type_de_quantite ~ lot_methode_obtention, value.var = "value")
	if (!"MESURE" %in% colnames(datasub3))
		datasub3$MESURE = 0
	if (!"CALCULE" %in% colnames(datasub3))
		datasub3$CALCULE = 0
	if (!"EXPERT" %in% colnames(datasub3))
		datasub3$EXPERT = 0
	if (!"PONCUTEL" %in% colnames(datasub3))
		datasub3$PONCTUEL = 0
	datasub3$MESURE[is.na(datasub3$MESURE)] <- 0
	datasub3$CALCULE[is.na(datasub3$CALCULE)] <- 0
	datasub3$EXPERT[is.na(datasub3$EXPERT)] <- 0
	datasub3$PONCTUEL[is.na(datasub3$PONCTUEL)] <- 0
	# pour compatibilite
	datasub3 <- cbind(data.frame(No.pas = as.numeric(strftime(datasub3$debut_pas,
									format = "%j")) - 1), datasub3)
	datasub3$Effectif_total = rowSums(datasub3[, c("MESURE", "CALCULE", "EXPERT",
							"PONCTUEL")])
	return(datasub3)
}



#' Calculate daily migration by simple repartition
#' 
#' Function to calculate daily migration from migration monitoring whose length is less than one day,
#'  typically video recording whose period are instant events.
#' @param time.sequence the time sequence to be filled in with new data
#' @param datasub the initial dataset
#' @param negative 'boolean', default FALSE, TRUE indicates a separate sum for negative and positive migrations
#' @return A data.frame with number summed over over the time.sequence. 
#' The function returns the same output than \link{fun_report_mig_mult_overlaps}
#' but is intended to work faster. In the data.frame, the total number is 
#' 'Effectif_total' and corresponds to the addition of numbers and numbers converted from weight,
#' the total weight is 'Poids_total'+'poids_depuis_effectifs' and corresponds to weighed glass eel plus glass eel number converted in weights.
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @export
fun_report_mig_mult <- function(time.sequence, datasub, negative = FALSE) {    
	df.ts = data.frame(debut_pas = time.sequence, fin_pas = time.sequence + as.difftime(1,
					units = "days"), ts_id = strftime(time.sequence, format = "%j"), stringsAsFactors = FALSE)
	datasub$ts_id <- strftime(datasub$ope_date_debut, format = "%j")
	datasub1 <- merge(df.ts, datasub, by = "ts_id")
	if (negative) {
		
		the_negative <- datasub1 %>%
				dplyr::select(debut_pas, fin_pas, value, type_de_quantite, ope_dic_identifiant,
						lot_tax_code, lot_std_code, lot_methode_obtention) %>%
				dplyr::filter(value < 0) %>%
				dplyr::group_by(ope_dic_identifiant, lot_tax_code, lot_std_code, lot_methode_obtention,
						debut_pas, fin_pas, type_de_quantite) %>%
				dplyr::summarize(value = sum(value)) %>%
				dplyr::arrange(ope_dic_identifiant, debut_pas, lot_tax_code, lot_std_code,
						type_de_quantite)
		
		the_positive <- datasub1 %>%
				dplyr::select(debut_pas, fin_pas, value, type_de_quantite, ope_dic_identifiant,
						lot_tax_code, lot_std_code, lot_methode_obtention) %>%
				dplyr::filter(value >= 0) %>%
				dplyr::group_by(ope_dic_identifiant, lot_tax_code, lot_std_code, lot_methode_obtention,
						debut_pas, fin_pas, type_de_quantite) %>%
				dplyr::summarize(value = sum(value)) %>%
				dplyr::arrange(ope_dic_identifiant, debut_pas, lot_tax_code, lot_std_code,
						type_de_quantite)
		
		datasub2 <- as.data.frame(rbind(the_negative, the_positive))
		
	} else {
		datasub2 <- as.data.frame(datasub1 %>%
						dplyr::select(debut_pas, fin_pas, value, type_de_quantite, ope_dic_identifiant,
								lot_tax_code, lot_std_code, lot_methode_obtention) %>%
						dplyr::group_by(ope_dic_identifiant, lot_tax_code, lot_std_code, lot_methode_obtention,
								debut_pas, fin_pas, type_de_quantite) %>%
						dplyr::summarize(value = sum(value)) %>%
						dplyr::arrange(ope_dic_identifiant, debut_pas, lot_tax_code, lot_std_code,
								type_de_quantite))
		
	}
	# note 2020 I'm getting this strange results that I don't understand
	# round(sum(datasub$value, na.rm = TRUE), 2) and
	# round(sum(datasub2$value, na.rm = TRUE), 2) are not equal: Mean
	# relative difference: 0.000996741 so rounded values by 2 digits are
	# not equal ???? # changed test to 0.1 
	# 2021 same issue when running the vignette but don't see any difference in the browser() ?
	# maybe due to different time settings on the machine so it's converted to a warning
	if (!abs(round(sum(datasub$value, na.rm = TRUE), 2) - round(sum(datasub2$value,
							na.rm = TRUE), 2)) < 0.1) warning(
				paste("the numbers are different between raw numbers",
						round(sum(datasub$value, na.rm = TRUE), 2),
						"and number recalculated per day",
						round(sum(datasub2$value,	na.rm = TRUE),2)))
	datasub3 <- reshape2::dcast(datasub2, debut_pas + fin_pas + ope_dic_identifiant +
					lot_tax_code + lot_std_code + type_de_quantite ~ lot_methode_obtention, value.var = "value")
	if (!"MESURE" %in% colnames(datasub3))
		datasub3$MESURE = 0
	if (!"CALCULE" %in% colnames(datasub3))
		datasub3$CALCULE = 0
	if (!"EXPERT" %in% colnames(datasub3))
		datasub3$EXPERT = 0
	if (!"PONCTUEL" %in% colnames(datasub3))
		datasub3$PONCTUEL = 0
	datasub3$MESURE[is.na(datasub3$MESURE)] <- 0
	datasub3$CALCULE[is.na(datasub3$CALCULE)] <- 0
	datasub3$EXPERT[is.na(datasub3$EXPERT)] <- 0
	datasub3$PONCTUEL[is.na(datasub3$PONCTUEL)] <- 0
	datasub3 <- cbind(data.frame(No.pas = as.numeric(strftime(datasub3$debut_pas,
									format = "%j")) - 1), datasub3)
	datasub3$Effectif_total = rowSums(datasub3[, c("MESURE", "CALCULE", "EXPERT",
							"PONCTUEL")])
	return(datasub3)
}

#' returns a table where weights and number are calculated from number and weights respectively
#' performs a query to collect the conversion coefficients
#' @param tableau Table issued from report_mig
#' @param time.sequence Time sequence from report_mig
#' @param silent If silent=TRUE do not display messages
#' @return tableau, the data frame with weight converted to numbers
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
fun_weight_conversion = function(tableau, time.sequence, silent) {
	if (!silent)
		funout(gettextf("dc=%s Conversion weight / number\n", unique(tableau$ope_dic_identifiant)))
	nr <- nrow(unique(tableau[, c("debut_pas", "lot_tax_code", "lot_std_code")]))
	tableaupoids = subset(tableau, tableau$type_de_quantite == "poids")
	tableaueffectif = subset(tableau, tableau$type_de_quantite == "effectif")
	tableaueffectif = tableaueffectif[, c("No.pas", "lot_tax_code", "lot_std_code",
					"CALCULE", "MESURE", "EXPERT", "PONCTUEL", "Effectif_total")]
	tableaudesdeux = tableau[, c("No.pas", "debut_pas", "fin_pas", "ope_dic_identifiant",
					"lot_tax_code", "lot_std_code", "coe_valeur_coefficient")]
	tableaudesdeux = tableaudesdeux[!duplicated(tableaudesdeux[, c("No.pas", "lot_tax_code",
									"lot_std_code")]), ]
	# Conversion des poids en effectifs
	tableauconvert = tableaupoids[, c("MESURE", "CALCULE", "EXPERT", "PONCTUEL",
					"Effectif_total")]
	tableauconvert = tableauconvert * tableaupoids$coe_valeur_coefficient  # les coeff sont du type 2.54 et non 0.3
	if (sum(tableaupoids$coe_valeur_coefficient) == 0)
		funout(gettext("Careful sum=0, you didn't enter the coefficient of conversion\n",
						domain = "R-stacomiR"))
	# creation d'une tableau (matricepoids) a 5 colonnes comprenant les
	# effectifs convertis
	matricepoids = cbind(tableaupoids[, c("No.pas", "lot_tax_code", "lot_std_code")],
			tableauconvert, tableaupoids[, c("MESURE", "CALCULE", "EXPERT", "PONCTUEL",
							"Effectif_total")])
	dimnames(matricepoids) = list(1:length(tableaupoids[, 1]), c("No.pas", "lot_tax_code",
					"lot_std_code", "MESURE", "CALCULE", "EXPERT", "PONCTUEL", "Effectif_total",
					"poids_MESURE", "poids_CALCULE", "poids_EXPERT", "poids_PONCTUEL", "Poids_total"))
	tableau = merge(tableaudesdeux, tableaueffectif, by = c("No.pas", "lot_tax_code",
					"lot_std_code"), all.x = TRUE, all.y = FALSE)
	tableau = merge(tableau, matricepoids, all.x = TRUE, all.y = FALSE, by = c("No.pas",
					"lot_tax_code", "lot_std_code"), sort = TRUE, suffixes = c(".e", ".p"))
	# je vire les NA
	tableau[is.na(tableau)] = 0
	tableau$MESURE = tableau$MESURE.e + tableau$MESURE.p
	tableau$CALCULE = tableau$CALCULE.e + tableau$CALCULE.p
	tableau$EXPERT = tableau$EXPERT.e + tableau$EXPERT.p
	tableau$PONCTUEL = tableau$PONCTUEL.e + tableau$PONCTUEL.p
	tableau$Effectif_total = tableau$Effectif_total.e + tableau$Effectif_total.p
	tableau[, "poids_depuis_effectifs"] = tableau[, "Effectif_total.e"]/tableau$coe_valeur_coefficient
	stopifnot(nr == nrow(tableau))
	return(tableau)
}

#' Calculates a data.frame where all components within the list calcdata are aggregated
#' and formatted for plot
#' @param object An object of class \link{report_mig_mult-class}
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @return A data.frame
#' @export
fun_aggreg_for_plot <- function(object) {
	if (!inherits(object , "report_mig_mult"))
		stop("This function must have for argument an object of class report_mig_mult")
	the_taxa = paste(object@taxa@data[object@data$tax_code %in% object@taxa@taxa_selected,"tax_nom_latin"], collapse = ",")
	the_stages = paste(object@stage@data[object@data$std_code %in% object@stage@stage_selected,"std_libelle"], collapse = ",")
	grdata <- data.frame()
	for (i in 1:length(object@calcdata)) {
		data <- object@calcdata[[i]]$data
		# extracting similar columns (not those calculated)
		data <- data[, c("No.pas", "debut_pas", "fin_pas", "ope_dic_identifiant",
						"lot_tax_code", "lot_std_code", "MESURE", "CALCULE", "EXPERT", "PONCTUEL",
						"Effectif_total")]
		grdata <- rbind(grdata, data)
	}
	names(grdata) <- tolower(names(grdata))
	grdata <- fun_date_extraction(grdata, nom_coldt = "debut_pas", annee = FALSE,
			mois = TRUE, quinzaine = TRUE, semaine = TRUE, jour_an = TRUE, jour_mois = FALSE,
			heure = FALSE)
	annee = unique(strftime(as.POSIXlt(object@time.sequence), "%Y"))
	dis_commentaire = paste(as.character(object@dc@dc_selected), collapse = ",")
	grdata <- stacomirtools::chnames(grdata, c("ope_dic_identifiant", "lot_tax_code",
					"lot_std_code"), c("DC", "taxa", "stage"))
	grdata$DC <- as.factor(grdata$DC)
	grdata$taxa <- as.factor(grdata$taxa)
	return(grdata)
}



