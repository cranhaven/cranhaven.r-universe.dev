#' Trend of wet weight in glass eel
#' 
#' In trapping ladders, glass eel are seldom counted, as they are too tiny to handle and too numerous to count.
#' The usual operation is to weight them, or to use a bucket to measure their volume. These weights or volumes will later
#' need to be converted to numbers. The glass eel weight may follow a seasonal pattern. It's the case for Anguilla anguilla 
#' glass eel in the Bay of Biscay. Weights can be modelled using sine wave curves, or more complex gam models. 
#' This class has a model method to try those models, which can also be used to extact coefficients manually
#' to manually test more complex models.
#' Some plots are provided to display the coefficients stored in the database, and the model results. A parameter provided in 
#' the graphical interface or in the command line (slot liste) takes values '1', '>1', 'tous' which mean respectively use
#' only individual sample of glass eels, or use 'group weights' which can be more close to the real weight of glass eel
#' during counts as glass eel are not completely drained from their water during handling to preserve their mucus. The list choice
#' 'tous' means that both individual and group weights are selected.
#' @include ref_coe.R
#' @note In this class some tools are available to import glass eel measurement from
#' experimental fishing in the estuary. For the charge method dates for the
#' request are from august to august (a glass eel season)
#' @slot data A \code{'data.frame'} data for report lot
#' @slot calcdata  A list containing two processed data frames, data and coe
#' @slot dc Object of class \code{\link{ref_dc-class}}, the counting device
#' @slot start_year Object of class \code{\link{ref_year-class}}. ref_year allows to choose the year of beginning
#' @slot end_year Object of class \code{\link{ref_year-class}}
#' ref_year allows to choose last year of the report
#' @slot coe Object of class \code{\link{ref_coe-class}} class loading coefficient
#' of conversion between quantity (weights or volumes of glass eel) and numbers
#' @slot liste Object of class \code{\link{ref_list-class}} ref_list referential
#' class choose within a list, here the choice is whether subsamples or not. Subsamples
#' in the stacomi database are samples with a non null value for parent sample. Migration
#' counts are never made on subsamples but those can be integrated to calculate mean weights.
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @family report Objects
#' @keywords classes
#' @example inst/examples/report_ge_weight-example.R
#' @aliases report_ge_weight
#' @export 
setClass(Class = "report_ge_weight", representation = representation(data = "data.frame",
				calcdata = "list", dc = "ref_dc", start_year = "ref_year", end_year = "ref_year",
				coe = "ref_coe", liste = "ref_list"), prototype = prototype(data = data.frame(),
				calcdata = list(), dc = new("ref_dc"), start_year = new("ref_year"), end_year = new("ref_year"),
				coe = new("ref_coe"), liste = new("ref_list")))

#' connect method for report_Poids_moyen
#' 
#' The connect method adapts queries according to user choices, mean weight
#'  w is calculated as car_valeur_quantitatif/lot_effectif. These coefficients are stored in the database,
#' and the connect method loads them from the table using the \link{ref_coe-class}
#' @param object An object of class \link{report_ge_weight-class}
#' @param silent Should the method be silent
#' @return An object of class \link{report_ge_weight-class}  with slots data and coe filled from the database
#' @note dates for the request are from august to august (a glass eel season)
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases connect.report_ge_weight
setMethod("connect", signature = signature("report_ge_weight"), definition = function(object, silent=TRUE) {
			# object<-r_gew loading mean weights
			requete = new("RequeteDBwheredate")
			requete@datedebut = strptime(paste(object@start_year@year_selected, "-08-01",
							sep = ""), format = "%Y-%m-%d")
			requete@datefin = strptime(paste(object@end_year@year_selected, "-08-01",
							sep = ""), format = "%Y-%m-%d")
			requete@colonnedebut = "ope_date_debut"
			requete@colonnefin = "ope_date_fin"
			requete@select = paste("SELECT lot_identifiant,ope_date_debut,ope_date_fin,lot_effectif,car_valeur_quantitatif as poids,",
					" (car_valeur_quantitatif/lot_effectif) AS w,", " (ope_date_fin-ope_date_debut)/2 AS duree,",
					" ope_date_debut+(ope_date_fin-ope_date_debut)/2 as datemoy,", " date_part('year', ope_date_debut) as annee,",
					" date_part('month',ope_date_debut) as mois", " FROM ", get_schema(), "vue_lot_ope_car_qan", sep = "")
			requete@and = paste(" AND ope_dic_identifiant=", object@dc@dc_selected, " AND std_libelle='civelle'",
					ifelse(object@liste@selectedvalue == "tous", "", paste(" AND  lot_effectif",
									object@liste@selectedvalue)), " AND upper(car_methode_obtention::text) = 'MESURE'::text",
					" AND car_par_code='A111'", sep = "")
			requete <- stacomirtools::query(requete)
			object@data <- requete@query
			# loading conversion coefficients
			object@coe@datedebut = requete@datedebut
			object@coe@datefin = requete@datefin
			object@coe <- charge(object@coe)
			if (!silent){
				funout(gettext("The query to load the coefficients of conversion is finished\n",
								domain = "R-stacomiR"))
				funout(gettextf("%1.0f lines found for the conversion coefficients\n", nrow(object@coe),
								domain = "R-stacomiR"))
			}
			assign(x = "report_ge_weight", value = object, envir = envir_stacomi)
			return(object)
		})


#' command line interface for \link{report_ge_weight-class}
#' @param object An object of class \link{report_ge_weight-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,ref_dc-method}
#' @param start_year The starting the first year, passed as character or integer
#' @param end_year the finishing year, must be > start_year (minimum one year in august to the next in august)
#' @param selectedvalue A character to select and object in the \link{ref_list-class}
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class \link{report_ge_weight-class}  with data selected
#' The choice_c method fills in the data slot for classes \link{ref_dc-class} \link{ref_year-class}
#' \link{ref_coe-class} \link{ref_list-class}
#' @aliases choice_c.report_ge_weight
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
setMethod("choice_c", signature = signature("report_ge_weight"), definition = function(object,
				dc, start_year, end_year, selectedvalue, silent = FALSE) {
			# code for debug using example
			# dc=c(5,6);start_year='2015';end_year='2016';selectedvalue='>1';silent=FALSE
			if (length(selectedvalue) != 1)
				stop("selectedvalue must be of length one")
			r_gew <- object
			stopifnot(end_year > start_year)
			r_gew@dc = charge(r_gew@dc)
			# loads and verifies the dc this will set dc_selected slot
			r_gew@dc <- choice_c(object = r_gew@dc, dc)
			# only taxa present in the report_mig are use
			r_gew@start_year <- charge(object = r_gew@start_year, objectreport = "report_ge_weight")
			r_gew@start_year <- choice_c(object = r_gew@start_year, nomassign = "start_year",
					annee = start_year, silent = silent)
			r_gew@end_year@data <- r_gew@start_year@data
			r_gew@end_year <- choice_c(object = r_gew@end_year, nomassign = "end_year", annee = end_year,
					silent = silent)
			r_gew@liste = charge(object = r_gew@liste, listechoice = c("=1", ">1", "tous"),
					label = gettext("choice of number in sample (one, several,all)", domain = "R-stacomiR"))  # choix de la categorie d'effectif)
			r_gew@liste <- choice_c(r_gew@liste, selectedvalue = selectedvalue)
			assign("report_ge_weight", r_gew, envir = envir_stacomi)
			return(r_gew)
		})




#' Calcule method for report_ge_weight
#' @param object An object of class \link{report_ge_weight-class}
#' @param silent Boolean, if TRUE, information messages are not displayed, only warnings and errors
#' @return An object of class  \link{report_ge_weight-class} with  \code{@calcdata[["data"]]} (essentially a selection of 
#' columns and renaming from \code{@data}) and \code{coe} daily coefficients extracted from the database 
#' \code{@calcdata[["coe"]]} and prepared for graphs
#' @aliases calcule.report_ge_weight
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
setMethod("calcule", signature = signature("report_ge_weight"), definition = function(object,
				silent = FALSE) {
			r_gew <- object
			donnees <- r_gew@data
			coeff <- r_gew@coe@data
			coeff$w <- 1/coeff$coe_valeur_coefficient
			coeff$date <- as.POSIXct(coeff$coe_date_debut)
			if (!silent)
				funout(gettext("To obtain the table, type : report_ge_weight=get('report_ge_weight',envir_stacomi)@data\n",
								domain = "R-stacomiR"))
			# changement des noms
			donnees <- stacomirtools::chnames(donnees, c("lot_identifiant", "ope_date_debut",
							"ope_date_fin", "lot_effectif", "poids", "w", "duree", "datemoy"), c("lot",
							"date", "date_fin", "effectif", "poids", "w", "time.sequence", "date"))
			# correction de manques d'effectifs dans la base
			if (sum(is.na(donnees$effectif)) > 0)
				warnings(gettextf("size is missing, lots %s", paste(unique(donnees$lot[is.na(donnees$effectif)]),
										collapse = " "), domain = "R-stacomiR"))
			r_gew@calcdata[["data"]] <- donnees[, c(8, 6, 4, 1)]
			r_gew@calcdata[["coe"]] <- coeff[order(coeff$date), c(10, 9)]
			assign("report_ge_weight", r_gew, envir = envir_stacomi)
			return(r_gew)
		})


#' Plot method for report_ge_weight' 
#' @note the model method provides plots for the fitted models
#' @param x An object of class \link{report_ge_weight-class}
#' @param plot.type Default '1'. '1' plot of mean weight of glass eel against the mean date of operation (halfway between start,
#' and end of operation). The ggplot 'p' can be accessed from envir_stacomi using \code{get('p',envir_stacomi)}. '2' standard plot of current coefficent.
#' '3' same as '1' but with size according to number.
#' @param silent Stops displaying the messages
#' @return Nothing, called for its side effect of plotting data
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases plot.report_ge_weight
#' @export
setMethod("plot", signature(x = "report_ge_weight", y = "missing"), definition = function(x,
				plot.type = 1, silent = FALSE) {
			# plot.type='1';silent=FALSE r_gew=get('report_ge_weight',envir_stacomi)
			r_gew <- x
			don <- r_gew@calcdata$data
			coe <- r_gew@calcdata$coe
			####################'
			# ggplot
			if (plot.type == 1) {
				p <- ggplot2::qplot(x = date, y = w, data = don)
				print(p)
				assign("p", p, envir = envir_stacomi)
				if (!silent)
					funout(gettext("ggplot object p assigned to envir_stacomi", domain = "R-stacomiR"))
				####################'
				# standard plot
			} else if (plot.type == 2) {
				if (length(r_gew@liste@selectedvalue) == 0)
					stop("Internal error, the value has not been selected before launching plot")
				type_poids = switch(r_gew@liste@selectedvalue, `>1` = gettext("wet weights",
								domain = "R-stacomiR"), `=1` = gettext("dry weights", domain = "R-stacomiR"),
						tous = gettext("wet and dry weights", domain = "R-stacomiR"))
				plot(x = don$date, y = don$w, xlab = gettext("date", domain = "R-stacomiR"),
						ylab = gettext("mean weights", domain = "R-stacomiR"), col = "red", main = gettextf("Seasonal trend of %s, from %s to %s",
								type_poids, r_gew@start_year@year_selected, r_gew@end_year@year_selected,
								domain = "R-stacomiR"), sub = "Trend of wet weights")
				coe <- coe[order(coe$date), ]
				points(coe$date, coe$w, type = "l", col = "black", lty = 2)
				# legend('topright',c('Obs.', 'Coeff base'),
				# col=c('black','cyan'),pch='o',cex = 0.8)
				
				####################'
				# geom_point + size
			} else if (plot.type == 3) {
				p <- ggplot2::qplot(x = date, y = w, data = don)
				print(p + aes(size = effectif))
				assign("p", p, envir = envir_stacomi)
				if (!silent)
					funout(gettext("object p assigned to envir_stacomi", domain = "R-stacomiR"))
			}
			return(invisible(NULL))
		})


#' model method for report_ge_weight' 
#' this method uses samples collected over the season to model the variation in weight of
#' glass eel or yellow eels.
#' @param object An object of class \link{report_ge_weight-class}
#' @param model.type default 'seasonal', 'seasonal1','seasonal2','manual'. 
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return An object of class \link{report_ge_weight-class} with \code{@calcdata[["import_coe"]]} filled.
#' @details 
#' Depending on model.type several models are produced
#' \describe{
#'\item{model.type='seasonal'.}{The simplest model uses a seasonal variation, it is
#'     fitted with a sine wave curve allowing a cyclic variation 
#'     w ~ a*cos(2*pi*(d'-T)/365)+b with a period T. The modified day d' used is this model is set
#'     at 1 the 1st of august doy = d' + d0; d0 = 212, doy=julian days}
#'\item{model.type='seasonal1'.}{A time component is introduced in the model, which allows
#'   for a long term variation along with the seasonal variation. This long term variation is
#'   is fitted with a gam, the time variable is set at zero at the beginning of the first day of observed values.
#'   The seasonal variation is modeled on the same modified julian time as model.type='seasonal'
#'   but here we use a cyclic cubic spline cc, which allows to return at the value of d0=0 at d=365.
#'   This model was considered as the best to model size variations by Diaz & Briand in prep. but using a large set of values
#'   over years.}
#'\item{model.type='seasonal2'.}{The seasonal trend in the previous model is now modelled with a sine
#'   curve similar to the sine curve used in seasonal.  The formula for this is \eqn{sin(\omega vt) + cos(\omega vt)}{sin(omega vt) + cos(omega vt)}, 
#'  where vt is the time index variable \eqn{\omega}{omega} is a constant that describes how the index variable relates to the full period
#'   (here, \eqn{2\pi/365=0.0172}{2pi/365=0.0172}). The model is written as following \eqn{w~cos(0.0172*doy)+sin(0.0172*doy)+s(time).}}
#'\item{model.type='manual'.}{The dataset don (the raw data), coe (the coefficients already present in the
#'   database, and newcoe the dataset to make the predictions from, are written to the environment envir_stacomi. 
#'   please see example for further description on how to fit your own model, build the table of coefficients,
#'   and write it to the database.}
#' }
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases model.report_ge_weight
setMethod("model", signature(object = "report_ge_weight"), definition = function(object,
				model.type = "seasonal", silent = FALSE) {
			# r_gew=get('report_ge_weight',envir_stacomi);silent=TRUE;require(ggplot2)
			# r_gew <- bilPM
			r_gew <- object
			don <- r_gew@calcdata$data
			coe <- r_gew@calcdata$coe
			seq = seq(as.Date(r_gew@coe@datedebut), as.Date(r_gew@coe@datefin), by = "day")
			origine <- as.POSIXct(trunc(min(don$date), "day"))
			# season starting in november
			fndate <- function(data) {
				if (!"date" %in% colnames(data))
					stop("date should be in colnames(data)")
				if (!inherits(data$date[1], "POSIXct"))
					stop("date should be POSIXct")
				data$year <- lubridate::year(data$date)
				# lubridate::yday(lubridate::dmy(01082008))
				data$yday = lubridate::yday(data$date)
				data$doy = data$yday - 212  # year begins in august to be consistent with the class   
				data$season <- stringr::str_c(lubridate::year(data$date) - 1, "-", lubridate::year(data$date))  # year-1-year
				data$season[data$doy > 0] <- stringr::str_c(lubridate::year(data$date), "-",
						lubridate::year(data$date) + 1)[data$doy > 0]  # for november and december it's year - year+1
				data$yearbis <- data$year  # same as season but with a numeric
				data$yearbis[data$doy > 0] <- data$yearbis[data$doy > 0] + 1  # same as season but a numeric
				data$doy[data$doy < 0] <- data$doy[data$doy < 0] + 365
				data$time = as.numeric(data$date - origine)
				return(data)
			}
			don$date <- as.POSIXct(as.Date(don$date)) # bug the tz in CEST and GMT don't fit well
			# and the range of time between don and newcoe becomes extremely different
			don <- fndate(don)
			newcoe = data.frame(date = seq, mean_weight = NA, number = NA, lot = NA, yday = lubridate::yday(seq))
			newcoe$date = as.POSIXct(newcoe$date)
			newcoe = fndate(newcoe)
			
			if (model.type == "seasonal") {
				result <- data.frame(season = unique(don$season), year = unique(don$yearbis),
						a = NA, T = NA, b = NA)
				for (seas in unique(don$season)) {
					# seas<-unique(don$season)[1]
					if (!silent){
						print(seas)
						print("___________")
					}
					# regression one per season, taking T as adjusted previously
					year = result[result$season == seas, "year"]
					g0 <- nls(formula = w ~ a * cos(2 * pi * (doy - T)/365) + b, data = don[don$season ==
											seas, ], start = list(a = 0.08, T = 73.7, b = 0.29))
					# getting the results into a table result
					result[result$season == seas, c("a", "T", "b")] <- coef(g0)
					if (!silent){
						print(summary(g0))
						print("AIC:")
						print(AIC(g0))
					}
					# what is the size in december ? I'm just using the formula from
					# Guerault and Desaunay
					# result[result$season==seas,'pred_weight']<-coef(g0)['a']*cos(2*pi*(50-T)/365)+coef(g0)['b']
					# dataframe for prediction, I will bind them to get a final
					# dataframe (predatafull) for the graph below
					predatay <- newcoe[newcoe$season == seas, ]
					predatay$pred_weight <- predict(g0, newdata = predatay)
					if (seas == unique(don$season)[1]) {
						predata <- predatay
					} else predata <- rbind(predata, predatay)
				}
				if (!silent) print(result)
				assign("result", result, envir_stacomi)
				if (!silent)
					funout(gettext("Model equations assigned to envir_stacomi (result)",
									domain = "R-stacomiR"))
				
				p <- ggplot(don) + geom_jitter(aes(x = doy, y = w), col = "aquamarine4") +
						facet_wrap(~season) + geom_line(aes(x = doy, y = pred_weight), data = predata) +
						# geom_line(aes(x=doy,y=pred_weight),color='green',size=1,data=predatafull[predatafull$doy==50,])+
						theme_minimal() + theme(panel.border = element_blank(), axis.line = element_line()) +
						xlab(gettext("Day in the season, starting 1st of august", domain = "R-StacomiR"))
				
				print(p)
				assign("p", p, envir = envir_stacomi)
				if (!silent)
					funout(gettext("ggplot object p assigned to envir_stacomi", domain = "R-stacomiR"))
				
				
				# fm <- stats::nls(formula=w ~ a*cos(2*pi*(doy-T)/365)+b
				# ,data=don,start=list(a=0.1,T=73,b=0.3)) pred<-stats::predict(fm,
				# newdata=newcoe) com=gettextf('sinusoidal model,
				# a.cos(2.pi.(jour-T)/365)+b a=%s t=%s
				# b=%s',round(coef(fm),2)[1],round(coef(fm),2)[2],round(coef(fm),2)[3])
				# plot(r_gew,plot.type=2) points(as.POSIXct(newcoe$date),pred,
				# col='magenta') legend('topright',c('Obs.', 'Coeff base','Mod'),
				# col=c('black','cyan','magenta'),pch='o',cex = 0.8)
				# mtext(com,side=3,line=0.5)
				
				result_to_text <- stringr::str_c(sapply(t(result[, c(1, 3, 4, 5)]), as.character),
						collapse = " ")
				
				# setting text for comment (lines inserted into the database)
				com = stringr::str_c("w ~ a*cos(2*pi*(doy-T)/365)+b with a period T.", " The julian time d0 used is this model is set at zero 1st of November doy = d + d0; d0 = 305.",
						" Coefficients for the model (one line per season): season, a, T, b =",
						result_to_text)
				
			} else if (model.type == "seasonal1") {
				
				g1 = mgcv::gam(w ~ s(yday, bs = "cc") + s(time), data = don, knots = list(yday = c(1,
										365)))
				# the knots=list(yday=c(1,365) is necessary for a smooth construction
				# of the model
				summary(g1)
				plot(g1, pages = 1)
				predata <- newcoe
				pred <- predict(g1, newdata = predata, se.fit = TRUE, type="response")
				predata$pred_weight <- pred$fit
				predata$pred_weight_lwr <- pred$fit - 1.96 * pred$se.fit
				predata$pred_weight_upr <- pred$fit + 1.96 * pred$se.fit
				p <- ggplot(don) + geom_jitter(aes(x = date, y = w), col = "aquamarine4") +
						geom_line(aes(x = date, y = pred_weight), data = predata) + geom_ribbon(data = predata,
								aes(x = date, ymin = pred_weight_lwr, ymax = pred_weight_upr), alpha = 0.3,
								fill = "saddlebrown") + scale_x_datetime(date_breaks = "years", date_minor_breaks = "month") +
						theme_minimal() + theme(panel.border = element_blank(), axis.line = element_line()) +
						xlab("Date")
				if (!silent) print(p)
				assign("p", p, envir = envir_stacomi)
				assign("g1", g1, envir = envir_stacomi)
				if (!silent)
					funout(gettext("ggplot object p assigned to envir_stacomi", domain = "R-stacomiR"))
				if (!silent)
					funout(gettext("gam model g1 assigned to envir_stacomi", domain = "R-stacomiR"))
				com = "model seasonal1 = gam(w~s(yday,bs='cc')+s(time), knots = list(yday = c(1, 365)))"
				
			} else if (model.type == "seasonal2") {
				
				## seasonal effects with a continuous sine-cosine wave,.  The
				## formula for this is 'sin(omegavt) + cos(omegavt)',
				## where vt is the time index variable
				## \tomega is a constant that describes how the index
				## variable relates to the full period (here,
				## 2pi/365=0.0172).
				g2 = mgcv::gam(w ~ cos(0.0172 * doy) + sin(0.0172 * doy) + s(time), data = don)
				print(gettext("One model per year, doy starts in august", domain = "R-stacomiR"))
				summary(g2)
				plot(g2, pages = 1)
				predata <- newcoe
				pred <-  predict(g2, newdata = predata, se.fit = TRUE, type="response")
				predata$pred_weight <- pred$fit
				predata$pred_weight_lwr <- pred$fit - 1.96 * pred$se.fit
				predata$pred_weight_upr <- pred$fit + 1.96 * pred$se.fit
				p <- ggplot(don) + geom_jitter(aes(x = date, y = w), col = "aquamarine4") +
						geom_line(aes(x = date, y = pred_weight), data = predata) + geom_ribbon(data = predata,
								aes(x = date, ymin = pred_weight_lwr, ymax = pred_weight_upr), alpha = 0.8,
								fill = "wheat") + 
						scale_x_datetime(date_breaks = "years", date_minor_breaks = "month") +
						theme_minimal() + 
						theme(panel.border = element_blank(), axis.line = element_line()) +
						xlab("Date")
				if (!silent) print(p)
				assign("p", p, envir = envir_stacomi)
				assign("g2", g2, envir = envir_stacomi)
				if (!silent)
					funout(gettext("ggplot object p assigned to envir_stacomi", domain = "R-stacomiR"))
				if (!silent)
					funout(gettext("gam model g2 assigned to envir_stacomi", domain = "R-stacomiR"))
				
				## comparison
				## with Guerault and Desaunay (summary table in latex)
				gamma = as.numeric(sqrt(g2$coefficients["cos(0.0172 * doy)"]^2 + g2$coefficients["sin(0.0172 * doy)"]^2))  #0.386
				# compared with 0.111
				phi = round(as.numeric(atan2(g2$coefficients["sin(0.0172 * doy)"], g2$coefficients["cos(0.0172 * doy)"]) -
										pi/2))  # -0.82
				# time is centered on zero
				s0 = as.numeric(g2$coefficients["(Intercept)"])  #7.04 (compared with 6.981)
				summary_harmonic <- data.frame(source = c("Vilaine 1991-1993, Guerault et Desaunay",
								"This model"), `$\\gamma$` = c(0.0375, gamma), `$s_0$` = c(0.263, s0),
						`$\\phi$` = c(319, 305 - phi))
				# need to repass colnames
				colnames(summary_harmonic) = c("source", "$\\gamma$", "$s_0(cm)$", "$\\phi$")
				xt_summary_harmonic <- xtable(summary_harmonic, caption = gettext("Comparison of the coefficients obtained by \\citet{desaunay_seasonal_1997} and in the present modelling of estuarine samples.",
								domain = "R-stacomiR"), label = gettext("summary_harmonic", domain = "R-stacomiR"),
						digits = c(0, 0, 3, 3, 0))
				tabname <- stringr::str_c(get("datawd", envir = envir_stacomi), "/summary_harmonic.tex")
				o <- print(xt_summary_harmonic, file = tabname, table.placement = "htbp",
						caption.placement = "top", NA.string = "", include.rownames = FALSE,
						tabular.environment = "tabularx", width = "0.6\\textwidth", sanitize.colnames.function = function(x) {
							x
						})
				
				funout(gettextf("summary coefficients written in %s", tabname, domain = "R-stacomiR"))
				com = stringr::str_c("model seasonal2 = gam(w~cos(0.0172*doy)+sin(0.0172*doy)+s(time), knots = list(yday = c(1, 365))),Desaunay's gamma=",
						round(gamma, 3), ", phi=", phi, ", s0=", round(s0, 3))
				
				
			} else if (model.type == "manual") {
				if (!silent)
					funout(gettext("Table for predictions newcoe assigned to envir_stacomi",
									domain = "R-stacomiR"))
				assign("newcoe", newcoe, envir = envir_stacomi)
				if (!silent)
					funout(gettext("Table of data don assigned to envir_stacomi", domain = "R-stacomiR"))
				assign("don", don, envir = envir_stacomi)
				if (!silent)
					funout(gettext("Table of current coefficients coe assigned to envir_stacomi",
									domain = "R-stacomiR"))
				assign("coe", coe, envir = envir_stacomi)
			}
			
			if (model.type != "manual") {
				import_coe = data.frame(coe_tax_code = "2038", coe_std_code = "CIV", coe_qte_code = 1,
						coe_date_debut = Hmisc::roundPOSIXt(predata$date, digits = "days"), coe_date_fin = Hmisc::roundPOSIXt(predata$date,
								digits = "days") + as.difftime(1, units = "days"), coe_valeur_coefficient = 1/predata$pred_weight,
						coe_commentaires = com)
				# will write only if the database is present
				if (get("database_expected", envir_stacomi)) {
					fileout = paste(get("datawd", envir = envir_stacomi), "import_coe", r_gew@start_year@year_selected,
							r_gew@end_year@year_selected, ".csv", sep = "")
					utils::write.table(import_coe, file = fileout, row.names = FALSE, sep = ";")
					if (! silent){ 
						funout(paste(gettextf("data directory :%s", fileout, domain = "R-stacomiR")))
					}
				}
				assign("import_coe", import_coe, envir = envir_stacomi)
				if (! silent){
					funout(gettext("To obtain the table, type : import_coe=get(\"import_coe\",envir_stacomi)",
									domain = "R-stacomiR"))
				}
				r_gew@calcdata[["import_coe"]] <- import_coe
			}
			return(r_gew)
		})




#' Method to write data to the stacomi database for \link{report_ge_weight-class}
#' 
#' Data will be written in tj_coefficientconversion_coe table, if the class retrieves some data
#' from the database, those will be deleted first. 
#' @param object An object of class \link{report_ge_weight-class}
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return Nothing, called for its side effect of writing to the database
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @aliases write_database.report_ge_weight
setMethod("write_database", signature = signature("report_ge_weight"), definition = function(object,
				silent = FALSE) {
			
			r_gew <- object
			if (!"import_coe" %in% names(r_gew@calcdata))
				funout(gettext("Attention, you must fit a model before trying to write the predictions in the database",
								domain = "R-stacomiR"), arret = TRUE)
			# first delete existing data from the database
			supprime(r_gew@coe, tax = 2038, std = "CIV", silent = silent)
			import_coe <- r_gew@calcdata$import_coe
			import_coe$coe_org_code <- get_org()
			import_coe$coe_date_debut <- as.Date(import_coe$coe_date_debut)+1  # to avoid day change with POSIXct and database
			import_coe$coe_date_fin <- as.Date(import_coe$coe_date_fin)+1  
			con <- new("ConnectionDB")
			con <- connect(con)
			on.exit(pool::poolClose(con@connection))
			sql <- "DROP TABLE IF EXISTS import_coe"
			pool::dbExecute(con@connection, statement = sql)
			pool::dbWriteTable(con@connection, 
					name = "import_coe", 
					value=import_coe, 
					temporary=TRUE)	
			sql <- stringr::str_c("INSERT INTO ", get_schema(), "tj_coefficientconversion_coe (",
					"coe_tax_code,coe_std_code,coe_qte_code,coe_date_debut,coe_date_fin,coe_valeur_coefficient,
							coe_commentaires,coe_org_code)",
					" SELECT coe_tax_code,coe_std_code,coe_qte_code,coe_date_debut,coe_date_fin,coe_valeur_coefficient::real,
							coe_commentaires,coe_org_code FROM import_coe;")
			pool::dbExecute(con@connection, statement = sql)
			if (!silent){
				funout(gettext(sprintf("You have written %s rows in the database",nrow(import_coe)),
								domain = "R-stacomiR"))
			}
			return(invisible(NULL))
		})



