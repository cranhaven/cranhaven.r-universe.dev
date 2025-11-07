

#' function used for some lattice graphs with dates 
#' @param vectordate date or POSIXt 
#' @return vectordate (without class)
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
graphdate<-function(vectordate){
	vectordate <- as.POSIXct(vectordate)
	attributes(vectordate) <- NULL
	unclass(vectordate)
	return(vectordate)
}







#' function used to remove special non utf8 character which cause the gtk
#' interface to crash
#' 
#' 
#' @param text a text string which might contain no utf8 characters
#' @return text
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
fun_char_spe<-function(text){
	text <- gsub("\u00e9","e",text) 
	text <- gsub("\u00e8","e",text) 
	text <- gsub("\u00ea","e",text) 
	text <- gsub("\u00e0","a",text) 
	return(text)}








#' Transforms a vector into a string called within an sql command e.g.
#' c('A','B','C') => in ('A','B','C')
#' 
#' Transforms a vector into a string called within an sql command e.g. c(A,B,C)
#' => in ('A','B','C')
#' 
#' 
#' @param vect a character vector
#' @return A list of value
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @export
vector_to_listsql<-function(vect)
{
	if (is.null(vect)) stop("The vector passed to vector_to_listsql should not be null")
	if (any(is.na(vect))) stop("The vector passed to vector_to_listsql should not be NA")
	if (length(vect)==0) stop("The vector passed to vector_to_listsql should not be of lenght zero")
	if (length(vect)==1) 
	{
		listsql=paste("(","'",vect,"'",")",sep="")
	}
	
	if (length(vect)>2)
	{
		listsql=paste("(","'",vect[1],"'",",", sep="")
		for(j in 2:(length(vect)-1)){
			listsql=paste(listsql,"'",vect[j],"'",",",sep="")
		}
		listsql=paste(listsql,"'",vect[length(vect)],"'",")", sep="")
	} 
	else if  (length(vect)==2)
	{
		listsql=paste("(","'",vect[1],"'",",", sep="")
		listsql=paste(listsql,"'",vect[length(vect)],"'",")", sep="") 
	}
	
	return(listsql)
} 



#' Create a dataframe suitable for charts per 24h and day
#' 
#' This functions takes a data frame with a column with starting time and another with ending time
#' If the period extends over midnight, it will be split into new lines, starting and ending at midnight
#' 
#' @param data The dataframe
#' @param horodatedebut The beginning time
#' @param horodatefin The ending time
#' @return A data frame with four new columns, Hmin (hour min), Hmax (hmax), xmin (day) and xmax (next day),
#' and new rows
#' @author cedric.briand
#' @examples
#' datatemp<-structure(list(per_dis_identifiant = c(1L, 1L, 1L), 
#' per_date_debut = structure(c(1420056600, 
#'	1420071000, 1420081200), class = c("POSIXct", "POSIXt"), tzone = ""), 
#'	per_date_fin = structure(c(1420071000, 1420081200, 1421000000
#'	), class = c("POSIXct", "POSIXt"), tzone = ""), per_commentaires = c("fonct calcul", 
#'	"fonct calcul", "fonct calcul"), per_etat_fonctionnement = c(1L, 
#'	0L, 0L), per_tar_code = 1:3, libelle = c("Fonc normal", "Arr ponctuel", 
#'	"Arr maint")), .Names = c("per_dis_identifiant", "per_date_debut", 
#'	"per_date_fin", "per_commentaires", "per_etat_fonctionnement", 
#'	"per_tar_code", "libelle"), row.names = c(NA, 3L), class = "data.frame")
#'  newdf<-split_per_day(data=datatemp,horodatedebut="per_date_debut",
#' horodatefin="per_date_fin")
#' @export
split_per_day<-function(data,horodatedebut,horodatefin){
	if(!horodatedebut%in%colnames(data)) stop("horodatedebut not in column names for data")
	if(!horodatefin%in%colnames(data)) stop("horodatefin not column names for data")	
	data$Hdeb<-as.numeric(strftime(data[,horodatedebut],"%H"))+as.numeric(strftime(data[,horodatedebut],"%M"))/60
	data$Hfin<-as.numeric(strftime(data[,horodatefin],"%H"))+round(as.numeric(strftime(data[,horodatefin],"%M"))/60,2)
	data$xmin<-lubridate::floor_date(data[,horodatedebut],unit="day") # pour les graphiques en rectangle
	data$xmax<-data$xmin+lubridate::days(1)
	# number of times we pass to midnigth
	# round is for when we switch hour
	data$n0<-round(difftime(floor_date(data[,horodatefin],unit="day"),floor_date(data[,horodatedebut],unit="day"),units="days"))
	# rows that will be duplicated
	data$id=sequence(nrow(data))
	data<-data[rep(sequence(nrow(data)),data$n0+1),]
	data$newid<-sequence(nrow(data))
	# within a group where dates overlap between two days
	#the first will and all lines except the last be set 24 for Hfin
	data1<-data%>%filter(n0>0)%>%group_by(id)%>%filter(min_rank(desc(newid)) !=1)%>%mutate("Hfin"=24)
	#replacing rows in data
	data[match(data1$newid,data$newid),]<-data1
	# all except the first will be set 0 to Hdeb
	data2<-data%>%filter(n0>0)%>%group_by(id)%>%filter(min_rank(newid) !=1)%>%mutate("Hdeb"=0)
	#replacing rows in data
	data[match(data2$newid,data$newid),]<-data2
	# now get the sequence of days righly set by adding the number of days to xmin and xmax
	data3<-data%>%filter(n0>0)%>%group_by(id)%>%mutate(xmin=xmin+ as.difftime(rank(newid)-1, units="days"),
			xmax=xmax+as.difftime(rank(newid)-1, units="days"))
	data[match(data3$newid,data$newid),]<-data3
	data<-as.data.frame(data)	
	return(data)
}

#' This function extracts temporal characteristics from a dataframe
#' 
#' 
#' @param data a data frame containing a Date or POSIXt column
#' @param nom_coldt the name of the column containing date or POSIXt entry to
#' be processed
#' @param annee logical do you want a column describing year to be added to the
#' dataframe
#' @param mois logical, add column with month
#' @param quinzaine logical, add column with 15 days
#' @param semaine logical, add column with weeks
#' @param semaine_std logical, add column with standard weeks (using isoweek from lubridate)
#' @param jour_an logical, add column with day of year
#' @param jour_mois logical, add column with day of month
#' @param heure logical, add column with hour
#' @return The dataframe with date column filled
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @export
fun_date_extraction=function(data, # tableau de donnees e importer
		nom_coldt, # nom de la colonne
		annee=TRUE,
		mois=TRUE,
		quinzaine=FALSE,
		semaine=TRUE,
		semaine_std=FALSE,
		jour_an=FALSE,
		jour_mois=TRUE,
		heure=FALSE                           
){
	if (annee) data$annee <- as.factor(strftime(as.POSIXlt(data[,nom_coldt]), format="%Y"))                        
	if (mois) data$mois <- as.factor(strftime(as.POSIXlt(data[,nom_coldt]), format="%m"))
	# %b Abbreviated month name in the current locale. (Also matches full name on input.)
	if (quinzaine) {
		data$quinzaine=ceiling(as.numeric(strftime(as.POSIXlt(data[,nom_coldt]),
								format="%W"))/2)
		data$quinzaine <- as.character(data$quinzaine)
		data$quinzaine[as.numeric(data$quinzaine)<10] <- paste("0", data$quinzaine[as.numeric(data$quinzaine)<10],sep="")
		data$quinzaine <- as.factor(data$quinzaine)
	}
	if (semaine) data$semaine <- as.factor(strftime(as.POSIXlt(data[,nom_coldt]), format="%W"))
	#%W : Week of the year as decimal number (00e53) using Monday as the first day of week (and typically with the first Monday of the year as day 1 of week 1). The UK convention
	if (jour_an) data$jour_365 <- strftime(as.POSIXlt(data[,nom_coldt]), format="%j")                          
	if (jour_mois) data$jour_mois <- as.factor(strftime(as.POSIXlt(data[,nom_coldt]), format="%d"))  
	# %d :  Day of the month as decimal number (01e31).
	if (heure) data$heure <- as.factor(strftime(as.POSIXlt(data[,nom_coldt]), format="%H"))  
	#%H     Hours as decimal number (00e23).    
	if (semaine_std) data$semaine_std=lubridate::isoweek(as.POSIXlt(data[,nom_coldt]))
	return(data)
}    


#' Builds a table with colors to merge with a dataframe for later
#' use in ggplot. An initial check will be done
#' on the name of the color vector. A data frame is built. It contains a column color which is a factor.
#' The factor order match the order of the vector (not the alphabetical order of the colors).

#' 
#' @param color Either null (default) or a named vector of colors, the
#' names should correspond to the values of vec 
#' @param  vec The vector to match the color with, if a named vector
#' or color is supplied the names should match
#' @param palette, the name of the RColorBrewer palette, defaults to "Set2", ignored for other
#' color gradient functions and if a named vector of colors is provided
#' @param color_function, the name of the function used to brew the colors, one for 
#' "brewer.pal", "gray.colors", "random", default to "brewer.pal, this argument is ignored if a
#' named vector of color is passed.
#' @return A dataframe with two columns, the vector (name) and the color (color) as a reordered factor
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @export
colortable <- function(color=NULL, vec, palette="Set2", color_function=c("brewer.pal","gray.colors","random")){
	color_function <- match.arg(color_function, choices = c("brewer.pal","gray.colors","random"))
	if (is.null(color)) {
		if (color_function == "brewer.pal") {
			number_available <- RColorBrewer::brewer.pal.info[rownames(RColorBrewer::brewer.pal.info)==palette,"maxcolors"]
			if (number_available>=length(vec)){
			color <- RColorBrewer::brewer.pal(length(vec),name=palette)[1:length(vec)] # 1:length(vec) as palette return minimum 3 values
			}	 else {
				message(gettextf("Palette %s has only got %s values and you need %s", palette, number_available, length(vec)))
				qual_col_pals <- RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
				color <- sample(unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))),length(vec))
			}
		} else if (color_function == "gray.colors"){
			color=grDevices::gray.colors(length(vec))
		} else if (color_function == "random"){
			color <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
			color <- sample(color, size=length(vec))
		}
		names(color)<-vec
	} else if (length(color) != length(vec)){
		funout(gettextf("The color argument should have length %s", length(vec)), arret=TRUE)
	}
	if (!all(names(color)%in%vec)) {
		stop (gettextf("The following name(s) %s do not match vector name: %s",
						names(color)[!names(color)%in%vec],
						paste(vec, collapse=", ")))
	}
	# creating a data frame to pass to merge later (to get the color in the data frame)
	cs <- data.frame(name=names(color), color=color)
	# problem with different order (set by color name) implying different order
	# in the graph (ie by color not by car_val_identifiant
	cs$color <- as.factor(cs$color)
	bonordre <- match(cs$color, levels(cs$color))
	cs$color  <-  factor(cs$color, levels(cs$color)[bonordre])
	return(cs)
}


#' this function displays text and will be used to convey stacomiR message in shiny
#' 
#' 
#' @param text The text to displaying the R 
#' console and later in shiny
#' @param arret Should this cause the program to stop ?
#' @param ... Additional parameters passed to print
#' @return nblignes Assigned in envir_stacomi
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @keywords internal
funout <- function(text,arret=FALSE,...){
	if(arret) stop(text) else print(text,quote=FALSE,...)
}

#' this function gets the schema from envir stacomi and throws warning
#' 
#' @param default passed to rlang::get_env
#' @return The schema in envir_stacomi
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @keywords internal
get_schema <- function(default=NULL){
	if (!exists("envir_stacomi")) stop("envir_stacomi not created did you run stacomi() ?")
	sch <- rlang::env_get(envir_stacomi, "sch", default=default)
	if (is.null(sch)) stop("program failure, sch not in envir_stacomi")
	return(sch)
}

#' this function gets the name of the stucture as it is set in the database
#' 
#' @return The name of the structure (org_code)
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @keywords internal
get_org <- function(){
return(toupper(gsub("\\.", "", get_schema())))
}