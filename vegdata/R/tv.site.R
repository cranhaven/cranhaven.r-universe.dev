#' Load site data from Turboveg Database
#'
#' @name tv.site
#' @description
#'   Loading Turboveg header data and do basic data evaluation. Empty columns are eliminated and warnings about possibly wrong '0' values are performed
#' @export
#' @param db (character) Name of your Turboveg database(s). Directory name containing tvabund.dbf, tvhabita.dbf and tvwin.set.
#' @param tv_home (character) Turbowin installation path. Optional, if Turbowin is either on "C:/Turbowin" or "C:/Programme/Turbowin".
#' @param drop (logical) Drop variables without values.
#' @param common.only (logical) Import only header data with the same name in all databases.
#' @param check.relevenumbers (logical) Are duplicate PlotObservationID's allowed. Default is FALSE.
#' @param replace.names (data.frame) replace variable names. Useful if using multiple source databases. Data frame with names to be replaced in first and replacing names in second column.
#' @param \dots Additional options like \code{dec} for type.convert
#' @details
#'   Please specify pathnames below but not above Turbowin/Data. Can be a single database or a character vector of multiple databases. In the latter case you have to assure, that all databases use the same taxonomic reference list.
#'   You can use the example in the final output line to make a summary statistic for attributes with potentially misleading '0' values. Just delete the \" at beginning and end.
#'
#' @return data.frame of site variables.
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#'
#' @keywords Turboveg

tv.site <- function (db, tv_home, drop = TRUE, common.only = FALSE, check.relevenumbers = TRUE, replace.names, ...)
{
  if (missing(tv_home)) tv_home <- tv.home()
  site <- read.dbf(file.path(tv_home, "Data", db[1], "tvhabita.dbf"), as.is=TRUE)
  names(site) <- TCS.replace(names(site))
  if(!missing(replace.names))
    for(i in 1:nrow(replace.names))
      names(site) <- sub(paste('^', replace.names[i,1], '$', sep=''), replace.names[i,2], names(site))
  if(suppressWarnings(any(site[,sapply(site, is.numeric)] < -1e+05, na.rm = TRUE)))
    message(paste("WARNING! Values less than -100,000. \n", "WARNING! tvhabita.dbf may be corrupt. \n", "WARNING! Please correct by im- / exporting e.g. with OpenOffice."))
  if(length(db) > 1)
    for(i in 2:length(db)) {
	    site.tmp <- read.dbf(file.path(tv_home, 'Data', db[i],'tvhabita.dbf'), as.is = TRUE)
	    names(site.tmp) <- TCS.replace(names(site.tmp))
	    if(!any(c('SURF_AREA','AREA_MIN') %in% names(site.tmp))) stop(db[i]) #' plot area must be present
	    if(check.relevenumbers)
  	    if(any(site$PlotObservationID %in% site.tmp$PlotObservationID))
  	      stop('Found duplicate releve numbers in ', db[i] , ' aborting!')
  	cols1 <- names(site)
  	cols2 <- names(site.tmp)
  	if (common.only){
	  	common <- intersect(cols1, cols2)
		  site <- rbind(site[, common], site.tmp[, common])
	  } else {
 		  All <- union(cols1, cols2)
  		miss1 <- setdiff(All, cols1)
  		miss2 <- setdiff(All, cols2)
  		site[, c(as.character(miss1))] <- NA
  		site.tmp[,c(as.character(miss2))] <- NA
  		site <- rbind(site, site.tmp)
  	}
  }

## Conversion of factors
  for(i in names(site)) if(is.character(site[,i])) site[,i] <- iconv(site[,i], getOption('tv.iconv'), '')

  if(!inherits(site$DATE, 'Date')) {
    if(any(is.na(site$DATE)))
      message(sum(is.na(site$DATE)), ' releves without date. Not converted from factor to date format.') else {
      site$DATE <- gsub('/','',site$DATE)
      index <- nchar(as.character(site$DATE))==4
      fun <- function(x) paste(x,'0101',sep='')
      if(any(index))  site$DATE[index] <- fun(site$DATE[index])
      index <- nchar(as.character(site$DATE))==6
      fun <- function(x) paste(x,'01',sep='')
      site$DATE[index] <- fun(site$DATE[index])
      site$DATE <- as.Date(site$DATE, '%Y%m%d')
    }
  }
  #'#'#' Survey Area
  n <- sum(site$SURF_AREA == 0 | is.na(site$SURF_AREA))
  if(n>0) message(paste(n, ' releves without survey area'))
  site$SURF_AREA[site$SURF_AREA==0] <- NA

##
  fun <- function(x) all(is.na(x))
  na <- apply(site, 2, fun)
  if (drop & any(na)) {
    message('Some columns contain no data and are omitted.')
    print(names(site)[na], quote = FALSE)
    site <- site[, !na]
  }
  fun.2 <- function(x) all(x == 0 | is.na(x))
  leer <- apply(site, 2, fun.2)
  if (drop & any(leer)) {
      message('Some numeric columns contain only 0 values and are omitted.')
	    print(names(site)[leer], quote = FALSE)
	    site <- site[, !leer]
  }
  fun.3 <- function(x) is.numeric(x) & any(x == 0, na.rm = TRUE)
  null <- logical()
  for (i in 1:length(site)) null[i] <- fun.3(site[, i])
  if (any(null) && getOption('warn') >= 0) {
      message("Some numeric fields contain 0 values:")
      print(names(site)[null], quote = FALSE)
      message('Please check if these are really meant as 0 or if they are erroneously assigned because of DBase restrictions.')
      message(paste("If so, use something like:"))
      message("site$Column_name[site$Column_name==0] <- NA", '\n')
    }
  site <- site[order(site$PlotObservationID),]
  if(file.access(file.path(tv_home, 'Data', db[1], 'tvwin.dbf')) == 0) attr(site, 'taxreflist') <- read.dbf(file.path(tv_home, 'Data', db, 'tvwin.dbf'), as.is = TRUE)$FLORA
  return(site)
}

