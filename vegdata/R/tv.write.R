#' Write species-plot observations and site information to Turboveg database.
#'
#' @name tv.write
#' @description Write species-plot observations and site information to Turboveg database.
#'
#' @export
#' @param x (data.frame) Either observations data.frame with PlotObservationID, TaxonUsageID and COVER_CODE (see \link{tv.obs}) columns or vegetation matrix of class "veg".
#' @param site (character) Header data for plots.
#' @param name (character) Name of the new database.
#' @param tvadmin (data.frame) Dataframe with plot UUID's and Turboveg columns from TvAdmin.dbf. A new file with new unique identifiers will be created if omitted.
#' @param remarks (data.frame) Remarks in Turboveg format if the comments for individual plots exceed 254 characters. See remarks.dbf in Turboveg databases. An empty file will be created if omitted.
#' @param dict (character) Turboveg dictionary name
#' @param cover (logical) Use of covercodes or (mean) cover percentages, see Details.
#' @param drop (logical) Drop columns which are empty or contain only NA values.
#' @param obl (logical) Add obligatory fields defined in the TV dictionary but not present in the site data table.
#' @param overwrite  (logical) Should an existing database be overwritten.
#' @param \dots additional arguments
#'
#' @details By default Covercode is written to Turboveg. This is only meaningful, if correct CoverScales are given in the site dataframe. Unique plot ID's are stored in *TvAdmin.dbf*. If you want to preserve already given UUID's you have to prepare an appropriate data.frame. Look for existing *TvAdmin.dbf* files for necessary columns.
#'
#' @return Five files will be created in "tv_home/Data/databasename" directory. *tvabund.dbf* with occurrence information n long format, *tvhabita.dbf* with plot information, remarks.dbf with comments longer then 255 characters, *TvAdmin.dbf* with plot UUID's and tvwin.dbf with information about taxonomic reference list, and dictionary used.
#'
#' @seealso  tv.veg
#'
#' @author Florian Jansen @email florian.jansen@uni-rostock.de
#'
#' @keywords Turboveg


if(getRversion() >= "2.15.1")  utils::globalVariables(c("DoWritedbf"))

tv.write <- function(x, site, name, tvadmin, remarks, dict = '', cover=c('code','perc'), drop = FALSE, obl = TRUE, overwrite = FALSE,  ...) {
  cover <- match.arg(cover)
  if('veg' %in% class(x)) {
    X <- reShape.veg(x, ...)
    if(cover == 'perc') {
      X$COVER_CODE <- as.character(X$COVER_PERC)
      site$COVERSCALE <- '00'
      X <- X[,c('PlotObservationID','TaxonUsageID', 'COVER_CODE', 'LAYER')]
    }
  } else {
    if(!any(c('tv.obs','vw.obs') %in% class(x)))
      warning('Species observations should be either of class \"tv.obs\" or \"vw.obs\".')
    if(!all(c('PlotObservationID') %in% names(x)))
      stop('table of species observations must contain a column called PlotObservationID')
    X <- x
    binrel <- unique(X$PlotObservationID[na.omit(as.numeric(X$"COVER_CODE") > 100)])
    if(length(binrel) > 0) X$"COVER_CODE"[X$PlotObservationID == binrel] <- word2bin(as.numeric(X$"COVER_CODE"[X$PlotObservationID == binrel]))
    X$"COVER_CODE"[X$"COVER_CODE" == '100'] <- '9X'
    names(X) <- TV.replace(names(X))
  }
  refl <- if(!is.null(attr(X, "taxreflist"))) attr(X, "taxreflist") else tax.refl()

  if(drop) {
    # Delete empty columns ####
    na <- apply(site, 2, function(x) all(is.na(x) | x == ''))
    if (any(na)) site <- as.data.frame(site[, !na])
    leer <- apply(site, 2, function(x) all(x == 0 | is.na(x)))
    if (any(leer)) site <- as.data.frame(site[, !leer])
  }
  for(i in names(site)) if(is.character(site[,i])) {
    site[is.na(site[,i]),i] <- ''
    site[,i] <- iconv(site[,i], '', getOption('tv.iconv'))
  }

  if(obl) {

    ### Add obligatory fields from dictionary ####
    dbasedic <- read.dbf(file.path(tv.home(), 'Popup', dict, 'dbasedic.dbf'), as.is=TRUE)
    oblig <- dbasedic[dbasedic$FILE_NR == 2, 'FIELD_NAME']
    oblig <- TCS.replace(oblig)
    for(m in oblig[!oblig %in% names(site)])   site[,m] <- ''
    site <- site[, match(unique(c(oblig, names(site))), names(site))] # order columns
  }

  if(!overwrite)
    if(file.exists(file.path(options('tv_home'), 'Data', name, 'tvhabita.dbf')))
      stop('Database ', name, ' already exists. Nothing will be exported.')

  site$DATE <- gsub('-','', site$DATE)

  ### Write Turboveg tvhabita and tvabund ####
  dir.create(file.path(options('tv_home'), 'Data', name), showWarnings = if(overwrite) FALSE else TRUE)
  if("tbl" %in% class(X)) X <- as.data.frame(X)
  write.dbf(X, max_nchar = 250, file.path(options('tv_home'), 'Data', name, 'tvabund.dbf'))
  # write TvAdmin ####
  if(missing(tvadmin)) {
        tvadmin <- data.frame(PlotObservationID=site$PlotObservationID, SOURCE_DB='R',  GUID=replicate(nrow(site), paste('{', uuid::UUIDgenerate(), '}', sep='')), CREAT_USER=Sys.getenv('USER'), CREAT_DATE=Sys.Date(), MOD_USER=Sys.getenv('USER'),	MOD_DATE=Sys.Date(), NDFF_QUAL=as.integer(0))
      } else if(is.character(tvadmin))
        tvadmin <- read.dbf(file.path(options('tv_home'), 'Data', tvadmin, 'TvAdmin.dbf'), as.is=TRUE)

  TvAdmin <- tvadmin
  TvAdmin <- TvAdmin[TvAdmin$PlotObservationID %in% site$PlotObservationID,]
  TvAdmin$MOD_USER[is.na(TvAdmin$MOD_USER)] <- Sys.getenv('USER')
  TvAdmin$MOD_DATE[is.na(TvAdmin$MOD_DATE)] <- format(Sys.Date())
  names(TvAdmin) <- TV.replace(names(TvAdmin))
  if("tbl" %in% class(TvAdmin)) TvAdmin <- as.data.frame(TvAdmin)
  write.dbf(TvAdmin, max_nchar = 250, file.path(options('tv_home'), 'Data', name, 'TvAdmin.dbf'))

  # write Remarks ####
  if(missing(remarks)) remarks <- data.frame(PlotObservationID=numeric(), REMARKS=character()) else
    if(nrow(remarks) > 0) remarks <- remarks[remarks$PlotObservationID %in% site$PlotObservationID,]
  names(remarks) <- TV.replace(names(remarks))
  if("tbl" %in% class(remarks)) remarks <- as.data.frame(remarks)
  suppressWarnings(
    write.dbf(remarks, max_nchar = 250, file.path(options('tv_home'), 'Data', name, 'remarks.dbf'))
    )
  options(warn=-1)
  # write tvwin.dbf ####
  write.dbf(data.frame(FLORA=refl, MINALLOW=0, MAXALLOW=0, MINACTUAL= min(site$PlotObservationID),	MAXACTUAL=max(site$PlotObservationID), MAP='', DICTIONARY=dict, META=''), max_nchar = 250, file = file.path(options('tv_home'),'Data', name, 'tvwin.dbf'))
  # write tvhabita.dbf
  names(site) <- TV.replace(names(site))
  if("tbl" %in% class(site)) site <- as.data.frame(site)
  write.dbf(site, max_nchar = 250, file.path(options('tv_home'), 'Data', name, 'tvhabita.dbf'))

    cat('Turboveg database', name, 'written to', file.path(options('tv_home'), 'Data', name),'\n')
}
