if(getRversion() >= "2.15.1")  utils::globalVariables(c("lc.1"))

#' Tabulates vegetation tables from Turboveg database
#' @name tv.veg
#' @aliases tv.veg
#'
#' @description Tabulates vegetation tables from Turboveg resp. VegetWeb database, including taxonomic emendation and layer combination. Using various default parameters for the included functions.
#' It is a wrapper for *tv.obs*, *taxval*, *tv.coverperc* and creating a vegetation matrix
#'
#' @export
#' @param db Name of your Turboveg database. Directory name containing tvabund.dbf, tvhabita.dbf and tvwin.set. Please specify pathnames below (if you sorted your databases in subfolders) but not above Turbowin/Data.
#' @param taxval Should taxonomic valuation (see \code{\link{taxval}}) be performed?
#' @param convcode Should cover code be converted to percentage values?
#' @param lc Layer combination type. Possible values: layer (default), sum, mean or max, see details
#' @param pseudo List used for layer combinations, see details
#' @param values Name of the variable which should be used for the vegetations matrix.
#' @param spcnames Should species numbers be replaced by shortletters or ScientificNames? Layer information is appended with dot.
#' @param dec Number of decimals for cover values in the resulting vegetation matrix.
#' @param cover.transform If you want to transform the abundancce values within your samples
#'     you can choose 'pa' for presence-absence or 'sqrt' for the \code{dec} rounded square root.
#' @param obs Observations, optional
#' @param site plot header data, see \code{\link{tv.site}}
#' @param refl Taxonomic reference list, optional
#' @param RelScale Vector with Cover Scale code per Releve.
#' @param tv_home Turbowin installation path.
#' @param ... additional arguments for included functions
#'
#' @details  \code{layer} means, the different layers are combined assuming there independence (a species occurring in two layers with a cover of 50\% will result in a overall cover of 75\%. \code{sum} will sum up cover values of all layers With option \code{pseudo} you can decide, which layers should be combined. Give a list with a combination data.frame and second the name of the column for combination. The default is \code{pseudo = list(lc.1, c('LAYER'))}, where lc.1 is a data.frame \code{data(lc.1)}, which will combine all tree layers, all shrub layers and all layers below shrubs. An alternative would be data(lc.all), combining all layers. With option pseudo=NULL there will be no layer aggregation.
#'
#' @return an object of class matrix with (combined) cover values.
#'
#' @seealso{
#'   \code{\link{taxval}}, \code{\link{tv.coverperc}}, \code{\link{tv.obs}}, \code{\link{tv.site}}
#' }
#'
#' @examples
#'   \dontrun{
#'     vignette("vegdata")
#'     #' If you have Turboveg installed on your computer try for a beginning
#'     #' tv.veg('databasename', tax=FALSE).
#'     args(tv.veg)
#'     help('taxval')
#'
#'     veg <- tv.veg('taxatest')
#'     names(veg)
#'     tv.veg('taxatest', uncertain=list('DET_CERT', data.frame(0:2,c('pres','agg','agg'))),
#'            pseudo=list(lc.0,'LAYER'), genus = 'delete')
#'   }
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#'
#' @keywords misc manip
#'

tv.veg <- function (db,
  taxval = TRUE,
  convcode = TRUE,
  lc = c("layer","mean","max","sum","first"),
  pseudo,
  values = "COVER_PERC",
  spcnames = c('shortletters','ScientificNames','Numbers'),
  dec = 0, cover.transform = c('no', 'pa', 'sqrt'),
  obs,
  site,
  refl = tax.refl(),
  RelScale,
  tv_home,
  ...)
{
  options(warn=1)
  ## Checks
    lc <- toupper(match.arg(lc))
    cover.transform <- match.arg(cover.transform)
    spcnames = match.arg(spcnames)
    if(missing(tv_home)) tv_home <- tv.home()
    if(missing(obs)) {
      if(missing(db)) stop('Either "obs" and "refl" or a Turboveg database name "db" must be given.')
      message('Reading tvabund.dbf\n')
      obs <- tv.obs(db, tv_home)
      } else names(obs) <- TCS.replace(names(obs))
    lc.1 <- data.frame(LAYER=0:9, COMB=c(0,rep('Tree',3),rep('Shrub',2),rep(0,4)))
    if(missing(pseudo))
      pseudo <- list(lc.1, 'LAYER')
## Taxa
    if(missing(refl) & is.null(tax.refl())) {
      if(missing(db))
        stop('Either "obs" and "refl" or a Turboveg database name "db" must be given.') else
      refl <- tax.refl(db = db[1], tv_home = tv_home)
    } else refl <- tax.refl()
    message('Taxonomic reference list: ', refl)
    if(taxval) {
      obs <- taxval(obs=obs, refl = refl, ...)
    }
## CoverCode
    if(missing(site)) {
      message('Reading tvhabita.dbf')
      site <- tv.site(db, tv_home, verbose = FALSE)
    }
    if(convcode){
      message('converting cover code ... \n')
        if(missing(RelScale)) {
          if(missing(db)) stop('\nEither database name or a vector with CoverScale per releve has to be permitted, to cope with Cover Scale information\n')
          RelScale <- site[, c("PlotObservationID", "COVERSCALE")]
        }
     	  obs <- tv.coverperc(db=db, obs=obs, RelScale = RelScale, tv_home = tv_home, ...)
    } else {
      if (!any(names(obs) == values)) stop(paste(values, " could not be found."))
          # obs[,values] <- type.convert(as.character(obs[,values], as.is = TRUE))
          if(is.factor(obs[, values])) {
            lc='first'
            warning("Multiple occurrences of a species in one layer of a releve can not be supported without cover code conversion.
                  Only the first occurrence will be used!")}
    }
## empty plots
    if(!missing(site)) {
    noobs <- site$PlotObservationID[!site$PlotObservationID %in% obs$PlotObservationID]
    if(length(noobs) > 0)
      for(i in 1:length(noobs)) {
        obs[(nrow(obs)+1), ] <- NA
        obs$PlotObservationID[nrow(obs)] <- noobs[i]; obs$TaxonUsageID[nrow(obs)] <- 0; obs$LAYER[nrow(obs)] <- 0
      }
    }
## Pseudo-Species / Layer
    # print('Pseudo species')
    if(!is.null(pseudo)) {
      message('creating pseudo-species ... \n')
	if(length(pseudo[[2]]) > 1) stop('Possibility to differentiate more than one plot-species attribute not yet implemented. \n
	Please contact <florian.jansen@uni-rostock.de>.')
        obs$COMB <- pseudo[[1]][, 2][match(obs[, pseudo[[2]]], pseudo[[1]][,1])]
        collab <- paste(obs$TaxonUsageID, obs$COMB, sep = ".")
    } else  collab <- as.vector(obs$TaxonUsageID)
    rowlab <- as.vector(obs$PlotObservationID)
    message('combining occurrences using type ', toupper(lc), ' and creating vegetation matrix ... \n')
    gc(TRUE, verbose = FALSE)
    layerfun <- function(x) round((1 - prod(1 - x/100)) * 100, dec)
    results <- switch(toupper(lc),  # Inflate matrix
      LAYER = tapply(obs[, values], list(rowlab, collab), layerfun),
      MEAN  = tapply(obs[, values], list(rowlab, collab), mean),
      MAX   = tapply(obs[, values], list(rowlab, collab), max),
      SUM   = tapply(obs[, values], list(rowlab, collab), sum),
      FIRST = tapply(obs[, values], list(rowlab, collab), stringr::word) )
    results[is.na(results)] <- 0
    results <- as.data.frame(results)

## Names
    # print('names')
    if(spcnames %in% c('shortletters','ScientificNames')) {
      message('replacing species numbers with ', spcnames, ' names ... \n')
      if(is.null(pseudo)) {
      	species <- tax(as.numeric(colnames(results)), syn=FALSE, refl = refl)
      	if(spcnames=='shortletters') colnames(results) <- species$LETTERCODE[match(colnames(results), species$TaxonUsageID)]
      	if(spcnames=='ScientificNames') colnames(results) <- gsub(' ','_', species$TaxonName[match(colnames(results), species$TaxonUsageID)] )
       } else {
	st <- unlist(strsplit(colnames(results), ".", fixed = TRUE))
	colspc <- st[seq(1, length(st), 2)]
	species <- tax(as.numeric(colspc), refl = refl)

	if(spcnames=='shortletters') coln <- as.character(species$LETTERCODE[match(as.numeric(colspc), species$TaxonUsageID)])
	if(spcnames=='ScientificNames') coln <- gsub(' ','_', species$TaxonName[match(as.numeric(colspc), species$TaxonUsageID)])

  ll <- st[seq(2, length(st), 2)]
  cn <- replace(coln, which(ll != '0'), paste(coln, ll, sep = ".")[ll != '0'])
	colnames(results) <- cn
	}
   }
   if (any(is.na(colnames(results)))) warning("Some taxa without names, check reference list!")
#  rownames(results) <- unique(rowlab)
## Result
#   results <- results[, order(names(results))]
   if(cover.transform!='no') {
      if(cover.transform == 'pa') results <- as.data.frame(ifelse(results > 0, 1,0))
      if(cover.transform == 'sqrt') results <- as.data.frame(round(sqrt(results),dec))
   }
  results <- results[, colSums(results)> 0]
  class(results) <- c("veg", "data.frame")
  attr(results, 'taxreflist') <- refl
  return(results)
}

