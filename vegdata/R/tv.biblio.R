#' Check bibliographic references from Turboveg codes
#' @name tv.bib
#'
#' @description Check bibliographic references from Turboveg codes
#'
#' @export
#' @param x (character) Turboveg reference code(s), e.g. "000001"
#' @param db (character) Database name. Needed to select appropriate TV Dictionary folder.
#' @param tv_home (character) Turbowin installation path. If not specified function [tv.home()] tries to discover.
#' @param dict (character) Name of Turboveg Dictionary (term lists for header data) if not the default one.
#' @param \dots additional arguments
#'
#' @return Dataframe of (selected) bibliographic references (when assigned to an object).
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#'
#' @keywords Turboveg

tv.bib <- function(x='all', db, dict = tv.dict(db), tv_home, ...) {
  if(missing(tv_home)) tv_home <- tv.home()
  if(missing(db) & missing(dict)) {
    message('Using tvrefenc.dbf from default dictionary.')
    dict = ''
  }
  if(dict == 'default') dict <- ''
  bibliopath <- file.path(tv_home, 'Popup', dict, 'tvrefenc.dbf')
  biblio <- read.dbf(bibliopath, as.is=TRUE)
  for(i in c('AUTHOR','TITLE','PUBLISHED', 'ADDRESS'))
    if(i %in% names(biblio)) biblio[,i] <- iconv(biblio[,i], getOption('tv.iconv'), "")
  if(x[1] != 'all') {
    x <- as.numeric(unique(x))
    biblio <- biblio[match(x, as.numeric(biblio$REFERENCE)),]
    if(nrow(biblio) == 1) print(biblio)
  }
  invisible(biblio)
}


tv.biblio <- tv.bib
