#' Read list of available Turboveg2 databases in given Turboveg directory
#'
#' @export
#' @name tv.db
#' @param path (character) directory path inside Turboveg/data directory
#' @return List of databases below specified path
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#' @keywords Turboveg
#' @usage tv.db(path)
#'
tv.db <- function(path='.') {
  tv.filenames <- c('tvhabita.dbf', 'tvabund.dbf', 'remarks.dbf', 'TvAdmin.dbf')
  valid.TVdb <- function(p) {
    all(tv.filenames %in% list.files(file.path(tv.home(), 'Data', path, p)))
  }
  ###
  dir <- list.dirs(path = file.path(tv.home(), 'Data', path), full.names = FALSE, recursive = TRUE)
  dir <- dir[dir != ""]
  if(length(dir)>0) {
    for(d in dir) {
      fiLe <- list.files(file.path(tv.home(), 'Data', path, d))
      FiLe <- fiLe[which(tolower(fiLe) %in% tolower(tv.filenames) & !fiLe %in% tv.filenames)]
      if(length(FiLe) > 0)
        for(i in 1:length(FiLe))
          file.rename(from = file.path(tv.home(), 'Data', path, d, FiLe[i]), to = file.path(tv.home(), 'Data', path, d, tv.filenames[which(tolower(tv.filenames) %in% tolower(FiLe))][i]))
    }
    valid.dir <- dir[sapply(dir, valid.TVdb)]
    return(file.path(path, valid.dir))
  } else paste('No TV directory found.')
}



#' Read list of available Turboveg2 databases in given Turboveg directory
#'
#' @export
#' @name tv.db
#' @param db (character) name of Turboveg database/directory
#' @param tv_home (character) path of Turboveg instalation
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#' @keywords Turboveg
#' @usage tv.dict(db, tv_home)
#'
tv.dict <- function(db, tv_home) {
  if(missing(tv_home)) tv_home <- tv.home()
  if(length(db) > 1) {
    for(i in 1:length(db))
      print(paste(db[i], ':   ', read.dbf(file.path(tv_home, 'Data', db[i], 'tvwin.dbf'), as.is = TRUE)$DICTIONARY))
    warning('Dictionary given by the first database will be used.')
  }
  if(file.exists(file.path(tv_home, 'Data', db[1], 'tvwin.dbf'))) {
    attrib <- read.dbf(file.path(tv_home, 'Data', db[1], 'tvwin.dbf'), as.is = TRUE)
    if(is.na(attrib$DICTIONARY)) attrib$DICTIONARY <- ''
    return(attrib$DICTIONARY)
  } else {
    dbattr <- file.path(tv_home, 'Data', db[1], 'tvwin.set')
    if(file.access(dbattr)==0) {
      allbytes <- readBin(dbattr, "raw", n = 100, size = 1, endian = "little")
      bin <- sapply(allbytes, readBin, what='character')
      return(readBin(allbytes[(which(bin == 'C')[3]+3):length(allbytes)], what=character()))
    } else warning('neither tvwin.set nor tvwin.dbf can be found')
  }
}


#' Show metainfo of vegetation database or ecodbase
#' @name tv.metadata
#' @aliases tv.metadata
#'
#' @description
#'   Showing "metadata.txt" when specified and saved in Turboveg database directory.
#'   When db = 'eco' and refl specified, metainfo of species attribute table is displayed.
#'
#' @usage tv.metadata(db, refl, tv_home, filename = 'metadata.txt', ...)
#'
#' @param db Turboveg database name
#' @param refl Turboveg taxonomic reference list, declaration only necessary for ecodbase info
#' @param tv_home Turboveg installation path
#' @param filename Name of metainfo file residing in database directory
#' @param ... additional arguments
#'
#' @details
#' Because Turboveg provides no formalised method to store information about database fields,
#'   I suggest to save a simple text file, named for example "metadata.txt" into the directory of your Turboveg database.
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#'
tv.metadata <- function (db, refl, tv_home, filename = 'metadata.txt', ...)
{
  if (missing(tv_home)) tv_home <- tv.home()
  if (db[1] == "eco") {
    if(missing(refl)) refl <- tax.refl(db = db[1], tv_home = tv_home, ...)
    shell.exec(file.path(tv_home, "Species", refl, "metadata-eco.txt"))
  } else {
    for(i in 1:length(db)) {
      meta <- file.path(tv_home, "Data", db[i], filename)
      if (file.access(meta)) stop('No metainfo file "',filename, '" available in directory "', db[1], '".') else
        if(.Platform$OS.type == "windows") shell.exec(meta)  else file.show(meta)
    }}
}
