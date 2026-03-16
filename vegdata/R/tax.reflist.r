

load.refl <- function(refl = tax.refl(), ...) {
  tv_home <- tv.home(...)
  dbf <- 'species.dbf'
  refl.dir <- if(dir.exists(file.path(tv_home, 'species'))) 'species' else 'Species'
  reflist.path <- file.path(tv_home, refl.dir, refl, dbf)
  # see if files exist
  if(file.exists(reflist.path)) {
    species <- read.dbf(reflist.path, as.is = TRUE)
    names(species) <- TCS.replace(names(species))
    return(species)
    } else {
     message(paste('Taxonomic reference list file', refl, 'is not accessible. Search path was', reflist.path))
     species <- NULL
  }
}


#' @export
print.refllist <- function(x, ...) {
  cat(sprintf("<Taxonomic Reflist> %s", tax.refl()), sep = "\n")
  cat(sprintf("  version: %s", x$Version), sep = "\n")
  cat(sprintf("  url:        %s", x$url), sep = "\n")
  cat(sprintf("  citation:   %s", x$citation), sep = "\n")
  cat(sprintf("  path:       %s", .my_cache$cache_path_get()), sep = "\n")
  cat("  data:", sep = "\n")
  print(x$data)
}

#' Taxon reference list to be used
#' @name tax.refl
#' @export
#' @param refl name of taxonomic reference list
#' @param db optional: Turboveg database name
#' @param tv_home Turboveg installation path

tax.refl <- function(refl, db, tv_home) {
  if(missing(tv_home)) tv_home <- tv.home()
  if(!missing(db)) {
    if(file.access(file.path(tv_home, 'Data', db[1], 'tvwin.dbf')) == 0) {
      refl <- read.dbf(file.path(tv_home, 'Data', db[1], 'tvwin.dbf'), as.is = TRUE)$FLORA
    } else {
      dbattr <- file.path(tv_home, 'Data', db[1], 'tvwin.set')
      if(file.access(dbattr) == 0)
        refl <-  sub('A\002', '', readBin(dbattr,what='character', n=3)[3]) else
    stop('Database attribute file (tvwin.dbf) from database "', db[1], '" not available. Please specify name of taxonomic reference list instead!')
    }
  } else
    if(!missing(refl)) {
      if(tolower(refl) %in% c('eurosl')) {
        refl <- 'EuroSL'} else {
      rli <- list.dirs(path = file.path(tv_home, "Species"), full.names = FALSE, recursive = FALSE)
      # rli <- sapply(rli, function(x) substring(x, nchar(tv_home) + 10), USE.NAMES = FALSE)
      if(length(rli) > 0) refl <- match.arg(refl, rli)
        }
    } else
      refl <- if(!is.null(getOption('tax.refl'))) getOption('tax.refl') else
        stop('You need to specify a valid reference list name.')
  if(tolower(substr(refl, 1,8)) == 'germansl')
    refl <- paste('GermanSL', substring(refl, 9, nchar(refl)), sep='')

  options(tax.refl = refl)
  return(refl)
 }

# tax.refl('eurosl')
# getOption('tax.refl')

