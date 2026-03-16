#' Dataframe of plot-species observations directly from Turboveg
#' @name tv.obs
#'
#' @description{Dataframe of plot-species observations directly from Turboveg.}
#'
#' @export
#' @param db (character) Name of your Turboveg database. This is the directory name containing tvabund.dbf, tvhabita.dbf and tvwin.set. Please include pathnames below but not above Turbowin/Data.
#' @param tv_home (character) Turbowin installation path. If not specified function \code{\link{tv.home}} tries to discover.
#' @param \dots additional arguments
#'
#' @return {Data.frame of species occurrences in Turboveg format, that is every occurrence is a row with releve number, species number, layer, cover code and optional additional species-plot information.}
#'
#' @seealso \code{\link{tv.veg}}
#'
#' @examples
#'   \dontrun{
#'     # Turboveg installation needed
#'     obs <- tv.obs('taxatest')
#'     head(obs)
#'   }
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#' @keywords Turboveg



"tv.obs" <- function(db, tv_home, ...) {
    if(missing(tv_home)) tv_home <- tv.home() else
      if(tv_home != tv.home()) warning(paste("Given Turboveg root directory:", tv_home, "differs from the global root directory given by getOption('tv_home'):", getOption('tv_home')))
    # Observations ####
    obs <- read.dbf(file.path(tv_home, 'Data', db[1],'tvabund.dbf'), as.is = TRUE)
    names(obs) <- TCS.replace(names(obs))
    ## Combine multiple databases ####
    # Would be more efficient with e.g. rbindlist but I am not sure if it is worth the new package dependency
    if(length(db) > 1) {
      cat('More than 1 database, trying to combine.\n')
    refl.1 <- tax.refl(db = db[1])
      for(i in 2:length(db)) {
      	refl.i <- tax.refl(db = db[i])
      	if(refl.1 != refl.i) {
          cat(db[1], 'vs.', db[i])
          stop('You are using different taxonomic reference lists in your databases!')
      	}
      	obs.tmp <- read.dbf(file.path(tv_home, 'Data', db[i],'tvabund.dbf'))
        names(obs.tmp) <- TCS.replace(names(obs.tmp))
        if(any(unique(obs$PlotObservationID) %in% unique(obs.tmp$PlotObservationID))) {
        		warning(db[i])
            stop('Overlap of releve numbers between the databases!')
        	}
    	  if(any(!names(obs) %in%  names(obs.tmp) ) | any(!names(obs.tmp) %in% names(obs))) {
  	      miss1 <- setdiff(colnames(obs), colnames(obs.tmp))
  	      miss2 <- setdiff(colnames(obs.tmp), colnames(obs))
  	      obs[, miss2] <- NA
  	      obs.tmp[, miss1] <- NA
  	      obs <- rbind(obs, obs.tmp)
        } else obs <- rbind(obs, obs.tmp)
      }
    }
    ## Cover codes: 9X and Binary coding
    if(any(obs$TaxonUsageID == -1)) warning('Taxon number -1 found. This is most likely due to deleted but not purged entries in tvabund.dbf. Please Reindex Turboveg database, see Turboveg Help file.')
    class(obs) <- c('tv.obs','data.frame')
    return(obs)
}


