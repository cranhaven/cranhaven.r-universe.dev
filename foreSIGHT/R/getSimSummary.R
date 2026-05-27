#----------------------------------------------------------------------------

# function to delete the timeseries in the simulation and keep only the metadata
# this function is intentionally kept different from runSystemModel for simplicity
# may also be named getSimSummary or summarizeSim or getSimMeta or getSimMetaData
# ideally two arguments - 1) a simulation 2) an existing summary metadata object (if NULL existing data will not be added to object)
# if a desired simulation is too large to hold in memory, the function can be used to progressively save metadata into a single summary file

#' Produces a summary object containing the metadata of a full simulation
#'
#' \code{getSimSummary} uses a full simulation generated using the function \code{generateScenarios} as input and outputs a summary object containing the
#' metadata of the full simulation. The output summary object may be used as an input to the plotting functions in this package. The output summary object
#' will be much smaller in size than the full simulation for ease of storage and use with the plotting functions.
#' @param sim list; a simulation containing the scenarios generated using the function \code{generateScenarios}.
#' @seealso \code{generateScenarios}, \code{plotPerformanceSpace}, \code{plotPerformanceOAT}
#' @export

getSimSummary <- function(sim) {
  # if using multiple simulations
  # check that all sim controlFile and expSpace are the same
  
  
  # unpacking sim metadata
  repNames <- names(sim[grep("Rep", names(sim))])
  if (is.null(repNames)) stop("There are no replicates in sim.")
  tarNames <- names(sim[[repNames[1]]])
  nRep <- length(repNames)
  nTar <- length(tarNames)
  
  varNames <- names(sim[["Rep1"]][["Target1"]])
  varNames <- varNames[!varNames %in% c("attSim", "targetSim", "parS", "score")]
  
  # subsetting sim to simSummary
  simSummary <- list()
  for (r in 1:nRep) {
    if (!is.list(sim[["controlFile"]])) {
      simSummary[[repNames[r]]] <- NULL
    } else {
      for (t in 1:nTar) {
        # save all info except the simulation variables
        indVars <- which(names(sim[[repNames[r]]][[tarNames[t]]]) %in% varNames)
        # othrVars <- names(sim[[repNames[r]]][[tarNames[t]]])[-indVars]
        simSummary[[repNames[r]]][[tarNames[t]]] <- sim[[repNames[r]]][[tarNames[t]]][-indVars]
        
        # for (i in 1:length(othrVars)) {
        #   simSummary[[repNames[r]]][[tarNames[t]]][[othrVars[i]]] <- sim[[repNames[r]]][[tarNames[t]]][[othrVars[i]]]
        # }
        #
        simSummary[[repNames[r]]][[tarNames[t]]][["seed"]] <- sim[[repNames[r]]][[tarNames[t]]][[varNames[1]]][["seed"]]
      }
    }
  }
  # other metadata
  simSummary[["simDates"]] <- sim[["simDates"]]
  simSummary[["expSpace"]] <- sim[["expSpace"]]
  simSummary[["controlFile"]] <- sim[["controlFile"]]
  
  return(simSummary)
}
