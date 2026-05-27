
#' Runs a system model and outputs the system performance
#'
#' \code{runSystemModel} uses time series of hydroclimatic variables generated using the function \code{generateScenarios} as input to a systemModel and
#' collates the system performance for all the targets and replicates in the scenarios.
#' @param sim list; a simulation containing the scenarios generated using the function \code{generateScenarios}.
#' @param systemModel a function; The function runs the system model using climate data in a list as input.
#' The function is expected to be created by the user for specific system models. \code{tankWrapper} is an example system model function available in this package.
#' \code{runSystemModel} calls the function \code{systemModel} with two arguments:
#' \itemize{
#' \item \code{data}: list; the climate data as a list in reference format, with POSIXct formatted \emph{times} and climate variables \emph{*variable_name1*} \emph{*variable_name2*} in vector or matrix.
#' \item \code{systemArgs}: list; containing the other arguments required by the system model.\code{systemModel} unpack the arguments from the list and uses them as required.
#' \item \code{metrics}: string vector; containing the names of the performance metrics that the system model returns. It is recommended that the
#' names also contain the units of the metric. See \code{viewTankMetrics()} for examples.
#' }
#' @param systemArgs a list; containing the input arguments to \code{systemModel}.
#' @param metrics a string vector; the names of the performance metrics the \code{systemModel} function returns.
#' @param varNames a string vector; containing the names of the climate variables that are extracted from sim and used in system model. If \code{NULL}, then \code{varNames} determined from attribute names in \code{sim$expSpace}.
#' @details The \code{runSystemModel} function code is structured to be simple and may be used as an example to create scripts that use scenarios
#' generated using \code{generateScenarios} to run system models in other programming languages. Type \code{runSystemModel} to view the function code.
#' The function \code{tankWrapper} in this package may be used as an example to create user defined functions for the \code{systemModel} argument.
#' Refer to \code{tankWrapper} to understand how the \code{systemModel} is expected to use \code{systemArgs} and return the calculated performance metrics.
#' The \code{systemModel} function is expected to return a named list of performance metrics. The elements of the vector should correspond to \code{metrics}.
#' @return The function returns a list containing the performance metrics calculated by the \code{systemModel}. Each element of the list corresponds to a performance metric and is named using the \code{metrics} argument.
#' Each element contains performance values calculated at all the target points in the exposure space in a matrix with nrow corresponding to the targets and ncol corresponding to the replicates.
#' @seealso \code{tankWrapper}, \code{generateScenarios}
#' @examples
#' \dontrun{
#' # Example using tankWrapper as the systemModel
#' # =====================================================
#' # create an exposure space
#' attPerturb <- c("P_day_all_tot", "Temp_day_all_avg")
#' attPerturbType <- "regGrid"
#' attPerturbSamp <- c(10, 10)
#' attPerturbMin <- c(0.8, -1)
#' attPerturbMax <- c(1.2, 1)
#' expSpace <- createExpSpace(
#'   attPerturb = attPerturb,
#'   attPerturbSamp = attPerturbSamp,
#'   attPerturbMin = attPerturbMin,
#'   attPerturbMax = attPerturbMax,
#'   attPerturbType = attPerturbType
#' )
#' data(tankDat)
#' simScaling <- generateScenarios(
#'   reference = tank_obs,
#'   expSpace = expSpace,
#'   controlFile = "scaling"
#' )
#' # use the simulation to run a system model
#' systemArgs <- list(
#'   roofArea = 205, nPeople = 1, tankVol = 2400,
#'   firstFlush = 2.0, write.file = FALSE
#' )
#' tankMetrics <- viewTankMetrics()
#' systemPerf <- runSystemModel(
#'   sim = simScaling,
#'   systemModel = tankWrapper,
#'   systemArgs = systemArgs,
#'   metrics = tankMetrics[1:2]
#' )
#' }
#' @export


# FUNCTION TO JUST MANAGE THE SIMULATION OF PERFORMANCE
runSystemModel <- function(sim, # output from scenario generator
                           systemModel, # system model function with arguments
                           systemArgs, # arguments of the system model
                           metrics, # names of performance metrics returned
                           varNames = NULL # vector of variable names
) {
  # unpacking sim
  repNames <- names(sim[grep("Rep", names(sim))])
  tarNames <- names(sim[[repNames[1]]])
  
  nRep <- length(repNames)
  nTar <- length(tarNames)
  
  if (is.null(varNames)) {
    varNames <- unlist(lapply(colnames(sim$expSpace$targetMat), get.attribute.varType))
    varNames <- unique(unlist(strsplit(varNames, "/")))
  }
  
  performance <- vector("list", length = length(metrics))
  for (i in 1:length(metrics)) performance[[i]] <- matrix(NA, nrow = nTar, ncol = nRep)
  
  for (r in 1:nRep) {
    for (t in 1:nTar) {
      # initialising scenarioData
      # scenarioData = as.data.frame(sim[["simDates"]],nm = 'times')
      scenarioData <- list(times = sim[["simDates"]])
      varTemp <- list()
      for (v in varNames) {
        varTemp <- sim[[repNames[r]]][[tarNames[t]]][[v]][["sim"]]
        scenarioData[[v]] <- varTemp
      }
      
      
      #### move this to separate function
      
      # store information about replicate, target, and target attributes
      # names of target attributes
      n <- names(sim$expSpace$targetMat)
      # values of target attributes
      v <- as.numeric(sim$expSpace$targetMat[t, ])
      # string combining target attribute names and values (e.g. 'PET_day_all_avg_m_1__P_day_all_tot_m_0.7')
      tmp <- paste(n, v, sep = "_")
      tmp1 <- paste(tmp, collapse = "__")
      
      # string combining rep number and target number
      strShort <- paste0("Rep", r, "_Target", t)
      # longer string including target attribute name and values from above
      strLong <- paste0(strShort, "__", tmp1)
      
      targetRepInfo <- list(
        repNum = r,
        tarNum = t,
        tarAttNames = n,
        tarAttVals = v,
        strLong = strLong,
        strShort = strShort
      )
      
      # run the systemModel
      if ("targetRepInfo" %in% methods::formalArgs(systemModel)) {
        perfTemp <- systemModel(data = scenarioData, systemArgs = systemArgs, metrics = metrics, targetRepInfo = targetRepInfo)
      } else {
        perfTemp <- systemModel(data = scenarioData, systemArgs = systemArgs, metrics = metrics)
      }
      
      # store performance metrics
      for (i in 1:length(metrics)) {
        performance[[i]][t, r] <- perfTemp[[i]]
      }
      rm(scenarioData)
    }
  }
  names(performance) <- metrics
  
  return(performance)
}

