# Note: Have good defaults so that most users won't have to input a controlFile

#' Prints the default optimisation arguments
#'
#' \code{viewDefautOptimArgs()} prints the default values of optimisation arguments (\code{optimisationArguments}) used by \code{generateScenarios}
#' @param optimizer A string for the numerical optimizer. Default optimizer is 'RGN'.
#' @details This a helper function that prints the default values of the optimisation arguments.
#' The user may specify alternate values of these arguments in fields named according to the corresponding argument name nested under
#' \code{optimisationArguments} in a JSON file to use as the \code{controlFile} input to the \code{generateScenarios} function.
#' @seealso \code{writeControlFile}
#' @examples
#' # To view the default optimisation arguments
#' viewDefaultOptimArgs()
#' @export
viewDefaultOptimArgs <- function(optimizer = "RGN") {
  allArgs <- names(optimArgsdefault)
  optimArgs_toPrint <- list(
    optimizer = optimizer,
    obj.func = optimArgsdefault$obj.func,
    seed = optimArgsdefault$seed,
    nMultiStart = optimArgsdefault$nMultiStart,
    OFtol = optimArgsdefault$OFtol,
    seed = optimArgsdefault$seed
  )

  controlName <- allArgs[grepl(optimizer, allArgs)]
  optimArgs_toPrint[[controlName]] <- optimArgsdefault[[controlName]]
  optimArgs_toPrint[[controlName]]$fnscale <- NULL
  optimArgs_toPrint[[controlName]]$maximize <- NULL
  print(optimArgs_toPrint)
}

optimArgsdefault <- list(
  optimizer = "RGN",
  obj.func = "WSS",
  nMultiStart = 5,
  OFtol = 0.,
  seed = NULL,
  GA.args = list(
    pcrossover = 0.8, # list of a parameters used by the ga optimiser (if used)
    pmutation = 0.1,
    maxiter = 50,
    maxFitness = -0.001,
    popSize = 500,
    run = 20,
    parallel = FALSE,
    keepBest = TRUE
  ),
  RGN.control = list(iterMax = 100, distMin = 0), # 0.01),
  SCE.control = list(
    fnscale = -1,
    initsample = "random",
    ncomplex = 5
  ),
  # CMAES.control=list(fnscale=-1,
  #                    stopfitness=1e-5),
  NM.control = list(
    maximize = T,
    tol = 1e-6
  ),
  lambda.mult = NULL,
  suggestions = NULL,
  use_different_seeds = F
)

varShortToLong <- c(
  "P" = "Precipitation",
  "Temp" = "Temperature",
  "PET" = "Evapotranspiration",
  "Radn" = "Radiation"
)

varUnits <- c(
  "P" = "mm",
  "Temp" = "\u00B0C",
  "PET" = "mm",
  "Radn" = "MJ/m2"
)


# function to get a vector of varUnits given varNames
getVarUnits <- function(varNames) {
  varUnits <- NA
  for (v in 1:length(varNames)) {
    if (varNames[v] %in% c("Temp")) {
      varUnits[v] <- "\u00B0C" # expression(paste0(~degree, "C"))
    } else {
      varUnits[v] <- "fraction"
    }
  }
  return(varUnits)
}


defaultModelTags <- c(
  P = "P-seas-latent",
  Temp = "Temp-har-wgenO",
  PET = "PET-har-wgenO"
)

# existing foreSIGHT variables
# fSVars <- unique(sapply(strsplit(modelTaglist[!(modelTaglist%in%c("Simple-ann","Simple-seas"))], "-"), `[[`, 1))

get_fSVars <- function(modelTaglist) {
  fSVars <- unique(sapply(strsplit(modelTaglist[!(modelTaglist %in% c("Simple-ann", "Simple-seas"))], "-"), `[[`, 1))
}

get_modelTags <- function(modelInfoList) {
  names(modelInfoList)
}

#' Prints the list of built-in attribute functions
#'
#' \code{viewAttributeFuncs} prints the list of built-in attribute functions
#' @seealso \code{viewAttributeDef}, \code{createExpSpace}
#' @examples
#' # To view the list of built-in functions used to calculate attributes
#' viewAttributeFuncs()
#' @export
viewAttributeFuncs <- function() {
  print(attributeFuncs())
}

attributeFuncs <- function() {
  allFuncsForesight <- utils::lsf.str("package:foreSIGHT")
  allFuncsGlobal <- utils::lsf.str(globalenv())
  allFuncs <- c(allFuncsForesight, allFuncsGlobal)
  funcs <- allFuncs[which(startsWith(allFuncs, "func_"))]
  mvFuncs <- allFuncs[which(startsWith(allFuncs, "mvFunc_"))]
  msFuncs <- allFuncs[which(startsWith(allFuncs, "msFunc_"))]
  attFuncs <- mvAttFuncs <- msAttFuncs <- c()
  if (length(funcs) > 0) {
    for (a in 1:length(funcs)) {
      attFuncs[a] <- strsplit(funcs, "func_")[[a]][2]
    }
  }
  if (length(mvFuncs) > 0) {
    for (a in 1:length(mvFuncs)) {
      mvAttFuncs[a] <- strsplit(mvFuncs, "mvFunc_")[[a]][2]
    }
  }
  if (length(msFuncs) > 0) {
    for (a in 1:length(msFuncs)) {
      msAttFuncs[a] <- strsplit(msFuncs, "msFunc_")[[a]][2]
    }
  }

  return(list(
    single = attFuncs,
    multivariable = mvAttFuncs,
    multisite = msAttFuncs
  ))
}

#' Prints the definition of an attribute
#'
#' \code{viewAttributeDef} prints the short definition of a valid attribute
#' @param attribute A string; the name of the attribute.
#' @seealso \code{createExpSpace}
#' @examples
#' # To view the definition of any valid attribute
#' viewAttributeDef("P_day_all_tot_m")
#' @export
viewAttributeDef <- function(attribute) {
  print(tagBlender(attribute))
}

#' Prints the names and bounds of the parameters of the stochastic models
#'
#' \code{viewModelParameters} prints the names of the parameters of the stochastic model and its default minimum and maximum bounds.
#' The stochastic model is specified using the function arguments.
#' @param variable A string; the name of the variable. Type \code{viewModels()} to view valid variable names
#' @param modelType A string; the model type. Use \code{viewModels} to view the valid values.
#' @param modelParameterVariation A string; the parameter variation. Use \code{viewModels} to view the valid values.
#' @details The available stochastic models can be viewed using the function \code{viewModels()}.
#' This function prints the default ranges of the parameters of the stochastic model specified the
#' stochastic model of interest.
#' @seealso \code{viewModels}, \code{writeControlFile}
#' @examples
#' viewModelParameters("P", "wgen", "annual")
#' viewModelParameters("P", "wgen", "harmonic")
#' @export
viewModelParameters <- function(variable, modelType, modelParameterVariation) {
  nml <- list()
  nml[["modelType"]] <- list()
  nml[["modelParameterVariation"]] <- list()
  nml[["modelType"]][[variable]] <- modelType
  nml[["modelParameterVariation"]][[variable]] <- modelParameterVariation

  modelTag <- getModelTag(nml, variable)
  modelInfo <- get.model.info(modelTag)
  modelPars <- data.frame(
    parameter = modelInfo[["parNam"]],
    min_bound = modelInfo[["minBound"]],
    max_bound = modelInfo[["maxBound"]]
  )
  if (nrow(modelPars) < 1) print("Are the input arguments a valid combination of variable|modelType|modelParameterVariation?")
  # colnames(modelPars) <- c("parameter", "minimum bound", "maximum bound")
  print(modelPars)
}


get.model.info <- function(modelTag = NULL) {
  return(modelInfoList[[modelTag]])
}

modelInfoList <- list()


modelInfoList[["Simple-ann"]] <- list(
  simVar = c(),
  simPriority = 1
)

modelInfoList[["Simple-seas"]] <- list(
  simVar = c(),
  simPriority = 1
)


