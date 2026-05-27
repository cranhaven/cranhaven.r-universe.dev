#-------------------------------------------------------------------------
# Wrapper function that loops over generateScenario


#' Produces time series of hydroclimatic variables for an exposure space.
#'
#' \code{generateScenarios} produces time series of hydroclimatic variables using requested climate attributes that correspond to a target exposure space using a reference daily time series as an input.
#' @param reference list; reference climate data, with vector \emph{times} in POSIXct format,
#' and climate variables \emph{*variable_name1*}, \emph{*variable_name2*}. Climate variables are specified as vectors for single-site data, and matrices for multi-site data (with columns for each site). \cr
#'            Please refer to data provided with the package that may be loaded using \code{data(tankDat)} and \code{data(barossaDat)} for examples of the expected format of single site and multi-site \code{reference}.
#' @param expSpace a list; created using the function \code{createExpSpace}
#' @param simLengthNyrs a number; a scalar that specifies the length in years of each generated scenario. This argument is used only with stochastic generation.
#' If \code{NULL} (the default), the generated simulation will be as long as \code{reference}.
#' @param numReplicates a number; a scalar that specific the number of stochastic replicates to be generated. The default is 1.
#' @param seedID a number; a scalar that specifies the seed to be used for the first replicate. Subsequent replicates will use seeds incremented by one.
#'               If \code{seedID} is \code{NULL} (which is the default), the function will use a random seed for stochastic time series generation.
#'               The seed used will be specified in the output. This argument is intended for use in cases that aim to reproduce an existing simulation.
#' @param controlFile a string; to specify the model/optimisation options used for simulating time series data. The valid values are:
#' \itemize{
#' \item \code{NULL}: the simulation uses the foreSIGHT default stochastic model settings.
#' \item \code{"scaling"}: the simulation uses scaling (simple/seasonal) instead of a stochastic model.
#'                           If all attributes in \emph{expSpace} are annual totals/averages, then simple scaling is used.
#'                           If seasonality ratio attributes are also included in \emph{expSpace}, then seasonal scaling is used.
#' \item \code{path to a JSON file}: the JSON file contains advanced options specify the stochastic model and optimisation inputs.
#'                   These options can be used to change stochastic model types, overwrite default model parameter bounds, change default optimisation arguments, and set penalty attributes to be used in optimisation.
#'                   Please refer to the function \code{writeControlFile} in order to create an \code{controlFile} JSON file.
#' }
#' @param targetTol a number; Acceptable tolerance between target and simulated attributes. Error larger than \code{targetTol} for a single replicate/target produces a warning.
#' @param cores a number; Number of core sued for parallel processing.
#' @return The function returns a list containing the time series data generated. The list can contain multiple replicates (named as \code{Rep1}, \code{Rep2} etc.) equal to the \code{numReplicates} function argument.
#'         Each replicate can contain multiple targets (named as \code{Target1}, \code{Target2} etc.) based on the specified exposure space (\code{expSpace}). The \code{expSpace} and \code{controlFile} are also returned as part of this output list.
#' @seealso \code{createExpSpace}, \code{writeControlFile}, \code{viewModels}
#' @examples
#' # Example 1: Simple scaling
#' #-----------------------------------------------------------------------
#' attPerturb <- c("P_day_all_tot", "Temp_day_all_avg")
#' attPerturbType <- "regGrid"
#' attPerturbSamp <- c(2, 2)
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
#'
#' # Example 2: Seasonal scaling
#' #-----------------------------------------------------------------------
#' attPerturb <- c("P_day_all_tot", "P_day_all_seasRatio")
#' attPerturbType <- "regGrid"
#' attPerturbSamp <- c(2, 2)
#' attPerturbMin <- c(0.8, 0.9)
#' attPerturbMax <- c(1.1, 1.2)
#' expSpace <- createExpSpace(
#'   attPerturb = attPerturb,
#'   attPerturbSamp = attPerturbSamp,
#'   attPerturbMin = attPerturbMin,
#'   attPerturbMax = attPerturbMax,
#'   attPerturbType = attPerturbType
#' )
#' data(tankDat)
#' seasScaling <- generateScenarios(
#'   reference = tank_obs,
#'   expSpace = expSpace,
#'   controlFile = "scaling"
#' )
#'
#' # Example 3: Stochastic simulation using foreSIGHT default settings
#' #----------------------------------------------------------------------
#' \dontrun{
#' # create an exposure space
#' attPerturb <- c("P_day_all_tot_m", "P_day_all_nWet_m", "P_day_all_R10_m")
#' attHold <- c(
#'   "P_day_Feb_tot_m", "P_day_SON_dyWet_m", "P_day_JJA_avgWSD_m", "P_day_MAM_tot_m",
#'   "P_day_DJF_avgDSD_m", "Temp_day_all_rng_m", "Temp_day_all_avg_m"
#' )
#' attPerturbType <- "regGrid"
#' attPerturbSamp <- c(2, 1, 1)
#' attPerturbMin <- c(0.8, 1, 1)
#' attPerturbMax <- c(1.1, 1, 1)
#' expSpace <- createExpSpace(
#'   attPerturb = attPerturb,
#'   attPerturbSamp = attPerturbSamp,
#'   attPerturbMin = attPerturbMin,
#'   attPerturbMax = attPerturbMax,
#'   attPerturbType = attPerturbType,
#'   attHold = attHold,
#'   attTargetsFile = NULL
#' )
#' # load example data available in foreSIGHT
#' data(tankDat)
#' # perform stochastic simulation
#' simStochastic <- generateScenarios(
#'   reference = tank_obs,
#'   expSpace = expSpace,
#'   simLengthNyrs = 30
#' )
#' }
#' # Example 4: Simple Scaling with multi-site data
#' #-----------------------------------------------------------------------
#' attPerturb <- c("P_day_all_tot_m", "P_day_all_seasRatio")
#' attPerturbType <- "regGrid"
#' attPerturbSamp <- c(3, 3)
#' attPerturbMin <- c(0.8, 1.2)
#' attPerturbMax <- c(0.8, 1.2)
#' expSpace <- createExpSpace(
#'   attPerturb = attPerturb,
#'   attPerturbSamp = attPerturbSamp,
#'   attPerturbMin = attPerturbMin,
#'   attPerturbMax = attPerturbMax,
#'   attPerturbType = attPerturbType
#' )
#' # load multi-site rainfall data
#' data(barossaDat)
#' # perform simple scaling
#' simScaling <- generateScenarios(
#'   reference = barossa_obs,
#'   expSpace = expSpace,
#'   controlFile = "scaling"
#' )
#'
#' # Example 5: Multi-site stochastic simulation
#' #-----------------------------------------------------------------------
#' \dontrun{
#' attPerturb <- c("P_day_all_tot_m")
#' attHold <- c(
#'   "P_day_all_wettest6monSeasRatio", "P_day_all_wettest6monPeakDay",
#'   "P_day_all_P99", "P_day_all_avgWSD_m", "P_day_all_nWetT0.999_m"
#' )
#' attPerturbType <- "regGrid"
#' # consider unperturbed climates in this example
#' attPerturbSamp <- attPerturbMin <- attPerturbMax <- c(1)
#' expSpace <- createExpSpace(
#'   attPerturb = attPerturb,
#'   attPerturbSamp = attPerturbSamp,
#'   attPerturbMin = attPerturbMin,
#'   attPerturbMax = attPerturbMax,
#'   attPerturbType = attPerturbType,
#'   attHold = attHold
#' )
#' # load multi-site rainfall data
#' data(barossaDat)
#' # specify the penalty settings in a list
#' controlFileList <- list()
#' controlFileList[["penaltyAttributes"]] <- c(
#'   "P_day_all_tot_m",
#'   "P_day_all_wettest6monSeasRatio", "P_day_all_wettest6monPeakDay"
#' )
#' controlFileList[["penaltyWeights"]] <- c(0.5, 0.5, 0.5)
#' # specify the alternate model selections
#' controlFileList[["modelType"]] <- list()
#' controlFileList[["modelType"]][["P"]] <- "latent"
#' # specify model parameter selection
#' controlFileList[["modelParameterVariation"]] <- list()
#' controlFileList[["modelParameterVariation"]][["P"]] <- "har"
#' # specify settings for multi-site model
#' controlFileList[["spatialOptions"]] <- list()
#' # specify spatial correlation perturbation factor
#' controlFileList[["spatialOptions"]][["spatCorFac"]] <- 0.9
#' # write control file sttings to file
#' controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
#' write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))
#' # run multi-site stochastic simulation - this will take a long time (e.g. hours)
#' sim <- generateScenarios(
#'   reference = barossa_obs, expSpace = expSpace,
#'   controlFile = paste0(tempdir(), "controlFile.json"), seed = 1
#' )
#' }
#' 
#' # Example 6: Subdaily stochastic simulation
#' #-----------------------------------------------------------------------
#' \dontrun{
#' # specify attributes for range of time scales from hourly to daily
#' attPerturb = c('P_day_all_tot')
#' attHold = c('P_hour_all_sd','P_hour_all_cor','P_hour_all_nWet',
#'             'P_3hour_all_sd','P_3hour_all_cor','P_3hour_all_nWet',
#'             'P_12hour_all_sd','P_12hour_all_cor','P_12hour_all_nWet',
#'             'P_day_all_sd','P_day_all_cor','P_day_all_nWet')
#' # consider unperturbed climate           
#' attPerturbType = "regGrid"
#' attPerturbSamp = c(1)
#' attPerturbMin = c(1.)
#' attPerturbMax = c(1.)
#' 
#' # create the exposure space
#' expSpace = createExpSpace(attPerturb = attPerturb,
#'                           attPerturbSamp = attPerturbSamp,
#'                           attPerturbMin = attPerturbMin,
#'                           attPerturbMax = attPerturbMax,
#'                           attPerturbType = attPerturbType,
#'                           attHold = attHold)
#'  
#' # load synthetic subdaily data                          
#' data('subdailySyntheticDat')
#'  
#' controlFileList = list()                          
#' controlFileList$modelType = list()
#' controlFileList$modelType$P = "BLRPM"  # Bartlett-Lewis rectangular pulse model        
#' controlFileList$modelParameterVariation = list()
#' controlFileList$modelParameterVariation$P = "ann" # annual parameters (don't vary with season)
#' controlFileList = jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
#' controlFile = paste0(tempdir(), "\\eg_controlFile.json")
#' write(controlFileList, file = controlFile)
#' 
#' sim = generateScenarios(reference = subdaily_synthetic_obs,
#'                         expSpace = expSpace,
#'                         controlFile = controlFile,
#'                         seedID = 1)
#' # plot biases in target and simulated attributes                       
#' plotScenarios(sim)  
#' }                       
#' #-----------------------------------------------------------------------
#' @export
#' @importFrom foreach foreach %:% %dopar%
generateScenarios <- function(reference, # data frame of observed data with column names compulsary [$year, $month, $day, $P,] additional [$Temp, $RH, $PET, $uz, $Rs] (or a subset of these)
                              expSpace, # the space contains multiple targets
                              simLengthNyrs = NULL, # desired length of simulation in years
                              numReplicates = 1, # reps
                              seedID = NULL, # seed - user may set this to reproduce a previous simulation
                              cores = 1, # number of cores for parallel computing
                              controlFile = NULL, # NULL = default stochastic model options, "scaling" = simple scaling, json file = stochastic model options
                              targetTol = 0.05 # tolerance between targets and simulated attributes. prints warning when diff > targetTol
) {
  # Number of targets
  nTarget <- dim(expSpace$targetMat)[1]

  # Replicates and seed don't go with scaling
  if (!is.null(controlFile)) {
    if (controlFile == "scaling") {
      if (numReplicates > 1) stop("Simple scaling cannot generate replicates. Please set numReplicates to 1.")
      if (!is.null(seedID)) stop("Simple scaling cannot use a seed. Please set seedID to NULL.")
    }
  }

  # Create random seedID
  if (is.null(seedID)) {
    seedID <- round(stats::runif(1) * 10000)
  }

  # Create seedID vector for all replicates
  if (numReplicates > 0 & numReplicates %% 1 == 0) {
    seedIDs <- seedID + seq(0, numReplicates - 1)
    nRep <- length(seedIDs)
  } else {
    stop("numReplicates should be a positive integer")
  }

  # assign("optim_num",0,envir = foreSIGHT_optimizationSeedTrackerEnv)
  assign("IOlist", list(), envir = foreSIGHT_optimizationInputOutputEnv)

  allSim <- replicate(nRep, vector("list", nTarget), simplify = FALSE)

  if (cores == 1) {
    for (iRep in 1:nRep) {
      cat(paste0("Generating replicate number ", iRep, " out of ", nRep, " replicates...\n"))
      pb <- progress::progress_bar$new(
        # format = " [:bar] :elapsedfull",
        total = nTarget, clear = FALSE, width = 60
      )
      pb$tick(0)
      for (iTarg in 1:nTarget) {
        cat(paste0("Generating target number ", iTarg, " out of ", nTarget, " targets...\n"))
        iRepTarg <- (iRep - 1) * nTarget + iTarg
        # Get the target location in the exposure space
        expTarg <- expSpace
        expTarg$targetMat <- expSpace$targetMat[iTarg, , drop = F]
        if (!is.null(expSpace$attRot)) {
          expTarg$attRot <- expSpace$attRot[iTarg]
        }
        to.allSim <- generateScenario(
          reference = reference,
          expTarg = expTarg,
          simLengthNyrs = simLengthNyrs,
          seedID = seedIDs[iRep],
          controlFile = controlFile,
          iRepTarg = iRepTarg
        )
        allSim[[iRep]][[iTarg]] <- to.allSim
      }
    }
  } else if ((cores <- round(cores)) & (cores > 1)) {
    #    fname = paste0(tempfile())
    fname <- "log.txt"
    print(fname)
    c1 <- parallel::makeCluster(cores, outfile = fname)
    doParallel::registerDoParallel(c1)
    global_funs <- ls(envir = .GlobalEnv)
    global_funs <- global_funs[sapply(global_funs, function(f) is.function(get(f, envir = .GlobalEnv)))]
    # Export them to the cluster
    parallel::clusterExport(c1, varlist = global_funs, envir = .GlobalEnv)

    allSim <- foreach(iRep = 1:nRep, .packages = "foreSIGHT", .export = ls(globalenv())) %:%
      foreach(iTarg = 1:nTarget, .packages = "foreSIGHT", .export = ls(globalenv())) %dopar% {
        cat(paste0("Rep", iRep, " Targ", iTarg, "\n"))

        iRepTarg <- (iRep - 1) * nTarget + iTarg

        # Get the target location in the exposure space
        expTarg <- expSpace
        expTarg$targetMat <- expSpace$targetMat[iTarg, ]
        if (!is.null(expSpace$attRot)) {
          expTarg$attRot <- expSpace$attRot[iTarg]
        }

        to.allSim <- generateScenario(
          reference = reference,
          expTarg = expTarg,
          simLengthNyrs = simLengthNyrs,
          seedID = seedIDs[iRep],
          controlFile = controlFile,
          iRepTarg = iRepTarg
        )
      }
    parallel::stopCluster(c1)
  } else {
    stop("cores must be integer > 0")
  }

  names(allSim) <- paste0("Rep", 1:nRep)
  allSim[["simDates"]] <- allSim[[1]][[1]][["simDates"]]
  allSim[["expSpace"]] <- expSpace
  allSim[["controlFile"]] <- allSim[[1]][[1]][["nml"]]

  for (iRep in 1:nRep) {
    names(allSim[[iRep]]) <- paste0("Target", 1:nTarget)

    for (iTarg in 1:nTarget) {
      expTarg <- expSpace
      expTarg$targetMat <- expSpace$targetMat[iTarg, ]
      if (!is.null(expSpace$attRot)) {
        expTarg$attRot <- expSpace$attRot[iTarg]
      }

      # Get & remove simDates and nml from the target simulation, will be added back later
      allSim[[iRep]][[iTarg]][["nml"]] <- NULL
      allSim[[iRep]][[iTarg]][["simDates"]] <- NULL

      if (!is.null(allSim[[iRep]][[iTarg]]$attSim)) { # check if stochastic simulation performed

        varNames <- names(allSim[[iRep]][[iTarg]])
        varNames <- varNames[!varNames %in% c("attSim", "targetSim", "parS", "score")]

        for (var in varNames) {
          if (any(allSim[[iRep]][[iTarg]][[var]]$onBounds)) {
            warning(paste0("parameters for ", var, " stoch rep, ", iRep, " for target ", iTarg, " on bounds\n"))
          }

          targDiff <- abs(allSim[[iRep]][[iTarg]]$targetSim - expTarg$targetMat)
          if (any(targDiff > targetTol)) {
            # warning(paste0('error in target atts for ', var,' stoch rep ', iRep, ' for target ',iTarg, ' = ', targDiff, ' greater than tolerance of ', targetTol, '\n'))
            warning(paste0("error in target atts for ", var, " stoch rep ", iRep, " for target ", iTarg, " greater than tolerance of ", targetTol, "\n"))
          }
        }
      }
    }
  }
  cat("Simulation completed")

  if (!is.null(controlFile)) {
    if (controlFile == "scaling") allSim[["controlFile"]] <- controlFile
  }

  return(allSim)
}

checkObsVars <- function(obs, file, fSVars) {
  obsVars <- names(obs)[-which(names(obs) %in% c("times", "timeStep"))]
  for (o in obsVars) {
    if (!(o %in% fSVars)) {
      logfile(paste0("Input variable ", o, " unrecognized."), file)
      logfile("Program terminated", file)
      stop(paste0("Input variable ", o, " unrecognized."))
    }
  }
  return(invisible())
}

# Process namelist
# reads controlFile and converts to required inputs: modelTag, modelInfoMon, attPenalty, optimArgs
# Anjana: pass "file" to readNamelist and modify checkNamelist functions to write errors into the file
getUserModelChoices <- function(controlFile, obs, attSel, file = NULL) {
  obsVars <- names(obs)[!(names(obs) %in% c("times", "timeStep"))]
  attVars <- vapply(attSel, FUN = get.attribute.varType, FUN.VALUE = character(1), USE.NAMES = FALSE)

  fSVars <- get_fSVars(get_modelTags(modelInfoList = modelInfoList))

  if (!is.null(controlFile)) {
    if (controlFile != "scaling") {
      checkObsVars(obs, file, fSVars)
      vars <- list()
      for (i in 1:length(attSel)) {
        vars[[i]] <- unlist(strsplit(attVars[i], "/"))
        if (!all(vars[[i]] %in% obsVars)) {
          logfile(paste0("Observations do not contain the variable ", attVars[i], " to compute the selected attribute attSel [", i, "]"), file)
          logfile("Program terminated", file)
          stop(paste0("Observations do not contain the variable ", attVars[i], " to compute the selected attribute attSel [", i, "]"))
        }
      }
    }
  }

  # defaults
  modelInfoMod <- list()
  attPenalty <- NULL
  optimArgs <- list()
  spatialArgs <- list()
  postProcessing <- list()

  if (is.null(controlFile)) {
    modelTag <- NULL
    for (v in intersect(obsVars, attVars)) {
      modelTag <- c(modelTag, getModelTag(nml = NULL, v))
    }
  } else if (controlFile == "scaling") {
    is.seas <- grepl("seasRatio", attSel)
    len.seas <- length(which(is.seas))
    if (len.seas == 0) {
      modelTag <- c("Simple-ann")
    } else if (length(attSel) == 2 * len.seas) {
      modelTag <- c("Simple-seas")
    } else {
      modelTag <- c("Simple-ann", "Simple-seas")
    }
  } else {
    # Read in user namelist
    # Some observations may have a user choice - others may be default
    # It is assumed that if the input obs contains a variable it is to be simulated provided that a correspondiong attribute is selected

    # JSON file, else it must alread by an Rlist
    if (is.character(controlFile)) {
      nml <- readNamelist(controlFile)
    } else if (is.list(controlFile)) {
      nml <- controlFile
    }
    nmlVars <- names(nml[["modelType"]])
    allVars <- union(unique(unlist(vars)), nmlVars)

    spatialArgs <- ppArgs <- list()
    modelTag <- NULL
    for (v in allVars) {
      modelTag <- c(modelTag, getModelTag(nml = nml, v))
    }
    modelInfoMod <- getModelInfoMod(nml)
    optimArgs <- getOptimArgs(nml)
    attPenalty <- getAttPenalty(nml)
    spatialArgs <- nml$spatialArgs
    postProcessing <- nml$postProcessing
  }

  return(list(
    modelTag = modelTag,
    modelInfoMod = modelInfoMod,
    optimArgs = optimArgs,
    attPenalty = attPenalty,
    spatialArgs = spatialArgs,
    postProcessing = postProcessing
  ))
}

# add extra model info for simple/seasonal scaling (modelinfo[[mod]]$simvar), and perform some extra checks
# note: this could possibly be incorporated into get.multi.model.info or check_duplicates_mismatch but would significant changes to either function
add_scaling_info <- function(obs, attSel, modelInfo) {
  obsVars <- names(obs)[-which(names(obs) %in% c("times", "timeStep"))]

  attVars <- vapply(attSel, FUN = get.attribute.varType, FUN.VALUE = character(1), USE.NAMES = FALSE)

  for (v in intersect(obsVars, attVars)) {
    attSelVar <- attSel[attVars == v]
    tmp <- unlist(strsplit(attSelVar, paste0(v, "_")))
    suffix <- tmp[tmp != ""]
    if (any(endsWith(x = attSelVar, "all_tot")) |
      any(endsWith(x = attSelVar, "all_tot_m")) |
      any(endsWith(x = attSelVar, "all_avg")) |
      any(endsWith(x = attSelVar, "all_avg_m"))) {
      if (length(attSelVar) == 1) {
        modelInfo[["Simple-ann"]]$simVar <- c(modelInfo[["Simple-ann"]]$simVar, v)
      } else if ((length(attSelVar) == 2)) {
        if (any(grepl("seasRatio", attSelVar))) {
          modelInfo[["Simple-seas"]]$simVar <- c(modelInfo[["Simple-seas"]]$simVar, v)
        }
      } else {
        stop("Scaling only works with 2 attributes (total/avg and seasRatio) for ", v)
      }
    } else {
      stop("Require attribute ending with '_all_tot' or 'all_tot_m' or '_all_avg' or 'all_avg_m' for simple scaling")
    }
  }

  return(modelInfo)
}

generateScenario <- function(reference, # list observed data with column names compulsary [$times, $P,] additional [$Temp, $RH, $PET, $uz, $Rs] (or a subset of these)
                             expTarg,
                             simLengthNyrs = NULL,
                             seedID = 1,
                             controlFile = NULL,
                             iRepTarg = NULL) {
  # renamed obs to reference (rename everywhere sometime)
  obs <- reference

  # # Create random seedID
  # if (is.null(seedID)) {
  #   seedID <- round(stats::runif(1)*10000)
  # }
  set.seed(seedID)

  file <- paste0(tempdir(), "/generateScenario_log.txt")
  #  file <- paste0("./",iRepTarg, "_generateScenario_log.txt")

  # Checking
  #-------------------------------------------------------
  banner("CHECK FOR ARGUMENT INPUTS", file)
  progress("Checking argument inputs...", file)


  # Unpack expTarg
  #------------------------------------------------------
  attPerturb <- expTarg$attPerturb
  attHold <- expTarg$attHold
  attSel <- names(expTarg$targetMat)

  # Process Namelist
  #------------------------------------------------------
  userModelChoices <- getUserModelChoices(controlFile, obs, attSel, file)
  modelTag <- userModelChoices$modelTag
  modelInfoMod <- userModelChoices$modelInfoMod
  optimArgs <- userModelChoices$optimArgs
  attPrim <- userModelChoices$attPenalty
  ppArgs <- userModelChoices$postProcessing
  spatialArgs <- userModelChoices$spatialArgs

  # Update optimArgs
  #------------------------------------------------------
  optimArgs <- utils::modifyList(optimArgsdefault, optimArgs)
  optimArgs <- cleanOptimArgs(optimArgs)

  # Identify target
  #-------------------------------------------------------
  if (!is.null(dim(expTarg$targetMat))) {
    if (dim(expTarg$targetMat)[1] > 1) {
      progress("expTarg has multiple targets. 'generateScenario' works on a single target, use 'generateScenarios' instead", file)
      stop()
    }
  }
  targetMat <- expTarg$targetMat

  check_duplicates_mismatch(
    obs = obs,
    attSel = attSel,
    attPrim = attPrim,
    attHold = attHold,
    attPerturb = attPerturb,
    modelTag = modelTag,
    optimArgs = optimArgs,
    file = file
  )
  progress("Argument input format OK", file)

  banner("CHECK FOR MODEL AND ATTRIBUTE COMBINATIONS", file)
  progress("Checking model and attribute combinations...", file)

  check_models_attributes(
    names = names(obs),
    attSel = attSel,
    attPrim = attPrim,
    modelTag = modelTag,
    file = file
  )

  progress("Model and attribute combinations OK", file)

  # CHECK FOR INPUTS
  banner("CHECK FOR INPUT", file)
  progress("Checking input...", file)
  inputcheck <- input_process_check(obs, file, simLengthNyrs)
  obs <- inputcheck # USE NEW APPENDED/CHECKED DATA
  progress("Data input OK", file)

  # GET ADDITIONAL MODEL INFO, ATT INFO & SORT (make into separate script/functions)
  nMod <- length(modelTag)
  modelInfo <- get.multi.model.info(modelTag = modelTag)

  # add extra model info for simple/seasonal scaling and perform some extra checks
  if (modelTag[1] %in% c("Simple-ann", "Simple-seas")) {
    modelInfo <- add_scaling_info(obs = obs, attSel = attSel, modelInfo = modelInfo)
  }

  # UPDATE MODELINFO IF NEEDED
  for (mod in 1:nMod) {
    if (!is.null(modelInfoMod[[modelTag[mod]]])) {
      # modifyList
      if (mod == 1) progress("Updating model info...", file)
      defaultMods <- list(minBound = NULL, maxBound = NULL, fixedPars = NULL)
      modPars <- utils::modifyList(defaultMods, modelInfoMod[[modelTag[mod]]])
      modelInfo[[modelTag[mod]]] <- update_model_info(
        modelTag = modelTag[mod],
        modelInfo = modelInfo[[modelTag[mod]]],
        fixedPars = modPars$fixedPars,
        minUserBound = modPars$minBound,
        maxUserBound = modPars$maxBound,
        file = file
      ) # need to build in checks for this
    }

    # add post-processing info to model. include parameters.
    # this can be moved to separate function
    v <- modelInfo[[modelTag[mod]]]$simVar
    if (modelTag[mod] != "Simple-ann") {
      ppTypes <- ppArgs[[v]]$types
      if (!is.null(ppTypes)) {
        modelInfo[[modelTag[mod]]]$ppTypes <- ppTypes
        for (type in ppTypes) {
          if (!is.null(ppInfoList[[type]])) {
            modelInfo[[modelTag[mod]]]$npars <- modelInfo[[modelTag[mod]]]$npars + ppInfoList[[type]]$npars
            modelInfo[[modelTag[mod]]]$parNam <- c(modelInfo[[modelTag[mod]]]$parNam, ppInfoList[[type]]$parNam)
            modelInfo[[modelTag[mod]]]$minBound <- c(modelInfo[[modelTag[mod]]]$minBound, ppInfoList[[type]]$minBound)
            modelInfo[[modelTag[mod]]]$maxBound <- c(modelInfo[[modelTag[mod]]]$maxBound, ppInfoList[[type]]$maxBound)
          }
        }
      }
    }
  }

  modelTag <- update_simPriority(modelInfo = modelInfo)
  simVar <- sapply(X = modelInfo[modelTag], FUN = return.simVar, USE.NAMES = TRUE) # ?CREATE MODEL MASTER INFO - HIGHER LEVEL?

  attInfo <- attribute.info.check(attSel = attSel, attPrim = attPrim, lambda.mult = optimArgs$lambda.mult, targetType = expTarg$targetType)
  simAgg <- unique(attInfo$aggType)
  nagg <- length(simAgg)

  if (modelTag[1] %in% c("Simple-ann", "Simple-seas")) {
    simVar <- attInfo$varType
  }
  attInd <- get.att.ind(attInfo = attInfo, simVar = simVar)

  attInfo <- update_att_Info(attInfo = attInfo, attInd = attInd, modelTag = modelTag, simVar = simVar) # add extra level for easier model mangmt

  # GET DATES DATA (and indexes for harmonic periods)
  banner("INDEXING DATES", file)
  progress("Indexing dates...", file)
  dateExtnd <- dateExtender(obs = obs, simLengthNyrs = simLengthNyrs, file = file, modelTag = modelTag) # Extend dates if needed

  datInd <- list()
  datInd[["obs"]] <- setup_datInd_agg(simAgg = simAgg, times = obs$times, timeStep = obs$timeStep)

  for (i in 1:length(modelTag)) {
    if (!is.null(modelInfoList[[modelTag[i]]]$timeStep)) {
      datInd[[modelTag[i]]] <- setup_datInd_agg(simAgg = simAgg, times = dateExtnd, timeStep = modelInfoList[[modelTag[i]]]$timeStep, nperiod = modelInfo[[modelTag[i]]]$nperiod)
    }
  }

  progress("Dates indexed OK", file)

  #-------------------------------------
  # SIMPLE/SEASONAL SCALING
  if (modelTag[[1]] %in% c("Simple-ann", "Simple-seas")) {
    sim <- list()
    for (mod in 1:nMod) {
      if (modelTag[mod] == "Simple-ann") {
        # simple scaling
        banner("SIMPLE SCALING FOR OBSERVED DATA", file)
        progress("Simple scaling data...", file)
        i_simple_ann <- which(attInfo$varType %in% modelInfo[["Simple-ann"]]$simVar)
        varSel <- modelInfo[["Simple-ann"]]$simVar # [i_simple_ann]
        sim[varSel] <- simple.scaling(
          target = unlist(targetMat)[i_simple_ann],
          targetType = attInfo$targetType[i_simple_ann],
          data = obs,
          varType = attInfo$varType[i_simple_ann],
          period = 1, # modelInfo[[modelTag[mod]]]$nperiod,
          i.pp = datInd$obs[[aggNameShort[[obs$timeStep]]]]$i.pp
        )[varSel]
        progress("Simple scaling OK", file)
      } else if (modelTag[mod] == "Simple-seas") {
        # seasonal scaling
        banner("SEASONAL SCALING FOR OBSERVED DATA", file)
        progress("Seasonal scaling data...", file)
        for (v in modelInfo[["Simple-seas"]]$simVar) {
          timeStep <- names(datInd$obs)
          if (length(timeStep) > 1) {
            stop("require single timestep/aggregation period in attribute names for seasonal scaling")
          }
          i1 <- c()
          attNames <- names(targetMat)
          for (t in 1:length(attNames)) {
            if (any(endsWith(attNames[t], c("_all_tot", "_all_avg", "_all_tot_m", "_all_avg_m"))) &
              (startsWith(attNames[t], paste0(v, "_")))) {
              i1 <- c(i1, t)
            }
          }
          if (length(i1) != 1) {
            stop("require single attribute that ends with '_all_tot','_all_avg','_all_tot_m' or '_all_avg_m'")
          }
          i2 <- which(grepl(paste0(v, "_", timeStep, "_all_seasRatio"), names(targetMat))) # seasonality ratio attribute
          attSeas <- names(targetMat)[i2]
          o <- attribute.calculator.setup(attSeas, datInd$obs[[timeStep]])
          targetTypes <- attInfo$targetType[c(i1, i2)]
          if (any(targetTypes != "frac")) {
            stop("seas scaling cann only handle targetType=frac")
          }

          sim[[v]] <- seasonal.scaling(
            target_total_fac = unlist(targetMat)[i1],
            target_seas_fac = unlist(targetMat)[i2],
            data = obs[[v]],
            targetType = attInfo$targetType[i1],
            i.seas = o[[attSeas]]$attArgs$indexSeas
          )
        }
        progress("Seasonal scaling OK", file)
      }
    }
    sim[["simDates"]] <- dateExtnd
    progress("Data scaled OK", file)
  } else {
    # STOCHASTIC SIMULATION
    #---------------------------

    nsite <- dim(obs[[simVar[1]]])[2] ## need to fix this up for nMod > 1

    if (nsite == 1) {
      # GET ATTRIBUTES OF OBSERVED DATA (testing attribute calc function)
      banner("OBSERVED BASELINE ATTRIBUTE CALCULATION", file)
      progress("Calculating attributes...", file)

      attObs <- aggregate_calculate_attributes(
        data = obs,
        attSel = attSel,
        datInd = datInd$obs,
        attInfo = attInfo
      )

      progress(paste("Attributes of observed series - ", paste(attSel, ": ", signif(attObs, digits = 5), collapse = ", ", sep = ""), sep = ""), file)
      progress("Attributes calculated OK", file) # NEED SOME ACTUAL CHECKING HERE BEFORE PRONOUNCING OK

      # OPTIMISING TO DETERMINE PARS
      # LOOP OVER EXPOSURE SPACE POINTS TO DETERMINE PARS
      banner("DETERMINING STOCHASTIC MODEL PARAMETERS FOR TARGET", file)
      progress("Determining stochastic model parameters at target location...", file)
      progress("Simulating stochastic time series...", file)
      progress("Starting cluster...", file)

      # DETERMINE WHICH PARS ATTACH TO WHICH MODEL (make this a function in stochParManager.R)
      parLoc <- whichPars(modelInfo = modelInfo)

      # SCREEN INAPPROPRIATE SUGGESTIONS IF ANY
      if (!is.null(optimArgs$suggestions)) {
        optimArgs$suggestions <- screenSuggest(suggest = optimArgs$suggestions, modelInfo = modelInfo, modelTag = modelTag, parLoc = parLoc)
      }

      a <- Sys.time()

      # #IF "OAT" ROTATE attPrim
      # #attRot is returned only for "OAT" grids
      # if(!is.null(expTarg$attRot)) {
      #   attApp <- expTarg$attRot
      # } else {
      #   attApp <- attPrim
      # }

      sim <- simulateTarget(
        optimArgs = optimArgs, # sim[[i]]$P, $Temp $attSim $targetSim
        simVar = simVar,
        modelTag = modelTag,
        modelInfo = modelInfo,
        attSel = attSel,
        attPrim = attPrim, # controlled via switch
        attInfo = attInfo,
        attInd = attInd,
        datInd = datInd,
        obs = obs,
        initCalibPars = NULL,
        targetLoc = targetMat, # is  a vector  (just 1 target here)
        attObs = attObs,
        parLoc = parLoc,
        parSim = NULL,
        # Anjana - do I need this?
        setSeed = seedID, # seed based on loop counter
        iRepTarg = iRepTarg,
        file = file
      )

      b <- Sys.time()
      runt <- b - a
      logfile(signif(runt, digits = 3), file)
      nmlOut <- toNamelist(modelTag = modelTag, modelInfoMod = modelInfoMod, optimArgs = optimArgs, ppArgs = ppArgs, attPenalty = attPrim)
      sim[["nml"]] <- nmlOut
      sim[["simDates"]] <- dateExtnd
      progress("Stochastic model parameters and time series obtained at target location ", file)
    } else { # multi-site simulation

      # OPTIMISING TO DETERMINE PARS
      # LOOP OVER EXPOSURE SPACE POINTS TO DETERMINE PARS
      banner("DETERMINING STOCHASTIC MODEL PARAMETERS FOR TARGET", file)
      progress("Determining stochastic model parameters at target location...", file)
      progress("Simulating stochastic time series...", file)
      progress("Starting cluster...", file)

      # DETERMINE WHICH PARS ATTACH TO WHICH MODEL (make this a function in stochParManager.R)
      parLoc <- whichPars(modelInfo = modelInfo)

      # SCREEN INAPPROPRIATE SUGGESTIONS IF ANY
      if (!is.null(optimArgs$suggestions)) {
        optimArgs$suggestions <- screenSuggest(suggest = optimArgs$suggestions, modelInfo = modelInfo, modelTag = modelTag, parLoc = parLoc)
      }

      a <- Sys.time()

      # IF "OAT" ROTATE attPrim
      # attRot is returned only for "OAT" grids
      # if(!is.null(expTarg$attRot)) {
      #   attApp <- expTarg$attRot
      # } else {
      #   attApp <- attPrim
      # }

      # perform Stage 1 of multi-site simulation, where we calculate marginal parameters at each site, based on input spatial correlation matrix (spatialArgs$spatCorMatIn)
      sim1 <- simulateTargetMarg(
        optimArgs = optimArgs,
        simVar = simVar,
        modelTag = modelTag,
        modelInfo = modelInfo,
        attSel = attSel,
        attPrim = attPrim,
        attInfo = attInfo,
        attInd = attInd,
        datInd = datInd,
        initCalibPars = NULL,
        targetLoc = targetMat,
        parLoc = parLoc,
        parSim = NULL,
        setSeed = seedID,
        iRepTarg = iRepTarg,
        file = file,
        obs = obs,
        spatialArgs = spatialArgs
      )

      # perform Stage 2 of multi-site simulation, using marginal parameters from Stage 1 and this time calculating spatial correlation matrix
      sim2 <- simulateTargetCor(
        optimArgs = optimArgs,
        simIn = sim1,
        simVar = simVar,
        modelTag = modelTag,
        modelInfo = modelInfo,
        attSel = attSel,
        attPrim = attPrim,
        attInfo = attInfo,
        attInd = attInd,
        datInd = datInd,
        targetLoc = targetMat,
        parLoc = parLoc,
        setSeed = seedID,
        file = file,
        obs = obs,
        spatialArgs = spatialArgs
      )

      # perform Stage 3 of multi-site simulation, where we re-calculate marginal parameters at each site, based on spatial correlation matrix from Stage 2
      spatialArgs3 <- spatialArgs
      spatialArgs3$spatCorMatIn <- sim2$cor_par
      sim3 <- simulateTargetMarg(
        optimArgs = optimArgs,
        simVar = simVar,
        modelTag = modelTag,
        modelInfo = modelInfo,
        attSel = attSel,
        attPrim = attPrim,
        attInfo = attInfo,
        attInd = attInd,
        datInd = datInd,
        initCalibPars = NULL,
        targetLoc = targetMat,
        parLoc = parLoc,
        parSim = NULL,
        setSeed = seedID,
        iRepTarg = iRepTarg,
        file = file,
        obs = obs,
        spatialArgs = spatialArgs3
      )

      sim <- sim3

      sim$stages <- list()
      sim$stages$Stage1 <- sim1
      sim$stages$Stage2 <- sim2
      sim$stages$Stage3 <- sim3

      b <- Sys.time()
      runt <- b - a
      logfile(signif(runt, digits = 3), file)
      nmlOut <- toNamelist(modelTag = modelTag, modelInfoMod = modelInfoMod, optimArgs = optimArgs, ppArgs = ppArgs, attPenalty = attPrim)
      sim[["nml"]] <- nmlOut
      sim[["simDates"]] <- dateExtnd
      progress("Stochastic model parameters and time series obtained at target location ", file)
    }
  } # END STOCHASTIC SEGMENT

  return(sim)
}


