# Should we use a namelist file to specify the attributes and samples?
# Note: Both attPerturb and attHold have to specified in the target file

#' Creates exposure space of hydroclimatic targets for generation of scenarios 
#' using 'generateScenarios'
#'
#' \code{createExpSpace} returns a list containing the targets (\code{targetMat}) 
#' and the metadata (input arguments) used to create the exposure space.
#'
#' @param attPerturb A char vector; the names of the attributes to be perturbed.
#'                   This vector can contain attributes of different hydroclimatic 
#'                   variables.
#' @param attPerturbSamp An integer vector; the number of samples for each 
#'                       attribute \code{attPerturb}.
#'                       The length of this vector should be equal to the length 
#'                       of \code{attPerturb}.
#' @param attPerturbMin A numeric vector; the minimum bounds for sampling of 
#'                      \code{attPerturb}.
#'                      The length of this vector should be equal to the length 
#'                      of \code{attPerturb}.
#'                      For variables like precipitation, evapotranspiration, 
#'                      radiation, etc. \code{attPerturbMin} should specified 
#'                      as a fraction of the original
#'                      (eg: 0.9 = 90\% of the original attribute). 
#'                      For temperature, \code{attPerturbMin} should be 
#'                      specified in K (eg: 0.9 = 0.9 K).
#' @param attPerturbMax A numeric vector; the maximum bounds for sampling of 
#'                      \code{attPerturb}.
#'                      The length of this vector should be equal to the 
#'                      length of \code{attPerturb}.
#'                      For variables like precipitation, evapotranspiration, 
#'                      radiation, etc. \code{attPerturbMax} should specified 
#'                      as a fraction of the original
#'                      (eg: 0.9 = 90\% of the original attribute). For 
#'                      temperature, \code{attPerturbMax} should be specified 
#'                      in K (eg: 0.9 = 0.9 K).
#'                      Note that to create a single sample of the attribute, 
#'                      \code{attPerturbSamp} could be specified as 1 with 
#'                      \code{attPerturbMin} and \code{attPerturbMax} specified 
#'                      as equal.
#' @param attPerturbType A string to specify the type of sampling, defaults to 
#'                       regular spacing. Valid sampling types are:
#' \itemize{
#' \item "regGrid" a regular grid sampling all the attributes specified in 
#'        \code{attPerturb} simultaneously
#' \item "OAT" one-at-a-time sampling of the attributes specified in 
#'        \code{attPerturb}
#' }
#' @param attPerturbBy A numeric vector; increment of values to create samples 
#'                     between \code{attPerturbMin} and \code{attPerturbMax}.
#'                     If \code{attPerturbBy} is specified, attPerturbSamp 
#'                     should be set as \code{NULL}.
#' @param attHold A char vector; the names of the attributes to be held at 
#'                historical levels.
#'                This vector can contain attributes of different 
#'                hydroclimatic variables.
#' @param attTied A list; the attributes to be tied to other perturbed or 
#'                held attributes.
#'                First level of list is name of perturbed/held attributes.
#'                Second level of list is vector of attributes that are tied 
#'                (change with) to attribute in first level.
#' @param targetTypes A list; target type ('frac','diff') used to calculate 
#'                changes in attributes.
#'                First level of list is name of attribute.
#'                Second level of list is target type ('frac','diff').
#'                Specifying \code{targetTypes} overrides default target types 
#'                ('diff' for temperature attributes, 'frac' for other)
#' @param attTargetsFile String specifying the full path to a CSV file 
#'                      containing the target exposure space.
#'                       The column names in the file should correspond to the 
#'                       attributes specified in \code{attPerturb} and \code{attHold}.
#'                       \code{attTargetsFile} is alternate way to specify 
#'                       exposure space targets that do not form a regular grid.
#'                       If \code{attTargetsFile} is specified, the inputs 
#'                       arguments \code{attPerturbSamp}, \code{attPerturbMin}, 
#'                       \code{attPerturbMax},
#'                       and \code{attPerturbType} should be set to \code{NULL} 
#'                       and will not be used by the function.
#' @return The exposure space as a list containing the following fields:
#' \itemize{
#' \item \code{targetMat} a dataframe or matrix; each column is a perturb/hold 
#'                        attribute, each row is a point in the exposure space.
#' \item \code{attRot} a char vector containing the one-at-a-time ("OAT") 
#'                     attributes associated with \code{targetMat}, \code{attRot} 
#'                     is \code{NULL} for other types of sampling.
#' \item \code{attPerturb}, \code{attHold}, \code{attPerturbSamp}, 
#'                     \code{attPerturbMin}, \code{attPerturbMax}, 
#'                     \code{attPerturbType} in the  function input arguments, 
#'                     if not \code{NULL}.
#' }
#' @details See "Detailed Tutorial: Climate 'Stress-Testing' using *fore*SIGHT" 
#' vignette for specifying attribute names for \code{attPerturb} and \code{attHold}. 
#' The definition of the attribute can be viewed using the function \code{viewAttributeDef}.
#' @seealso \code{generateScenarios}, \code{viewAttributeDef}
#' @examples
#' # To view the definition of any valid attribute
#' viewAttributeDef("P_day_all_tot_m")
#'
#' # To create an exposure space of points on a regular grid
#' attPerturb <- c("P_day_all_tot_m", "P_day_all_nWet_m", "P_day_all_R10_m")
#' attPerturbType <- "regGrid"
#' attPerturbSamp <- c(3, 1, 1)
#' attPerturbMin <- c(0.9, 1, 1)
#' attPerturbMax <- c(1.1, 1, 1)
#' attHold <- c(
#'   "P_day_Feb_tot_m", "P_day_SON_dyWet_m", "P_day_JJA_avgWSD_m",
#'   "P_day_MAM_tot_m", "P_day_DJF_avgDSD_m", "Temp_day_all_rng_m", "Temp_day_all_avg_m"
#' )
#' expSpace <- createExpSpace(
#'   attPerturb = attPerturb, attPerturbSamp = attPerturbSamp,
#'   attPerturbMin = attPerturbMin, attPerturbMax = attPerturbMax,
#'   attPerturbType = attPerturbType, attHold = attHold, attTargetsFile = NULL
#' )
#'
#' # Using attPerturbBy to specify the increment of perturbation (attPerturbSamp set to NULL)
#'
#' attPerturb <- c("P_day_all_tot_m", "P_day_all_nWet_m", "P_day_all_R10_m")
#' attPerturbType <- "regGrid"
#' attPerturbMin <- c(0.9, 1, 1)
#' attPerturbMax <- c(1.1, 1, 1)
#' attPerturbBy <- c(0.1, 0, 0)
#' attHold <- c(
#'   "P_day_Feb_tot_m", "P_day_SON_dyWet_m", "P_day_JJA_avgWSD_m", "P_day_MAM_tot_m",
#'   "P_day_DJF_avgDSD_m", "Temp_day_all_rng_m", "Temp_day_all_avg_m"
#' )
#' expSpace <- createExpSpace(
#'   attPerturb = attPerturb, attPerturbSamp = NULL,
#'   attPerturbMin = attPerturbMin, attPerturbMax = attPerturbMax, attPerturbType = attPerturbType,
#'   attPerturbBy = attPerturbBy, attHold = attHold, attTargetsFile = NULL
#' )
#'
#' # To create an exposure space of observed attributes without perturbation
#' # Note that attPerturbMin and attPerturbMax values are set to 1 for variables like precipitation,
#' # and 0 for temperature
#' attPerturb <- c("P_day_all_tot_m", "P_day_all_nWet_m", "P_day_all_R10_m", "Temp_day_DJF_avg_m")
#' attPerturbType <- "regGrid"
#' attPerturbSamp <- c(1, 1, 1, 1)
#' attPerturbMin <- c(1, 1, 1, 0)
#' attPerturbMax <- c(1, 1, 1, 0)
#' expSpace <- createExpSpace(
#'   attPerturb = attPerturb, attPerturbSamp = attPerturbSamp,
#'   attPerturbMin = attPerturbMin, attPerturbMax = attPerturbMax, attPerturbType = attPerturbType,
#'   attHold = NULL, attTargetsFile = NULL
#' )
#' 
#' # Example showing tied attributes that tie changes in seasonal attributes 
#' # to changes in annual attributes 
#' attPerturb <- c("P_day_all_P99")
#' attHold <- c("P_day_all_tot_m","P_day_all_avgDSD_m", "P_day_all_nWet_m")
#' attPerturbType = "regGrid"
#' attPerturbSamp = c(5)
#' attPerturbMin = c(0.9)
#' attPerturbMax = c(1.3)
#' # use following function to create seasonally tied attribute list 
#' attTied = setSeasonalTiedAttributes(attSel=c(attPerturb,attHold)) 
#' # view attTied
#' attTied
#' expSpace <- createExpSpace(attPerturb = attPerturb, 
#'                            attPerturbSamp = attPerturbSamp, 
#'                            attPerturbMin = attPerturbMin, 
#'                            attPerturbMax = attPerturbMax,
#'                            attPerturbType = attPerturbType, 
#'                            attHold = attHold,
#'                            attTied = attTied)
#' @export

createExpSpace <- function(attPerturb,
                           attPerturbSamp = NULL,
                           attPerturbMin,
                           attPerturbMax,
                           attPerturbType = "regGrid",
                           attPerturbBy = NULL,
                           attHold = NULL,
                           attTied = NULL,
                           targetTypes = NULL,
                           attTargetsFile = NULL 
) {
  # print("CHECKING INPUT ARGUMENTS")

  if (!is.null(attPerturbBy)) {
    if (!is.null(attPerturbSamp)) {
      stop("Since attPerturbBy is specified, attPerturbSamp should be set to NULL")
    }
  } else {
    if (is.null(attTargetsFile)) {
      if (is.null(attPerturbSamp)) {
        stop("Either attPerturbSamp or attPerturbBy should be specified.")
      }
    }
  }

  check_attributes(
    attHold = attHold,
    attPerturb = attPerturb,
    attTargetsFile = attTargetsFile,
    attPerturbSamp = attPerturbSamp,
    attPerturbBy = attPerturbBy,
    attPerturbMin = attPerturbMin,
    attPerturbMax = attPerturbMax
  ) # ,
  #                   attribute.funcs = attribute.funcs)

  # convert "by" to "samp" since downstream code is set up to work with samp
  if (!is.null(attPerturbBy)) {
    attPerturbSamp <- floor((attPerturbMax - attPerturbMin) / attPerturbBy) + 1
    attPerturbSamp[attPerturbBy == 0] <- 1
    attPerturbMax <- attPerturbMin + (attPerturbSamp - 1) * attPerturbBy
  }

  # create exSpArgs
  if (!is.null(attTargetsFile)) {
    exSpArgs <- attTargetsFile
  } else {
    exSpArgs <- list()
    exSpArgs$samp <- attPerturbSamp
    exSpArgs$type <- attPerturbType
    exSpArgs$bounds <- createBounds(attPerturb, attPerturbMin, attPerturbMax)
    exSpArgs <- addExpArgs_attHold(attPerturb, attHold, exSpArgs)
  }

  attSel <- c(attPerturb, attHold)
  # create attInfo
  attInfo <- list()
  # attribute name chopper function
  attInfo$varType <- vapply(attSel, FUN = get.attribute.varType, 
                            FUN.VALUE = character(1), 
                            USE.NAMES = FALSE) # drop use of names as comes ordered anyway
  # ASSIGN TARGET TYPE (IF P USE "FRAC", IF T USE "DIFF")
  attInfo$targetType <- vapply(attInfo$varType, FUN = get.target.type, 
                               FUN.VALUE = character(1), USE.NAMES = FALSE)
  if (!is.null(targetTypes)) {
    for (att in names(targetTypes)) {
      i <- which(att == attSel)
      attInfo$targetType[i] <- targetTypes[[att]]
    }
  }

  # create temporary log file
  file <- paste0(tempdir(), "/generateExpSpace_log.txt")

  # print("CREATING EXPOSURE SPACE")
  # create the space
  spaceInfo <- expSpaceSampManager(exSpArgs = exSpArgs, attInfo = attInfo, 
                                   attSel = attSel, file = file)

  # save input arguments into the list
  spaceInfo$attPerturb <- attPerturb
  spaceInfo$attHold <- attHold
  spaceInfo$attTargetsFile <- attTargetsFile
  spaceInfo$attPerturbSamp <- attPerturbSamp
  spaceInfo$attPerturbMin <- attPerturbMin
  spaceInfo$attPerturbMax <- attPerturbMax
  spaceInfo$attPerturbType <- attPerturbType
  spaceInfo$attPerturbBy <- attPerturbBy
  spaceInfo$targetType <- attInfo$targetType

  # add tied attributes to exposure space
  if (!is.null(attTied)) {
    spaceInfo <- tieAttributes(spaceInfo, attTied)
  }

  return(spaceInfo)
}

createBounds <- function(attPerturb,
                         attPerturbMin,
                         attPerturbMax) {
  bounds <- list()
  for (i in 1:length(attPerturb)) {
    if (attPerturbMin[i] == attPerturbMax[i]) {
      bounds[[i]] <- attPerturbMin[i]
    } else {
      bounds[[i]] <- c(attPerturbMin[i], attPerturbMax[i])
    }
  }
  names(bounds) <- attPerturb
  return(bounds)
}

addExpArgs_attHold <- function(attPerturb = attPerturb, attHold = attHold, 
                               exSpArgs = exSpArgs) {
  # make attSel
  attSel <- c(attPerturb, attHold)
  attVars <- sapply(attSel, get.varType, USE.NAMES = FALSE)

  # Code for creating a default set of historical bounds
  # The bounds are set to '0' for attributes of Temperature (absolute values) & '1' for other variables
  n <- length(attVars)
  boundsdefault <- vector(length = n)
  for (i in 1:n) {
    if (attVars[i] == "Temp") {
      boundsdefault[i] <- 0
    } else {
      boundsdefault[i] <- 1
    }
  }
  tmp <- as.list(boundsdefault)
  names(tmp) <- attSel

  # this fills in samp/bounds
  # A default list of exSpArgs - specific to the particular case setup
  exSpArgsdefault <- list(
    type = "regGrid",
    samp = rep(1, n),
    bounds = tmp
  )

  # Combine user specified and defaults
  exSpArgs <- utils::modifyList(exSpArgsdefault, exSpArgs)
  exSpArgs$samp <- c(exSpArgs$samp, rep(1, length(attHold)))

  return(exSpArgs)
}

#######

#' Creates tied attributes which tie seasonal changes in attributes to annual changes
#'
#' \code{setSeasonalTiedAttributes} returns a list containing tied attributes,
#' matching seasonal attributes to non-stratified attributes.
#' This list can be used to specify tied attributes in \code{createExpSpace()}
#' @param attSel A char vector; the names of the attributes (perturbed/held) for 
#'               which seasonal attributes will be tied to.
#' @return A list describing tied attributes with first level corresponding to
#' original perturbed/held attributes, and second level a vector with tied seasonal attributes.
#' @examples
#' attSel <- c("P_day_all_tot_m", "P_day_all_P99")
#' attTied <- setSeasonalTiedAttributes(attSel)
#' attTied
#' @export
setSeasonalTiedAttributes <- function(attSel) {
  attsTied <- list()
  for (att in attSel) {
    attsTied[[att]] <- c()
    for (seas in c("DJF", "MAM", "JJA", "SON")) {
      att.seas <- gsub("all", seas, att)
      attsTied[[att]] <- c(attsTied[[att]], att.seas)
    }
  }
  return(attsTied)
}

##################################################
# generates list of tied attributes, for tying WetDay and DryDay attributes to 
# other attributes
setWDdayTiedAttributes <- function(attSel) {
  attsTied <- list()
  for (att in attSel) {
    var <- get.attribute.varType(att)
    if (grepl("/", var)) {
      stop("can't have multivariable tied attributes in perturb/hold atts")
    }
    var.new <- paste0(var, ".P")
    attsTied[[att]] <- c()
    for (cond in c("WetDay", "DryDay")) {
      att.cond <- gsub(var, var.new, att)
      att.cond <- paste0("mv.", att.cond, cond)
      attsTied[[att]] <- c(attsTied[[att]], att.cond)
    }
  }
  return(attsTied)
}

#################################################
# add tied attributes to expsire space
tieAttributes <- function(expSpace, # initial exposure space (created by createExpSpace)
                          attTied) # list of tied attributes
{
  for (att1 in names(attTied)) {
    if (!att1 %in% colnames(expSpace$targetMat)) {
      stop(paste0("must have tied attribute ", att1, " in targetMat attributes"))
    }

    for (att2 in attTied[[att1]]) {
      i <- which(colnames(expSpace$targetMat) == att1)
      if (att2 %in% expSpace$attTied) {
        if (expSpace$targetType[i] == "frac") {
          expSpace$targetMat[att2] <- expSpace$targetMat[att2] * expSpace$targetMat[att1]
        } else if (expSpace$targetType[i] == "diff") {
          expSpace$targetMat[att2] <- expSpace$targetMat[att2] + expSpace$targetMat[att1]
        }
      } else {
        expSpace$targetMat[att2] <- expSpace$targetMat[att1]
        expSpace$attTied <- c(expSpace$attTied, att2)
        expSpace$targetType <- c(expSpace$targetType, expSpace$targetType[i])
      }
    }
  }

  return(expSpace)
}

#######

#' #' @export
#' tieAttributes = function(expSpace,attTied,exclude.seas=NULL){
#'
#'   for (tieType in names(attTied)){
#'
#'     attSel = attTied[[tieType]]
#'
#'     if (length(attSel)==1){
#'       if (attSel=='allTargets'){
#'         attSel=colnames(expSpace$targetMat)
#'       }
#'     }
#'
#'     if (!all(attSel%in%colnames(expSpace$targetMat))){stop("must have tied attributes in targetMat attributes")}
#'
#'     if(tieType=='wDdD'){
#'       for (att in attSel){
#'         i=which(colnames(expSpace$targetMat)==att)
#'         var = get.attribute.varType(att)
#'         if(grepl('/',var)){stop("can't have multivariable tied attributes in perturb/hold atts")}
#'         var.new = paste0(var,'.P')
#'         for (cond in c('WetDay','DryDay')){
#'           att.cond = gsub(var,var.new,att)
#'           att.cond = paste0('mv.',att.cond,cond)
#'           if (att.cond%in%expSpace$attTied){
#'             if (expSpace$targetType[i]=='frac'){
#'               expSpace$targetMat[att.cond] = expSpace$targetMat[att.cond]*expSpace$targetMat[att]
#'             } else if (expSpace$targetType[i]=='diff'){
#'               expSpace$targetMat[att.cond] = expSpace$targetMat[att.cond]+expSpace$targetMat[att]
#'             }
#'             expSpace$targetMat[att.cond] = expSpace$targetMat[att.cond]*expSpace$targetMat[att]
#'           } else {
#'             expSpace$targetMat[att.cond] = expSpace$targetMat[att]
#'             expSpace$attTied = c(expSpace$attTied,att.cond)
#'             expSpace$targetType = c(expSpace$targetType,expSpace$targetType[i])
#'           }
#'         }
#'       }
#'     } else if(tieType=='seas'){
#'       for (att in attSel){
#'         i=which(colnames(expSpace$targetMat)==att)
#'         for (seas in c('DJF','MAM','JJA','SON')){
#'           att.seas = gsub('all',seas,att)
#'           if (!att.seas%in%exclude.seas){
#'             if (att.seas%in%expSpace$attTied){
#'               if (expSpace$targetType[i]=='frac'){
#'                 expSpace$targetMat[att.seas] = expSpace$targetMat[att.seas]*expSpace$targetMat[att]
#'               } else if (expSpace$targetType[i]=='diff'){
#'                 expSpace$targetMat[att.seas] = expSpace$targetMat[att.seas]+expSpace$targetMat[att]
#'               }
#'             } else {
#'               expSpace$targetMat[att.seas] = expSpace$targetMat[att]
#'               expSpace$attTied = c(expSpace$attTied,att.seas)
#'               expSpace$targetType = c(expSpace$targetType,expSpace$targetType[i])
#'             }
#'           }
#'         }
#'       }
#'     } else {
#'
#'       stop(paste0('cannot handle tie type',tieType))
#'
#'     }
#'
#'
#'
#'   }
#'
#'   return(expSpace)
#'
#' }
