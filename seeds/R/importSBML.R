#'  Import SBML Models using the Bioconductor package 'rsbml'
#'
#'  A simple function for importing sbml models from a extensive markup language file.
#'  
#' @param  filename name of the import file. Should be located in the working directory.
#' @param  times    timestep at which the function should be evaluated
#' @param  meas_input  measurements have to be given in order to analyze the data
#'  
#' @return returns a odeModel object
#' 
#' @examples 
#' 
#' \dontrun{ 
#' 
#' t <- uvbData[,1]
#' y <- uvbData[,1:3]
#' modelFile <- system.file("extdata","BIOMD0000000545_url.xml", package = "seeds")
#' 
#' # generate an odeModel object
#' uvb <- importSBML(modelFile, times = t, meas = y)
#' 
#' }
#' 
#' @export importSBML
#'
importSBML <- function(filename, times, meas_input) {

  if (!base::require('rsbml', character.only = TRUE)) {
    message('Please install rsbml from the Bioconducture repository')
    stop('Bioconductor package rsbml not found.')
  } else {
    requireNamespace("rsbml")
    model <- rsbml::rsbml_read(filename = filename, dom = TRUE)

    if (missing(meas_input)) {
      warning("No measurements given. Returned model can't directly be used with the algorithms. Use method 'setMeas' for adding them to the model.")
    }
    
    states <- model@model@species
    parameter <- model@model@parameters

    # measurements
    rules <- model@model@rules
    reactions <- model@model@reactions
    reacList <- list()
    for (i in 1:length(reactions)) {
      reacList[[i]] <- gsub(pattern = "expression", replacement = '', deparse(model@model@reactions[[i]]@kineticLaw@math, width.cutoff = 300))
    }

    stoichM <- rsbml::stoichiometryMatrix(object = model@model)
    react <- c()
    combieReact <- function(reactStrs, stMatrix) {
      for (i in 1:nrow(stMatrix)) {
        m <- which(stMatrix[i,] != 0)
        if (length(m) > 0) {
          react <- c(react, paste0(stMatrix[i, m], '*', reactStrs[m], collapse = ' + '))
        }
      }
      return(react)
    }

    react <- combieReact(reacList, stoichM)
    meas <- c()
    if (length(rules) != 0) {
      for (i in 1:length(rules)) {
        meas[i] <- gsub(pattern = "expression", replacement = '', x = rules[[i]]@math)
      }

    }

    reformatEqs <- function(reactions, states, measureRules) {


      xStates <- paste('x', 1:length(states), sep = '')
      for (i in 1:length(states)) {
        regState <- paste0('\\b', states[i], '\\b')
        reactions = unlist(lapply(X = reactions, FUN = function(x) gsub(pattern = regState, replacement = xStates[i], x = x)))

        if (length(measureRules) != 0) {
          measureRules = unlist(lapply(X = measureRules, FUN = function(x) gsub(pattern = regState, replacement = xStates[i], x = x)))
        } else {
          measureRules = paste0('x', 1:length(reactions))
          warning('No measurement function found. Set it yourself. Model will use the identity of every state.')
        }
      }
      res = list('reac' = reactions, 'meas' = measureRules)
      return(res)
    }


    reactNames = rownames(stoichM[rowSums(abs(stoichM)) != 0,])
    eqList <- reformatEqs(reactions = react, states = reactNames, measureRules = meas)
    eqFuncList = writeDummy(eqList)

    # format the parameter and initial vector into names vectors
    if (length(parameter) != 0) {
      v <- c()
      n <- c()
      for (i in 1:length(parameter)) {
        v[i] <- parameter[[i]]@value
        n[i] <- parameter[[i]]@name
      }
      namedParaVec <- v
      names(namedParaVec) <- tolower(n)
    }

    initToPara <- function(model, namedParaV) {
      const <- c()
      for (i in 1:length(model@model@species)) {
        const[i] = model@model@species[[i]]@constant
      }
      constSpec <- model@model@species[[which(const, TRUE)]]

      nV <- c()
      namesV <- c()
      for (i in 1:length(constSpec)) {
        nV[i] = constSpec@initialAmount
        namesV[i] = constSpec@id
      }
      names(nV) <- tolower(namesV)

      namedParaV = c(namedParaV, nV)
      return(namedParaV)
    }

    if (length(parameter) != 0) {
      namedParaVec = initToPara(model, namedParaVec)
    } else {
      reactionsList <- model@model@reactions
      reaction_anno <- c()
      for (i in 1:length(reactionsList)) {
        reaction <- reactionsList[[i]]

        parametersList <- reaction@kineticLaw@parameters

        values_vec <- c()
        names_vec <- c()
        for (j in 1:length(parametersList)) {
          values_vec[j] = parametersList[[j]]@value
          names_vec[j] = parametersList[[j]]@id

        }
        names(values_vec) <- names_vec
        reaction_anno <- append(reaction_anno, values_vec)

      }
      parameters_vec <- reaction_anno[order(names(reaction_anno))]
      namedParaVec <- parameters_vec[!duplicated(names(parameters_vec))]
      
      # add constant values
      constants <- model@model@species
      const_idx <- which(rowSums(abs(stoichM)) == 0)
      vec <- c()
      names <- c()
      for (i in 1:length(const_idx)) {
        names[i] = tolower(constants[[const_idx[i]]]@id)
        vec[i] = constants[[const_idx[i]]]@initialAmount
      }
      names(vec) <- names
      namedParaVec <- append(namedParaVec,vec)
      
    }


    initVec <- model@model@species
    if (length(initVec) != 0) {

      v <- c()
      n <- c()
      for (i in 1:length(initVec)) {
        v[i] <- initVec[[i]]@initialAmount
      }
      initState <- v
      initState = initState[rowSums(abs(stoichM)) != 0]
    }
    
    
    # print(eqFuncList)
    
    if (missing(meas_input)) {
      model <- odeModel(func = eqFuncList$reac, parms = namedParaVec, measFunc = eqFuncList$meas, y = initState, times = times)
    } else {
      model <- odeModel(func = eqFuncList$reac, parms = namedParaVec, measFunc = eqFuncList$meas, y = initState, times = times, meas = meas_input)
    }
  }
  unloadNamespace('rsbml')

  return(model)
}


