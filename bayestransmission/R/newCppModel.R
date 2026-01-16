#' Create a new C++ model object with parameters
#'
#' Creates and initializes a C++ model object based on the provided parameters.
#' This function wraps the underlying C++ model classes (LogNormalModel, LinearAbxModel,
#' LinearAbxModel2, MixedModel) in appropriate R reference classes that expose
#' the model's methods and properties.
#'
#' The function uses the existing `newModel` C++ function to instantiate the model
#' and configure all parameters, then wraps it in the appropriate R reference class
#' based on the model type specified in `modelParameters$modname`.
#'
#' @param modelParameters List of model parameters created using functions from
#'   constructors.R, such as:
#'   * `LogNormalModelParams()` - Basic log-normal model
#'   * `LinearAbxModel()` - Linear antibiotic model
#'   * Or custom parameter lists containing:
#'     - `modname`: Model name ("LogNormalModel", "LinearAbxModel", "LinearAbxModel2", "MixedModel")
#'     - `nstates`: Number of states (2 or 3)
#'     - `nmetro`: Number of Metropolis-Hastings steps
#'     - `forward`: Forward simulation flag
#'     - `cheat`: Cheat flag for debugging
#'     - `Insitu`: In situ parameters from `InsituParams()`
#'     - `SurveillanceTest`: Surveillance test parameters from `SurveillanceTestParams()`
#'     - `ClinicalTest`: Clinical test parameters from `ClinicalTestParams()`
#'     - `OutCol`: Out-of-unit infection parameters from `OutOfUnitInfectionParams()`
#'     - `InCol`: In-unit parameters from `InUnitParams()` or `ABXInUnitParams()`
#'     - `Abx`: Antibiotic parameters from `AbxParams()`
#'     - `AbxRate`: Antibiotic rate parameters from `AbxRateParams()`
#'
#' @param verbose Logical flag to print progress messages during model creation
#'   and parameter setup (default: FALSE)
#'
#' @return A reference class object wrapping the C++ model. The specific class depends
#'   on `modelParameters$modname`:
#'   * `CppLogNormalModel` - For "LogNormalModel"
#'   * `CppLinearAbxModel` - For "LinearAbxModel"
#'   * `CppLinearAbxModel2` - For "LinearAbxModel2"
#'   * `CppMixedModel` - For "MixedModel" (if exposed in C++)
#'
#'   All returned objects inherit from `CppBasicModel` and provide access to:
#'   * **Properties:**
#'     - `InColParams` - In-unit colonization parameters
#'     - `OutColParams` - Out-of-unit colonization parameters
#'     - `InsituParams` - In situ parameters
#'     - `SurveillanceTestParams` - Surveillance test parameters
#'     - `ClinicalTestParams` - Clinical test parameters
#'     - `AbxParams` - Antibiotic parameters
#'   * **Methods:**
#'     - `logLikelihood(hist)` - Calculate log likelihood for a SystemHistory
#'     - `getHistoryLinkLogLikelihoods(hist)` - Get individual link log likelihoods
#'     - `forwardSimulate(...)` - Perform forward simulation
#'     - `initEpisodeHistory(...)` - Initialize episode history
#'     - `sampleEpisodes(...)` - Sample episodes
#'     - `setAbx(...)` - Set antibiotic parameters
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Create a linear antibiotic model (recommended - stable constructors)
#' params <- LinearAbxModel()
#' model <- newCppModel(params)
#'
#' # Access model properties
#' inColParams <- model$InColParams
#' insituParams <- model$InsituParams
#'
#' # Get parameter values
#' paramValues <- inColParams$values
#' 
#' # Get parameter names (if available)
#' paramNames <- inColParams$names
#' 
#' # Create a log-normal model
#' params <- LogNormalModelParams("LogNormalModel")
#' model <- newCppModel(params, verbose = TRUE)
#' }
#' 
#' @seealso
#' * [LogNormalModelParams()] for creating model parameters
#' * [LinearAbxModel()] for linear antibiotic model parameters
#' * [InsituParams()], [SurveillanceTestParams()], etc. for parameter components
#' * [newModelExport()] for extracting parameter values from a model
newCppModel <- function(modelParameters, verbose = FALSE) {
  # Validate input
  if (!is.list(modelParameters)) {
    stop("modelParameters must be a list")
  }
  
  # Check required fields
  required_fields <- c("modname", "nstates", "nmetro", "forward", "cheat",
                       "Insitu", "SurveillanceTest", "ClinicalTest", 
                       "OutCol", "InCol", "Abx", "AbxRate")
  missing_fields <- setdiff(required_fields, names(modelParameters))
  if (length(missing_fields) > 0) {
    stop("modelParameters is missing required fields: ", 
         paste(missing_fields, collapse = ", "))
  }
  
  # Extract model name
  modname <- modelParameters$modname
  
  # Validate model name
  valid_models <- c("LogNormalModel", "LinearAbxModel", "LinearAbxModel2", "MixedModel")
  if (!modname %in% valid_models) {
    stop("Invalid model name '", modname, "'. Must be one of: ",
         paste(valid_models, collapse = ", "))
  }
  
  # Create the model using the internal C++ function
  # This function instantiates the appropriate C++ class and sets up all parameters
  if (verbose) {
    message("Creating C++ model of type: ", modname)
  }
  
  # Call the C++ newModel function through the exported wrapper
  model_ptr <- newCppModelInternal(modelParameters, verbose)
  
  # Return the wrapped model
  # The C++ side has already wrapped it in the correct reference class
  return(model_ptr)
}


#' Extract Model Parameters from C++ Model Object
#'
#' Convenience function to extract all parameter values from a C++ model object
#' created with `newCppModel()`. This is essentially a wrapper around accessing
#' the model's parameter properties.
#'
#' @param model A C++ model object created with `newCppModel()`
#'
#' @return A named list containing all model parameter values:
#'   * `Insitu` - Named numeric vector of in situ parameter values
#'   * `SurveillanceTest` - Named numeric vector of surveillance test parameter values
#'   * `ClinicalTest` - Named numeric vector of clinical test parameter values
#'   * `OutCol` - Named numeric vector of out-of-unit colonization parameter values
#'   * `InCol` - Named numeric vector of in-unit colonization parameter values
#'   * `Abx` - Named numeric vector of antibiotic parameter values (if applicable)
#'
#'   If a component's names cannot be determined or lengths mismatch, the vector
#'   is returned unnamed.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Create a linear antibiotic model
#' params <- LinearAbxModel()
#' model <- newCppModel(params)
#' 
#' # Extract all parameters
#' all_params <- getCppModelParams(model)
#' 
#' # View specific parameter groups
#' all_params$InCol  # In-unit colonization parameters
#' all_params$Insitu # In situ parameters
#' }
getCppModelParams <- function(model) {
  if (!inherits(model, "C++Object")) {
    stop("model must be a C++ model object created with newCppModel()")
  }
  
  result <- list()
  
  # Try to access each parameter component
  # Use tryCatch in case some are NULL
  assign_named <- function(values, names_vec) {
    if (is.null(values)) return(values)
    if (!is.null(names_vec) && length(names_vec) == length(values)) {
      names(values) <- names_vec
    }
    values
  }

  # InsituParams
  tryCatch({
    vals <- model$InsituParams$values
    nms <- NULL
    if (!is.null(model$InsituParams$paramNames)) nms <- model$InsituParams$paramNames
    result$Insitu <- assign_named(vals, nms)
  }, error = function(e) { result$Insitu <<- NULL })

  # SurveillanceTestParams (Random/TestParams use 'names')
  tryCatch({
    vals <- model$SurveillanceTestParams$values
    nms <- NULL
    if (!is.null(model$SurveillanceTestParams$names)) nms <- model$SurveillanceTestParams$names
    result$SurveillanceTest <- assign_named(vals, nms)
  }, error = function(e) { result$SurveillanceTest <<- NULL })

  # ClinicalTestParams (RandomTestParams inherits TestParams)
  tryCatch({
    vals <- model$ClinicalTestParams$values
    nms <- NULL
    if (!is.null(model$ClinicalTestParams$names)) nms <- model$ClinicalTestParams$names
    result$ClinicalTest <- assign_named(vals, nms)
  }, error = function(e) { result$ClinicalTest <<- NULL })

  # OutColParams (has names property)
  tryCatch({
    vals <- model$OutColParams$values
    nms <- NULL
    if (!is.null(model$OutColParams$names)) nms <- model$OutColParams$names
    result$OutCol <- assign_named(vals, nms)
  }, error = function(e) { result$OutCol <<- NULL })

  # InColParams (LogNormalAbxICP / LogNormalICP provide names)
  tryCatch({
    vals <- model$InColParams$values
    nms <- NULL
    # Some implementations expose 'names'; others may expose 'paramNames'
    if (!is.null(model$InColParams$names)) nms <- model$InColParams$names else if (!is.null(model$InColParams$paramNames)) nms <- model$InColParams$paramNames
    result$InCol <- assign_named(vals, nms)
  }, error = function(e) { result$InCol <<- NULL })

  # AbxParams
  tryCatch({
    abx_params <- model$AbxParams
    if (!is.null(abx_params)) {
      vals <- abx_params$values
      nms <- NULL
      if (!is.null(abx_params$names)) nms <- abx_params$names
      result$Abx <- assign_named(vals, nms)
    } else {
      result$Abx <- NULL
    }
  }, error = function(e) { result$Abx <<- NULL })
  
  return(result)
}
