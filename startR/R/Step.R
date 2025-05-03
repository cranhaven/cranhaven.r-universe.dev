#'Define the operation applied on declared data.
#'
#'The step of the startR workflow after declaring data by Start() call. It 
#'identifies the operation (i.e., function) and the target and output 
#'dimensions of data array for the function. Ideally, it expects the dimension
#'name to be in the same order as the one requested in the Start() call. 
#'If a different order is specified, startR will reorder the subset dimension 
#'to the expected order for this function.
#'
#'@param fun A function in R format defining the operation to be applied to the 
#'  data declared by a Start() call. It should only work on the essential 
#'  dimensions rather than all the data dimensions. Since the function will be
#'  called numerous times through all the non-essential dimensions, it is 
#'  recommended to keep them as light as possible.
#'@param target_dims A vector for single input array or a list of vectors for 
#'  multiple input arrays indicating the names of the dimensions 'fun' to be 
#'  applied along. 
#'@param output_dims A vector for single returned array or a list of vectors 
#'  for multiple returned arrays indicating the dimension names of the function
#'  output. 
#'@param use_libraries A vector of character string indicating the R library 
#'  names to be used in 'fun'. Only used when the jobs are run on HPCs; if the 
#'  jobs are run locally, load the necessary libraries by \code{library()}
#'  directly. The default value is NULL.
#'@param use_attributes One or more lists of vectors of character string 
#'  indicating the data attributes to be used in 'fun'. The list name should be
#'  consistent with the list name of 'data' in AddStep(). The default value is 
#'  NULL.
#'@return A closure that contains all the objects assigned. It serves as the
#'  input of Addstep().
#'@examples
#'  data_path <- system.file('extdata', package = 'startR')
#'  path_obs <- file.path(data_path, 'obs/monthly_mean/$var$/$var$_$sdate$.nc')
#'  sdates <- c('200011', '200012')
#'  data <- Start(dat = list(list(path = path_obs)),
#'                var = 'tos',
#'                sdate = sdates,
#'                time = 'all',
#'                latitude = 'all',
#'                longitude = 'all',
#'                return_vars = list(latitude = 'dat', 
#'                                   longitude = 'dat', 
#'                                   time = 'sdate'),
#'                retrieve = FALSE)
#'  fun <- function(x) {
#'            lat = attributes(x)$Variables$dat1$latitude
#'            weight = sqrt(cos(lat * pi / 180))
#'            corrected = Apply(list(x), target_dims = "latitude",
#'                              fun = function(x) {x * weight})
#'          }
#'  step <- Step(fun = fun,
#'               target_dims = 'latitude',
#'               output_dims = 'latitude',
#'               use_libraries = c('multiApply'),
#'               use_attributes = list(data = "Variables"))
#'  wf <- AddStep(data, step)
#'
#'@export
Step <- function(fun, target_dims, output_dims, 
                 use_libraries = NULL, use_attributes = NULL) {
  # Check fun
  if (!is.function(fun)) {
    stop("Parameter 'fun' must be a function.")
  }
  
  # Check target_dims
  if (is.character(target_dims)) {
    target_dims <- list(target_dims)
    names(target_dims) <- 'input1'
  }
  if (is.list(target_dims)) {
    sapply(target_dims, 
           function(x) {
             if (!(is.character(x) && (length(x) > 0))) {
               stop("Parameter 'target_dims' must be one or a list of vectors ",
                    "of target dimension names for each data array input in ",
                    "the function 'fun'.")
             }
           })
    if (is.null(names(target_dims))) {
      names(target_dims) <- paste0('input', 1:length(target_dims))
    }
  }
  
  # Check output_dims
  if (is.character(output_dims) || is.null(output_dims)) {
    output_dims <- list(output_dims)
    names(output_dims) <- 'output1'
  }
  if (is.list(output_dims)) {
    sapply(output_dims, 
           function(x) {
             if (!(is.character(x) || is.null(x))) {
               stop("Parameter 'output_dims' must be one or a list of vectors ",
                    "of target dimension names for each data array input in ",
                    "the function 'fun'.")
             }
           })
    if (is.null(names(output_dims))) {
      names(output_dims) <- paste0('output', 1:length(output_dims))
    }
  }
  
  # Check use_libraries
  if (!is.null(use_libraries)) {
    if (!is.character(use_libraries)) {
      stop("Parameter 'use_libraries' must be a vector of character ",
           "strings.")
    }
  }
  
  # Check use_attributes
  if (!is.null(use_attributes)) {
    raise_error <- FALSE
    if (!is.list(use_attributes)) {
      raise_error <- TRUE
    }
    if (!all(sapply(use_attributes, 
                    function(x) {
                      is.character(x) ||
                        (is.list(x) && all(sapply(x, is.character)))
                    }))) {
      raise_error <- TRUE
    }
    if (raise_error) {
      stop("Parameter 'use_attributes' must be a list of vectors of ",
           "character strings or of lists of vectors of character ",
           "strings.")
    }
  }
  
  attr(fun, 'TargetDims') <- target_dims
  attr(fun, 'OutputDims') <- output_dims
  attr(fun, 'UseLibraries') <- use_libraries
  attr(fun, 'UseAttributes') <- use_attributes
  
  # TODO: Add provenance info
  class(fun) <- 'startR_step_fun'
  
  fun
}
