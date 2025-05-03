#'Create the workflow with the previous defined operation and data.
#'
#'The step that combines the previous declared data and operation together to 
#'create the complete workflow. It is the final step before data processing.
#'
#'@param inputs One or a list of objects of the class 'startR_cube' returned by 
#'  Start(), indicating the data to be processed.
#'@param step_fun A startR step function as returned by Step().
#'@param \dots Additional parameters for the inputs of function defined in 
#'  'step_fun' by Step().
#'
#'@return A list of the class 'startR_workflow' containing all the objects 
#' needed for the data operation.
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
#'  pi_short <- 3.14
#'  fun <- function(x, pi_val) {
#'            lat = attributes(x)$Variables$dat1$latitude
#'            weight = sqrt(cos(lat * pi_val / 180))
#'            corrected = Apply(list(x), target_dims = "latitude",
#'                              fun = function(x) {x * weight})
#'          }
#'
#'
#'  step <- Step(fun = fun,
#'               target_dims = 'latitude',
#'               output_dims = 'latitude',
#'               use_libraries = c('multiApply'),
#'               use_attributes = list(data = "Variables"))
#'  wf <- AddStep(data, step, pi_val = pi_short)
#'
#'@importFrom methods is
#'@export
AddStep <- function(inputs, step_fun, ...) {
  # Check step_fun
  if (!is(step_fun, 'startR_step_fun')) {
    stop("Parameter 'step_fun' must be a startR step function as returned by Step.")
  }
  
  # Check inputs
  if (is(inputs, 'startR_cube') | is(inputs, 'startR_workflow')) {
    inputs <- list(inputs)
    names(inputs) <- 'input1'
  }
  else if (is.list(inputs)) {
    if (any(!sapply(inputs, 
                    function(x) is(x, 'startR_cube') | is(x, 'startR_workflow')))) {
      stop("Parameter 'inputs' must be one or a list of objects of the class ",
           "'startR_cube' or 'startR_workflow'.")
    }
  } else {
    stop("Parameter 'inputs' must be one or a list of objects of the class ",
         "'startR_cube' or 'startR_workflow'.")
  }
  
  # Consistency checks
  if (!is.null(attr(step_fun, "UseAttributes"))) {
    if (!all(names(inputs) == names(attr(step_fun, "UseAttributes")))) {
      names(inputs) <- names(attr(step_fun, "UseAttributes"))
      .warning(paste("The name of inputs is not assigned or differs from",
                     "name of use_attributes list in Step(). Force inputs",
                     "name to be consistent with use_attributes list"))
    }
  }
  
  if (length(inputs) != length(attr(step_fun, 'TargetDims'))) {
    stop("The number of provided 'inputs' (", length(inputs), ") does not ",
         "match the number of expected inputs by the provided 'step_fun' (", 
         length(attr(step_fun, 'TargetDims')), ").")
  }
  
  # Work out the total target dims of the step
  previous_target_dims <- NULL
  all_input_dims <- NULL
  for (input in 1:length(inputs)) {
    dims_to_compare <- names(attr(inputs[[input]], 'Dimensions'))
    if (!all(attr(step_fun, 'TargetDims')[[input]] %in% dims_to_compare)) {
      stop("The target dimensions required by 'step_fun' for the input ", input, 
           " are not present in the corresponding provided object in 'inputs'.")
    }
    if (is(inputs[[input]], 'startR_workflow')) {
      if (is.null(previous_target_dims)) {
        previous_target_dims <- attr(inputs[[input]], 'TargetDims')
      } else {
        dims1 <- rep(1, length(previous_target_dims))
        names(dims1) <- previous_target_dims
        dims2 <- rep(1, length(attr(inputs[[input]], 'TargetDims')))
        names(dims2) <- attr(inputs[[input]], 'TargetDims')
        previous_target_dims <- names(.MergeArrayDims(dims1, dims2)[[1]])
      }
    }
    new_input_dims <- attr(inputs[[input]], 'Dimensions')
    if (any(is.na(new_input_dims))) {
      new_input_dims[which(is.na(new_input_dims))] <- rep(1, length(which(is.na(new_input_dims))))
    }
    if (is.null(all_input_dims)) {
      all_input_dims <- new_input_dims
    } else {
      all_input_dims <- .MergeArrayDims(all_input_dims, new_input_dims)[[1]]
    }
  }
  
  new_target_dims <- unique(unlist(attr(step_fun, 'TargetDims')))
  result <- list()
  dims1 <- rep(1, length(previous_target_dims))
  names(dims1) <- previous_target_dims
  dims2 <- rep(1, length(new_target_dims))
  names(dims2) <- new_target_dims
  target_dims <- names(.MergeArrayDims(dims1, dims2)[[1]])
  for (output in 1:length(attr(step_fun, 'OutputDims'))) {
    workflow <- list(inputs = inputs,
                     fun = step_fun,
                     params = list(...))
    if (!is.null(attr(step_fun, 'OutputDims')[[output]])) {
      dimensions <- rep(NA, length(attr(step_fun, 'OutputDims')[[output]]))
      names(dimensions) <- attr(step_fun, 'OutputDims')[[output]]
    } else {
      dimensions <- NULL
    }
    in_dims_to_remove <- which(names(all_input_dims) %in% new_target_dims)
    if (length(in_dims_to_remove) > 0) {
      dimensions <- c(dimensions, all_input_dims[-in_dims_to_remove])
    } else {
      dimensions <- c(dimensions, all_input_dims)
    }
    attr(workflow, 'Dimensions') <- dimensions
    attr(workflow, 'AllTargetDims') <- target_dims
    class(workflow) <- 'startR_workflow'
    result[[names(attr(step_fun, 'OutputDims'))[output]]] <- workflow
  }
  
  if (length(result) == 1) {
    result[[1]]
  } else {
    result
  }
}
