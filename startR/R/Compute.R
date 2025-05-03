#'Specify the execution parameters and trigger the execution 
#'
#'The step of the startR workflow after the complete workflow is defined by 
#'AddStep(). This function specifies the execution parameters and triggers the
#'execution. The execution can be operated locally or on a remote machine. If 
#'it is the latter case, the configuration of the machine needs to be 
#'sepecified in the function, and the EC-Flow server is required to be 
#'installed.\cr\cr
#'The execution can be operated by chunks to avoid overloading the RAM memory.
#'After all the chunks are finished, Compute() will gather and merge them, and 
#'return a single data object, including one or multiple multidimensional data 
#'arrays and additional metadata.
#'
#'@param workflow A list of the class 'startR_workflow' returned by function 
#'  AddSteop() or of class 'startR_cube' returned by function Start(). It 
#'  contains all the objects needed for the execution.
#'@param chunks A named list of dimensions which to split the data along and 
#'  the number of chunks to make for each. The chunked dimension can only be 
#'  those not required as the target dimension in function Step(). The default
#'  value is 'auto', which lists all the non-target dimensions and each one has
#'  one chunk.
#'@param threads_load An integer indicating the number of parallel execution
#'  cores to use for the data retrieval stage. The default value is 1.
#'@param threads_compute An integer indicating the number of parallel execution
#'  cores to use for the computation. The default value is 1.
#'@param cluster A list of components that define the configuration of the 
#'  machine to be run on. The comoponents vary from the different machines.
#'  Check \href{https://earth.bsc.es/gitlab/es/startR/-/blob/master/inst/doc/practical_guide.md}{Practical guide on GitLab} for more 
#'  details and examples. Only needed when the computation is not run locally. 
#'  The default value is NULL.
#'@param workflow_manager Can be NULL, 'ecFlow' or 'Autosubmit'. The default is
#'  'ecFlow'.
#'@param ecflow_suite_dir A character string indicating the path to a folder in
#'  the local workstation where to store temporary files generated for the 
#'  automatic management of the workflow. Only needed when the execution is run
#'  remotely. The default value is NULL.
#'@param ecflow_server A named vector indicating the host and port of the 
#'  EC-Flow server. The vector form should be 
#'  \code{c(host = 'hostname', port = port_number)}. Only needed when the 
#'  execution is run remotely. The default value is NULL.
#'@param autosubmit_suite_dir A character string indicating the path to a folder
#'  where to store temporary files generated for the automatic management of the
#'  workflow manager. This path should be available in local workstation as well
#'  as autosubmit machine. The default value is NULL, and a temporary folder 
#'  under the current working folder will be created.
#'@param autosubmit_server A character vector indicating the login node of the 
#'  autosubmit machine. It can be "bscesautosubmit01" or "bscesautosubmit02". 
#'  The default value is NULL, and the node will be randomly chosen.
#'@param silent A logical value deciding whether to print the computation 
#'  progress (FALSE) on the R session or not (TRUE). It only works when the 
#'  execution runs locally or the parameter 'wait' is TRUE. The default value
#'  is FALSE.
#'@param debug A logical value deciding whether to return detailed messages on 
#'  the progress and operations in a Compute() call (TRUE) or not (FALSE). 
#'  Automatically changed to FALSE if parameter 'silent' is TRUE. The default 
#'  value is FALSE.
#'@param wait A logical value deciding whether the R session waits for the 
#'  Compute() call to finish (TRUE) or not (FALSE). If FALSE, it will return an
#'  object with all the information of the startR execution that can be stored
#'  in your disk. After that, the R session can be closed and the results can
#'  be collected later with the Collect() function. The default value is TRUE.
#'
#'@return A list of data arrays for the output returned by the last step in the
#'  specified workflow (wait = TRUE), or an object with information about the 
#'  startR execution (wait = FALSE). The configuration details and profiling 
#'  information are attached as attributes to the returned list of arrays.
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
#'  res <- Compute(wf, chunks = list(longitude = 4, sdate = 2))
#'
#'@importFrom methods is
#'@export
Compute <- function(workflow, chunks = 'auto', workflow_manager = 'ecFlow',
                    threads_load = 1, threads_compute = 1,
                    cluster = NULL, ecflow_suite_dir = NULL, ecflow_server = NULL,
                    autosubmit_suite_dir = NULL, autosubmit_server = NULL,
                    silent = FALSE, debug = FALSE, wait = TRUE) {
  # Check workflow
  if (!is(workflow, 'startR_cube') & !is(workflow, 'startR_workflow')) {
    stop("Parameter 'workflow' must be an object of class 'startR_cube' as ",
         "returned by Start or of class 'startR_workflow' as returned by ",
         "AddStep.")
  }
  
  if (is(workflow, 'startR_cube')) {
    #machine_free_ram <- 1000000000
    #max_ram_ratio <- 0.5
    #data_size <- prod(c(attr(workflow, 'Dimensions'), 8))
    #if (data_size > (machine_free_ram * max_ram_ratio)) {
    #  stop("It is not possible to fit the requested data (", data_size, 
    #       " bytes) into the maximum allowed free ram (", max_ram_ratio, 
    #       " x ", machine_free_ram, ").")
    #}
    eval(workflow)
  } else {
    # TODO:
    #explore tree of operations and identify set of operations that reduce dimensionality as much as possible
    #  while being able to fit in (cluster and to exploit number of available nodes) | (machine)
    #combine set of operations into a single function
    #Goal: to build manually a function following this pattern:
    #operation <- function(input1, input2) {
    #  fun1 <- workflow$fun
    #  fun1(input1, input2, names(workflow$params)[1] = workflow$params[[1]])
    #}
    op_text <- "function("
    op_text <- paste0(op_text,
                      paste(paste0('input', 1:length(workflow$inputs)), 
                            collapse = ', '))
    op_text <- paste0(op_text, ") {")
    op_text <- paste0(op_text, "\n  fun1 <- ", paste(deparse(workflow$fun), collapse = '\n'))
    op_text <- paste0(op_text, "\n  res <- fun1(", 
                      paste(paste0('input', 1:length(workflow$inputs)),
                            collapse = ", "))
    if (length(workflow$params) > 0) {
      for (j in 1:length(workflow$params)) {
        op_text <- paste0(op_text, ", ")
        op_text <- paste0(op_text, names(workflow$params)[j], " = ",
                          paste(deparse(workflow$params[[j]]), collapse = '\n'))
      }
    }
    op_text <- paste0(op_text, ")")
    op_text <- paste0(op_text, "\n}")
    operation <- eval(parse(text = op_text))
    operation <- Step(operation, 
                      attr(workflow$fun, 'TargetDims'), 
                      attr(workflow$fun, 'OutputDims'),
                      attr(workflow$fun, 'UseLibraries'),
                      attr(workflow$fun, 'UseAttributes'))
    
    if (!all(sapply(workflow$inputs, class) == 'startR_cube')) {
      stop("Workflows with only one step supported by now.")
    }

    # Run ByChunks with the chosen operation
    if (!is.null(cluster)) {
      if (is.null(workflow_manager)) {
        stop("Specify parameter 'workflow_manager' as 'ecFlow' or 'Autosubmit'.")
      } else if (!tolower(workflow_manager) %in% c('ecflow', 'autosubmit')) {
        stop("Parameter 'workflow_manager' can only be 'ecFlow' or 'Autosubmit'.")
      }
    } else { # run locally
      workflow_manager <- 'ecflow'
    }

    if (tolower(workflow_manager) == 'ecflow') {
      # ecFlow or run locally
      res <- ByChunks_ecflow(step_fun = operation,
                             cube_headers = workflow$inputs,
                             chunks = chunks,
                             threads_load = threads_load,
                             threads_compute = threads_compute,
                             cluster = cluster,
                             ecflow_suite_dir = ecflow_suite_dir,
                             ecflow_server = ecflow_server,
                             silent = silent, debug = debug, wait = wait)
    } else {
      res <- ByChunks_autosubmit(step_fun = operation,
                      cube_headers = workflow$inputs,
                      chunks = chunks,
                      threads_load = threads_load,
                      threads_compute = threads_compute,
                      cluster = cluster,
                      autosubmit_suite_dir = autosubmit_suite_dir,
                      autosubmit_server = autosubmit_server,
                      silent = silent, debug = debug, wait = wait)

    } 

    # TODO: carry out remaining steps locally, using multiApply
    # Return results
    res
  }
}
