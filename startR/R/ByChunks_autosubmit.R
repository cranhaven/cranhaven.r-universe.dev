#'Execute the operation by chunks
#'
#'This is an internal function used in Compute(), executing the operation by 
#'the chunks specified in Compute(). It also returns the configuration details
#'and profiling information. It is used when the workflow manager is 
#'Autosubmit.
#'
#'@param step_fun A function with the class 'startR_step_fun' containing the 
#'  details of operation.
#'@param cube_headers A list with the class 'startR_cube' returned by Start().
#'  It contains the details of data to be operated.
#'@param \dots Additional parameters for the inputs of 'step_fun'.
#'@param chunks A named list of dimensions which to split the data along and 
#'  the number of chunks to make for each. The chunked dimension can only be 
#'  those not required as the target dimension in function Step(). The default
#'  value is 'auto', which lists all the non-target dimensions and each one has
#'  one chunk.
#'@param threads_load An integer indicating the number of execution threads to 
#'  use for the data retrieval stage. The default value is 1.
#'@param threads_compute An integer indicating the number of execution threads
#'  to use for the computation. The default value is 1.
#'@param cluster A list of components that define the configuration of the 
#'  machine to be run on. The comoponents vary from different machines. Check
#'  \href{https://earth.bsc.es/gitlab/es/startR/-/blob/master/inst/doc/practical_guide.md}{practical guide} 
#'  for more details and examples. 
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
#'  specified workflow. The configuration details and profiling information are
#'  attached as attributes to the returned list of arrays.
#'
#'@examples
#' # ByChunks_autosubmit() is internally used in Compute(), not intended to be 
#' # used by users. The example just illustrates the inputs of 
#' # ByChunks_autosubmit().
#' # data_path <- system.file('extdata', package = 'startR')
#' # path_obs <- file.path(data_path, 'obs/monthly_mean/$var$/$var$_$sdate$.nc')
#' # sdates <- c('200011', '200012')
#' # data <- Start(dat = list(list(path = path_obs)),
#' #               var = 'tos',
#' #               sdate = sdates,
#' #               time = 'all',
#' #               latitude = 'all',
#' #               longitude = 'all',
#' #               return_vars = list(latitude = 'dat', 
#' #                                  longitude = 'dat', 
#' #                                  time = 'sdate'),
#' #               retrieve = FALSE)
#' # fun <- function(x) {
#' #           lat = attributes(x)$Variables$dat1$latitude
#' #           weight = sqrt(cos(lat * pi / 180))
#' #           corrected = Apply(list(x), target_dims = "latitude",
#' #                             fun = function(x) {x * weight})
#' #         }
#' # step <- Step(fun = fun,
#' #              target_dims = 'latitude',
#' #              output_dims = 'latitude',
#' #              use_libraries = c('multiApply'),
#' #              use_attributes = list(data = "Variables"))
#' #ByChunks_autosubmit(step, data)
#'
#'@import multiApply
#'@importFrom methods is
#'@noRd
ByChunks_autosubmit <- function(step_fun, cube_headers, ..., chunks = 'auto',
                                threads_load = 1, threads_compute = 1, 
                                cluster = NULL, 
                                autosubmit_suite_dir = NULL, autosubmit_server = NULL, 
                                silent = FALSE, debug = FALSE, wait = TRUE) {

  #NOTE:
  #autosubmit_suite_dir: /home/Earth/aho/startR_local_autosubmit/
  #autosubmit_suite_dir_suite: /home/Earth/aho/startR_local_autosubmit/STARTR_CHUNKING_a68h/
  #remote_autosubmit_suite_dir: /esarchive/autosubmit/a68h/proj/
  #remote_autosubmit_suite_dir_suite: /esarchive/autosubmit/a68h/proj/STARTR_CHUNKING_a68h/

  # Build object to store profiling timings
  t_begin_total <- Sys.time()
  t_begin_bychunks_setup <- t_begin_total
  timings <- list(nchunks = NULL,
                  concurrent_chunks = NULL,
                  cores_per_job = NULL,
                  threads_load = NULL,
                  threads_compute = NULL,
                  bychunks_setup = NULL,
                  transfer = NULL,
                  queue = NULL,
                  job_setup = NULL,
                  load = NULL,
                  compute = NULL,
                  transfer_back = NULL,
                  merge = NULL,
                  total = NULL)

  MergeArrays <- .MergeArrays

  # Sanity checks
  ## step_fun
  if (!is(step_fun, 'startR_step_fun')) {
    stop("Parameter 'step_fun' must be of the class 'startR_step_fun', as returned ",
         "by the function Step.")
  }

  ## cube_headers
  if (is(cube_headers, 'startR_cube')) {
    cube_headers <- list(cube_headers)
  }
  if (!all(sapply(lapply(cube_headers, class), 
                  function(x) 'startR_cube' %in% x))) {
    stop("All objects passed in 'cube_headers' must be of class 'startR_cube', ",
         "as returned by Start().")
  }
  if (length(cube_headers) != length(attr(step_fun, 'TargetDims'))) {
    stop("Number of inputs in parameter 'cube_headers' must be equal to the ",
         "number of inputs expected by the function 'step_fun'.")
  }
  
  ## threads_load and threads_compute
  if (!is.numeric(threads_load)) {
    stop("Parameter 'threads_load' must be a numeric value.")
  }
  threads_load <- round(threads_load)
  if (!is.numeric(threads_compute)) {
    stop("Parameter 'threads_compute' must be a numeric value.")
  }
  threads_compute <- round(threads_compute)
  timings[['threads_load']] <- threads_load
  timings[['threads_compute']] <- threads_compute
 
  ## autosubmit_suite_dir
  if (is.null(autosubmit_suite_dir)) {
    # Create a tmp folder as autosubmit_suite_dir
    autosubmit_suite_dir <- file.path(getwd(), "startR_autosubmit_temp")
    if (!dir.exists(autosubmit_suite_dir)) {
      dir.create("startR_autosubmit_temp", recursive = FALSE)
    }
    .warning(paste0("Parameter 'autosubmit_suite_dir' is not specified. Create a temporary ",
                    "folder under current directory: ", autosubmit_suite_dir, "/. Make sure ",
                    "that Autosubmit machine can find this path."))
  }
  if (!is.character(autosubmit_suite_dir)) {
   stop("Parameter 'autosubmit_suite_dir' must be a character string.")
  }

  ## autosubmit_server
  if (!is.null(autosubmit_server)) {
    if (!autosubmit_server %in% c('bscesautosubmit01', 'bscesautosubmit02')) {
      stop("Parameter 'autosubmit_server' must be one existing Autosubmit machine login node, 'bscesautosubmit01' or 'bscesautosubmit02'.")
    }
  } else {
    autosubmit_server <- paste0('bscesautosubmit0', sample(1:2, 1))
  }

  ## silent
  if (!is.logical(silent)) {
    stop("Parameter 'silent' must be logical.")
  }
  
  ## debug
  if (!is.logical(debug)) {
    stop("Parameter 'debug' must be logical.")
  }
  if (silent) {
    debug <- FALSE
  }

  ## wait
  if (!is.logical(wait)) {
    stop("Parameter 'wait' must be logical.")
  }

  ## cluster
  default_cluster <- list(queue_host = NULL,
#                          queue_type = 'slurm',
                          data_dir = NULL,
#                          temp_dir = NULL,
                          lib_dir = NULL,
                          init_commands = list(''),
                          r_module = 'R',
                          CDO_module = NULL,
                          autosubmit_module = 'autosubmit',
                          node_memory = NULL,  # not used
                          cores_per_job = NULL,
                          job_wallclock = '01:00:00',
                          max_jobs = 6,
                          extra_queue_params = list(''),
#                          bidirectional = TRUE,
                          polling_period = 10,
                          special_setup = 'none',
                          expid = NULL, 
                          hpc_user = NULL,
                          run_dir = NULL) 
  if (!is.list(cluster) || is.null(names(cluster))) {
    stop("Parameter 'cluster' must be a named list.")
  }
  if (any(!(names(cluster) %in% c('queue_host', 'queue_type', 'data_dir', 
                                  'temp_dir', 'lib_dir', 'init_commands', 
                                  'r_module', 'CDO_module', 'autosubmit_module', 
                                  'ecflow_module', 'node_memory', 
                                  'cores_per_job', 'job_wallclock', 'max_jobs', 
                                  'extra_queue_params', 'bidirectional',
                                  'polling_period', 'special_setup', 'expid', 'hpc_user',
                                  'run_dir'
                                  )))) {
    stop("Found invalid component names in parameter 'cluster'.")
  }
  # Remove ecFlow components
  redundant_components <- c('queue_type', 'temp_dir', 'ecflow_module', 'bidirectional')
  if (any(redundant_components %in% names(cluster))) {
    tmp <- redundant_components[which(redundant_components %in% names(cluster))]
    .warning(paste0("Cluster component ", paste(tmp, collapse = ','), 
                    " not used when Autosubmit is the workflow manager."))
    cluster[[tmp]] <- NULL
  }
  default_cluster[names(cluster)] <- cluster
  cluster <- default_cluster

  ### queue_host 
  support_hpcs <- c('local', 'nord3') # names in platforms.yml
  if (is.null(cluster$queue_host) || !cluster$queue_host %in% support_hpcs) {
    stop("Cluster component 'queue_host' must be one of the follows: ", 
         paste(support_hpcs, collapse = ','), '.')
  }

  ### data_dir
  is_data_dir_shared <- FALSE
  if (is.null(cluster[['data_dir']])) {
    is_data_dir_shared <- TRUE
  } else {
    if (!is.character(cluster[['data_dir']])) {
      stop("The component 'data_dir' of the parameter 'cluster' must be a character string.")
    }
    remote_data_dir <- cluster[['data_dir']]
  }
  ### lib_dir
  if (!is.null(cluster[['lib_dir']])) {
    if (!is.character(cluster[['lib_dir']])) {
      stop("The component 'lib_dir', of the parameter 'cluster' must be NULL or ",
           "a character string.")
    }
  }
  ### init_commands
  if (!is.list(cluster[['init_commands']]) || 
      !all(sapply(cluster[['init_commands']], is.character))) {
    stop("The component 'init_commands' of the parameter 'cluster' must be a list of ",
         "character strings.")
  }
  ### r_module
  if (!is.character(cluster[['r_module']])) {
    stop("The component 'r_module' of the parameter 'cluster' must be a character string.")
  }
  if ((nchar(cluster[['r_module']]) < 1) || (grepl(' ', cluster[['r_module']]))) {
    stop("The component 'r_module' of the parameter 'cluster' must have at least one character ",
         "and contain no blank spaces.")
  }
  ### CDO_module
  if (!is.null(cluster[['CDO_module']])) {
    if (!is.character(cluster[['CDO_module']])) {
      stop("The component 'CDO_module' of the parameter 'cluster' must be a character string.")
    }
    if (nchar(cluster[['CDO_module']]) < 1 || grepl(' ', cluster[['CDO_module']])) {
      warning("The component 'CDO_module' of parameter 'cluster' must have ",
              " than 1 and only the first element will be used.")
    }
    cluster[['r_module']] <- paste(cluster[['r_module']], cluster[['CDO_module']])
  }
  ### autosubmit_module
  if (!is.character(cluster[['autosubmit_module']])) {
    stop("The component 'autosubmit_module' of the parameter 'cluster' must be a character string.")
  }
  ### cores_per_job
  if (is.null(cluster[['cores_per_job']])) {
    cluster[['cores_per_job']] <- threads_compute
  }
  if (!is.numeric(cluster[['cores_per_job']])) {
    stop("The component 'cores_per_job' of the parameter 'cluster' must be numeric.")
  }
  cluster[['cores_per_job']] <- round(cluster[['cores_per_job']])
#  NOTE: Why do we have this condition?
#  if (cluster[['cores_per_job']] > threads_compute) {
#    .message("WARNING: 'threads_compute' should be >= cluster[['cores_per_job']].")
#  }
  ### job_wallclock
  tmp <- strsplit( '01:00:00', ':')[[1]]
  if (!length(tmp) %in% c(2, 3) | any(!grepl("^[0-9]+$", tmp)) | any(nchar(tmp) != 2)) {
    stop("The compoment 'job_wallclock' should be the format of HH:MM or HH:MM:SS.")
  }
  ### max_jobs
  if (!is.numeric(cluster[['max_jobs']])) {
    stop("The component 'max_jobs' of the parameter 'cluster' must be numeric.")
  }
  cluster[['max_jobs']] <- round(cluster[['max_jobs']])
  ### extra_queue_params
  if (!is.list(cluster[['extra_queue_params']]) || 
      !all(sapply(cluster[['extra_queue_params']], is.character))) {
    stop("The component 'extra_queue_params' of the parameter 'cluster' must be a list of ",
         "character strings.")
  }
  ### polling_period
  if (!is.numeric(cluster[['polling_period']])) {
    stop("The component 'polling_period' of the parameter 'cluster' must be numeric.")
  }
  cluster[['polling_period']] <- round(cluster[['polling_period']])
  ### special_setup
  if (!(cluster[['special_setup']] %in% c('none', 'marenostrum4'))) {
    stop("The value provided for the component 'special_setup' of the parameter ",
         "'cluster' is not recognized.")
  }
  ### expid
  as_module <- cluster[['autosubmit_module']]
  if (is.null(cluster[['expid']])) {
    text <- system(
      paste0("module load ", as_module, "; ",
             "autosubmit expid -H local -d 'startR computation'"),
    intern = T)
    cluster[['expid']] <- strsplit(
                            text[grep("The new experiment", text)],
                            "\"")[[1]][2]
    message(paste0("ATTENTION: The new experiment '", cluster[['expid']], 
                   "' is created. Please note it down."))
  } else {
    if (!is.character(cluster[['expid']]) | length(cluster[['expid']]) != 1) {
      stop("The component 'expid' of the parameter 'cluster' must be a character string.")
    }
    if (!dir.exists(file.path("/esarchive/autosubmit", cluster[['expid']]))) {
      stop("Cluster component 'expid' is not found under /esarchive/autosubmit/.")
    }
  }
  suite_id <- cluster[['expid']]

  ### hpc_user
  if (!is.null(cluster$hpc_user) && (!is.character(cluster$hpc_user) | length(cluster$hpc_user) != 1)) {
    stop("Cluster component 'hpc_user' must be a character string.")
  }
  ### run_dir
  if (!is.null(cluster$run_dir)) {
    if (!dir.exists(cluster$run_dir)) {
      stop("Cluster component 'run_dir' ", cluster$run_dir," is not found.")
    }
  }

#==============================================

  autosubmit_suite_dir_suite <- paste0(autosubmit_suite_dir, '/STARTR_CHUNKING_', suite_id, '/')
  if (!dir.exists(autosubmit_suite_dir_suite)) {
    dir.create(autosubmit_suite_dir_suite, recursive = TRUE)
  }
  if (!dir.exists(autosubmit_suite_dir_suite)) {
    stop("Could not find or create the directory in  parameter 'autosubmit_suite_dir'.")
  }

  remote_autosubmit_suite_dir <- file.path("/esarchive/autosubmit/", suite_id, 'proj')
  remote_autosubmit_suite_dir_suite <- paste0(remote_autosubmit_suite_dir, '/STARTR_CHUNKING_', suite_id, '/')

  
  # Work out chunked dimensions and target dimensions
  all_dims <- lapply(cube_headers, attr, 'Dimensions')
  all_dims_merged <- NULL
  for (i in all_dims) {
    if (is.null(all_dims_merged)) {
      all_dims_merged <- i
    } else {
      all_dims_merged <- .MergeArrayDims(all_dims_merged, i)[[3]]
    }
  }
  all_dimnames <- names(all_dims_merged)
  
  target_dims_indices <- which(all_dimnames %in% unlist(attr(step_fun, 'TargetDims')))
  target_dims <- NULL
  if (length(target_dims_indices) > 0) {
    target_dims <- all_dimnames[target_dims_indices]
  }
  
  chunked_dims <- all_dimnames
  if (length(target_dims_indices) > 0) {
    chunked_dims <- chunked_dims[-target_dims_indices]
  }
  if (length(chunked_dims) < 1) {
    stop("Not possible to process input by chunks. All input dimensions are ",
         "target dimensions.")
  }
  
  # Check all input headers have matching dimensions
  cube_index <- 1
  for (cube_header in cube_headers) {

    # Check if all the margin dims are consistent among datasets
    if (!all(chunked_dims %in% names(attr(cube_header, "Dimensions")))) {
      trouble_dim_name <- chunked_dims[which(!chunked_dims %in%              
                                             names(attr(cube_header, "Dimensions")))]
      stop(paste0("Found margin dimension, ", toString(trouble_dim_name),
           ", is not in input data ", cube_index, "."))
    }

    # Only check margin dimensions (i.e., chunked_dims)
    if (!all(attr(cube_header, 'Dimensions')[chunked_dims] == all_dims_merged[names(attr(cube_header, 'Dimensions'))][chunked_dims])) {
      stop("All provided 'cube_headers' must have matching dimension lengths ",
           "with each other.")
    }
    if (!all(attr(step_fun, 'TargetDims')[[cube_index]] %in% names(attr(cube_header, 'Dimensions')))) {
      stop("All provided 'cube_headers' must contain at least the target dimensions ",
           "expected by 'step_fun'.")
    }
    cube_index <- cube_index + 1
    # work out expected result dimensions
  }
  
  # Check chunks
  default_chunks <- as.list(rep(1, length(chunked_dims)))
  names(default_chunks) <- chunked_dims
  if (length(chunks) == 1 && chunks == 'auto') {
    chunks <- default_chunks
  }
  if (!is.list(chunks)) {
    stop("Parameter 'chunks' must be a named list or 'auto'.")
  }
  if (is.null(names(chunks))) {
    stop("Parameter 'chunks' must be a named list or 'auto'.")
  }
  if (any(!(names(chunks) %in% chunked_dims))) {
    stop("All names in parameter 'chunks' must be one of the non-target dimensions ",
         "present in the cubes in 'cube_headers'. The target dimensions are ", 
         paste(paste0("'", target_dims, "'"), collapse = ', '), ". The non-target ",
         "dimensions (margins) are ", paste(paste0("'", chunked_dims, "'"), collapse = ', '), ".")
  }
  if (any(!(((unlist(chunks) %% 1) == 0) | (unlist(chunks) == 'all')))) {
    stop("All values in parameter 'chunks' must take a numeric value or 'all'.")
  }
  if (any(unlist(chunks) < 1)) {
    stop("All values in parameter 'chunks' must be >= 1.")
  }
  for (chunk_spec in 1:length(chunks)) {
    if (chunks[[chunk_spec]] > all_dims_merged[names(chunks)[chunk_spec]]) {
      stop("Too many chunks requested for the dimension ", names(chunks)[chunk_spec], 
           ". Maximum allowed is ", all_dims_merged[names(chunks)[chunk_spec]])
    }
  }
  default_chunks[names(chunks)] <- chunks
  #NOTE: chunks here has all the margin dims, not only the chunked ones
  chunks <- default_chunks
  timings[['nchunks']] <- prod(unlist(chunks))
  
  # Replace 'all's
  chunks_all <- which(unlist(chunks) == 'all')
  if (length(chunks_all) > 0) {
    chunks[chunks_all] <- all_dims[names(chunks)[chunks_all]] 
  }

  # Copy load_process_save_chunk_autosubmit.R into local folder
  chunk_script <- file(system.file('chunking/Autosubmit/load_process_save_chunk_autosubmit.R', 
                                   package = 'startR'))
  chunk_script_lines <- readLines(chunk_script)
  close(chunk_script)
  chunk_script_lines <- gsub('^lib_dir <- *', paste0('lib_dir <- ', 
                                                     paste(deparse(cluster[['lib_dir']]), collapse = '\n')), 
                             chunk_script_lines)
  #TODO: Change out_dir to somewhere else like expid/outputs/
  chunk_script_lines <- gsub('^out_dir <- *', paste0('out_dir <- ', 
                                                     paste(deparse(remote_autosubmit_suite_dir_suite), collapse = '\n')), chunk_script_lines)
  chunk_script_lines <- gsub('^debug <- *', paste0('debug <- ', paste(deparse(debug), collapse = '\n')), 
                             chunk_script_lines)
  deparsed_calls <- paste0('start_calls <- list(')
  extra_path <- ''
  if (cluster[['special_setup']] == 'marenostrum4') {
    extra_path <- '/gpfs/archive/bsc32/'
  }
  for (cube_header in 1:length(cube_headers)) {
    pattern_dim <- attr(cube_headers[[cube_header]], 'PatternDim')
    bk_pattern_dim <- cube_headers[[cube_header]][[pattern_dim]]
    bk_expected_files <- attr(cube_headers[[cube_header]], 'ExpectedFiles')
    if (!is_data_dir_shared) {
      cube_headers[[cube_header]][[pattern_dim]] <- paste0(remote_data_dir, '/', 
                                                           extra_path, '/', cube_headers[[cube_header]][[pattern_dim]])
      for (file_n in 1:length(bk_expected_files)) {
        attr(cube_headers[[cube_header]], 'ExpectedFiles')[file_n] <- paste0(remote_data_dir, '/', 
                                                                             extra_path, '/', attr(cube_headers[[cube_header]], 'ExpectedFiles')[file_n])
      }
    }
    deparsed_calls <- paste0(deparsed_calls, '\nquote(',
                             paste(deparse(cube_headers[[cube_header]]), collapse = '\n'), 
                             ')')
    cube_headers[[cube_header]][[pattern_dim]] <- bk_pattern_dim
    attr(cube_headers[[cube_header]], 'ExpectedFiles') <- bk_expected_files
    if (cube_header < length(cube_headers)) {
      deparsed_calls <- paste0(deparsed_calls, ', ')
    }
  }
  deparsed_calls <- paste0(deparsed_calls, '\n)')
  chunk_script_lines <- gsub('^start_calls <- *', deparsed_calls, chunk_script_lines)
  chunk_script_lines <- gsub('^start_calls_attrs <- *', 
                             paste0('start_calls_attrs <- ', 
                                    paste(deparse(lapply(cube_headers, attributes)), collapse = '\n')),
                             chunk_script_lines)
  chunk_script_lines <- gsub('^param_dimnames <- *', 
                             paste0('param_dimnames <- ', 
                                    paste(deparse(chunked_dims), collapse = '\n')), 
                             chunk_script_lines)
  chunk_script_lines <- gsub('^threads_load <- *', paste0('threads_load <- ', threads_load), 
                             chunk_script_lines)
  chunk_script_lines <- gsub('^threads_compute <- *', paste0('threads_compute <- ', threads_compute), 
                             chunk_script_lines)
  chunk_script_lines <- gsub('^fun <- *', paste0('fun <- ', paste(deparse(step_fun), collapse = '\n')), 
                             chunk_script_lines)
  chunk_script_lines <- gsub('^params <- *', paste0('params <- ', paste(deparse(list(...)), collapse = '\n')), 
                             chunk_script_lines)
  writeLines(chunk_script_lines, paste0(autosubmit_suite_dir_suite, '/load_process_save_chunk_autosubmit.R'))
   
  # Write and copy startR_autosubmit.sh into local folder
  write_autosubmit_bash(chunks, cluster, autosubmit_suite_dir = autosubmit_suite_dir)

  # Modify conf files from template and rewrite to /esarchive/autosubmit/expid/conf/
  write_autosubmit_confs(chunks, cluster, autosubmit_suite_dir)
  
  # Iterate through chunks
  chunk_array <- array(1:prod(unlist(chunks)), dim = (unlist(chunks)))
  arrays_of_results <- vector('list', length(attr(step_fun, 'OutputDims')))
  names(arrays_of_results) <- names(attr(step_fun, 'OutputDims'))
  for (component in 1:length(arrays_of_results)) {
    arrays_of_results[[component]] <- vector('list', prod((unlist(chunks))))
    dim(arrays_of_results[[component]]) <- (unlist(chunks))
  }
  found_first_result <- FALSE
  for (i in 1:length(chunk_array)) {
    chunk_indices <- which(chunk_array == i, arr.ind = TRUE)[1, ]
    names(chunk_indices) <- names(dim(chunk_array))
  }
  
  
  timings[['cores_per_job']] <- cluster[['cores_per_job']]
  timings[['concurrent_chunks']] <- cluster[['max_jobs']]
    
  t_end_bychunks_setup <- Sys.time()
  timings[['bychunks_setup']] <- as.numeric(difftime(t_end_bychunks_setup,
                                                     t_begin_bychunks_setup, units = 'secs'))
  if (!is_data_dir_shared) {
    #NOTE: Not consider this part yet
    t_begin_transfer <- Sys.time()
    .message("Sending involved files to the cluster file system...")
    files_to_send <- NULL
    #files_to_check <- NULL
    for (cube_header in 1:length(cube_headers)) {
      expected_files <- attr(cube_headers[[cube_header]], 'ExpectedFiles')
      #files_to_check <- c(files_to_check, expected_files)
      #if (cluster[['special_setup']] == 'marenostrum4') {
      #  expected_files <- paste0('/gpfs/archive/bsc32/', expected_files)
      #}
      files_to_send <- c(files_to_send, expected_files)
    }
    #which_files_exist <- sapply(files_to_check, file.exists)
    which_files_exist <- sapply(files_to_send, file.exists)
    files_to_send <- files_to_send[which_files_exist]
    if (cluster[['special_setup']] == 'marenostrum4') {
      file_spec <- paste(paste0("/gpfs/archive/bsc32/", 
                                files_to_send), collapse = ' ')
      system(paste0("ssh ", cluster[['queue_host']], " 'mkdir -p ", remote_data_dir, 
                    ' ; module load transfer ; cd ', remote_autosubmit_suite_dir_suite,
                    ' ; dtrsync -Rrav ', '\'', file_spec, '\' "', remote_data_dir, '/"',
                    " ; sleep 1 ; ",
                    "while [[ ! $(ls dtrsync_*.out 2>/dev/null | wc -l) -ge 1 ]] ; ",
                    "do sleep 2 ; done",
                    " ; sleep 1 ; ",
                    'while [[ ! $(grep "total size is" dtrsync_*.out | ',
                    "wc -l) -ge 1 ]] ; ",
                    "do sleep 5 ; done", "'"))
    } else {
      file_spec <- paste(files_to_send, collapse = ' :')
      system(paste0("ssh ", cluster[['queue_host']], ' "mkdir -p ', 
                    remote_data_dir, '"'))
      system(paste0("rsync -Rrav '", file_spec, "' '", 
                    cluster[['queue_host']], ":", remote_data_dir, "/'"))
    }
    .message("Files sent successfully.")
    t_end_transfer <- Sys.time()
    timings[['transfer']] <- as.numeric(difftime(t_end_transfer, t_begin_transfer, units = 'secs'))
  } else {
    timings[['transfer']] <- 0
  } 
  if (!silent) {
    .message(paste0("Processing chunks... "))
  }
  time_begin_first_chunk <- Sys.time()
  sys_commands <- paste0("module load ", as_module, "; ",
                         "autosubmit create ", suite_id, " -np; ",
                         "autosubmit refresh ", suite_id, "; ")
  if (wait) {
    sys_commands <- paste0(sys_commands, "autosubmit run ", suite_id)
  } else {
    sys_commands <- paste0(sys_commands, "nohup autosubmit run ", suite_id, " >/dev/null 2>&1 &") # disown?
  }
  if (gsub('[[:digit:]]', "", Sys.getenv('HOSTNAME')) == 'bscesautosubmit') {
    #NOTE: If we ssh to AS VM and run everything there, we don't need to ssh here
    system(sys_commands)

   } else {
#  } else if (gsub("[[:digit:]]", "", Sys.getenv("HOSTNAME")) == "bscearth") {
    # ssh from WS to AS VM to run exp
    as_login <- paste0(Sys.getenv("USER"), '@', autosubmit_server, '.bsc.es')
    sys_commands <- paste0('ssh ', as_login, ' "', sys_commands, '"') #'; exit"')
    system(sys_commands)

#  } else {
#      stop("Cannot identify host", Sys.getenv("HOSTNAME"), ". Where to run AS exp?")
  }

  # Check the size of tmp/ASLOGS/jobs_failed_status.log. If it is not 0, the jobs failed.    
  failed_file_size <- system(paste0("du /esarchive/autosubmit/", suite_id, "/tmp/ASLOGS/jobs_failed_status.log"), intern = T)
  if (substr(failed_file_size, 1, 1) != 0) {
    # Remove bigmemory objects (e.g., a68h_1_1 and a68h_1_1.desc) if they exist
    # If run_dir is specified, the files are under run_dir; if not, files are under proj/STARTR_CHUNKING_xxxx/
    if (!is.null(cluster[['run_dir']])) {
      file.remove(
        file.path(cluster[['run_dir']],
          list.files(cluster[['run_dir']])[grepl(paste0("^", suite_id, "_.*"), list.files(cluster[['run_dir']]))])
      )
    } else {
      file.remove(
        file.path(remote_autosubmit_suite_dir_suite,
          list.files(remote_autosubmit_suite_dir_suite)[grepl(paste0("^", suite_id, "_.*"), list.files(remote_autosubmit_suite_dir_suite))])
      )
    }

    stop("Some Autosubmit jobs failed. Check GUI and logs.")
  }

  timings[['total']] <- t_begin_total
  startr_exec <- list(cluster = cluster, workflow_manager = 'autosubmit',
                      suite_id = suite_id, chunks = chunks,
                      num_outputs = length(arrays_of_results),
                      autosubmit_suite_dir = autosubmit_suite_dir, #ecflow_server = ecflow_server, 
                      timings = timings)
  class(startr_exec) <- 'startR_exec'

  if (wait) {
    result <- Collect(startr_exec, wait = TRUE, remove = T)
   .message("Computation ended successfully.")
    return(result)

  } else {
    # if wait = F, return startr_exec and merge chunks in Collect().
    return(startr_exec)
  }

}
