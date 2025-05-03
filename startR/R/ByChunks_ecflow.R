#'Execute the operation by chunks
#'
#'This is an internal function used in Compute(), executing the operation by 
#'the chunks specified in Compute(). It also returns the configuration details
#'and profiling information.
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
#'  machine to be run on. The comoponents vary from the different machines. 
#'  Check 
#'  \href{https://earth.bsc.es/gitlab/es/startR/-/blob/master/inst/doc/practical_guide.md}{practical guide} 
#'  for more examples.
#'  Only needed when the computation is not run locally. The default value is 
#'  NULL.
#'@param ecflow_suite_dir A character string indicating the path to a folder in
#'  the local workstation where to store temporary files generated for the 
#'  automatic management of the workflow. Only needed when the execution is run
#'  remotely. The default value is NULL.
#'@param ecflow_server A named vector indicating the host and port of the 
#'  EC-Flow server. The vector form should be 
#'  \code{c(host = 'hostname', port = port_number)}. Only needed when the 
#'  execution is run remotely. The default value is NULL.
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
#' # ByChunks_ecflow() is internally used in Compute(), not intended to be used 
#' # by users. The example just illustrates the inputs of ByChunks_ecflow().
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
#' #ByChunks_ecflow(step, data)
#'
#'@import multiApply
#'@importFrom methods is
#'@noRd
ByChunks_ecflow <- function(step_fun, cube_headers, ..., chunks = 'auto',
                            threads_load = 1, threads_compute = 1, 
                            cluster = NULL, 
                            ecflow_suite_dir = NULL, 
                            ecflow_server = NULL, 
                            silent = FALSE, debug = FALSE,
                            wait = TRUE) {
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
  
  # Check input headers
  if (is(cube_headers, 'startR_cube')) {
    cube_headers <- list(cube_headers)
  }
  if (!all(sapply(lapply(cube_headers, class), 
                  function(x) 'startR_cube' %in% x))) {
    stop("All objects passed in 'cube_headers' must be of class 'startR_cube', ",
         "as returned by Start().")
  }
  
  # Check step_fun
  if (!is.function(step_fun)) {
    stop("Parameter 'step_fun' must be a function.")
  }
  
  # Check cores
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
  
  on_cluster <- !is.null(cluster)
  
  # Check ecflow_suite_dir
  suite_id <- sample(10 ^ 10, 1)
  ecflow_suite_dir_suite <- ''
  if (on_cluster) {
    if (is.null(ecflow_suite_dir)) {
      stop("Parameter 'ecflow_suite_dir' must be specified when dispatching on a cluster.")
    }
    if (!is.character(ecflow_suite_dir)) {
      stop("Parameter 'ecflow_suite_dir' must be a character string.")
    }
    ecflow_suite_dir_suite <- paste0(ecflow_suite_dir, '/STARTR_CHUNKING_', suite_id, '/')
    dir.create(ecflow_suite_dir_suite, recursive = TRUE)
    if (!dir.exists(ecflow_suite_dir_suite)) {
      stop("Could not find or create the directory in ",
           "parameter 'ecflow_suite_dir'.")
    }
  }
  
  # Check cluster
  default_cluster <- list(queue_host = NULL,
                          queue_type = 'slurm',
                          data_dir = NULL,
                          temp_dir = NULL,
                          lib_dir = NULL,
                          init_commands = list(''),
                          r_module = 'R',
                          CDO_module = NULL,
                          ecflow_module = 'ecFlow',
                          node_memory = NULL,
                          cores_per_job = NULL,
                          job_wallclock = '01:00:00',
                          max_jobs = 6,
                          extra_queue_params = list(''),
                          bidirectional = TRUE,
                          polling_period = 10,
                          special_setup = 'none')
  if (on_cluster) {
    if (!is.list(cluster)) {
      stop("Parameter 'cluster' must be a named list.")
    }
    if (is.null(names(cluster))) {
      stop("Parameter 'cluster' must be a named list.")
    }
    if (any(!(names(cluster) %in% c('queue_host', 'queue_type', 'data_dir',
                                    'temp_dir', 'lib_dir', 'init_commands',
                                    'r_module', 'CDO_module', 'autosubmit_module',
                                    'ecflow_module', 'node_memory',
                                    'cores_per_job', 'job_wallclock', 'max_jobs',
                                    'extra_queue_params', 'bidirectional',
                                    'polling_period', 'special_setup', 'expid', 'hpc_user')))) {

      stop("Found invalid component names in parameter 'cluster'.")
    }
    # Remove ecFlow components
    redundant_components <- c('autosubmit_module', 'expid', 'hpc_user')
    if (any(redundant_components %in% names(cluster))) {
      tmp <- redundant_components[which(redundant_components %in% names(cluster))]
      .warning(paste0("Cluster component ", paste(tmp, collapse = ','), " not used when ecFlow is the workflow manager."))
      cluster[[tmp]] <- NULL
    }
    default_cluster[names(cluster)] <- cluster
  }
  localhost_name <- NULL
  cluster <- default_cluster
  remote_ecflow_suite_dir <- ecflow_suite_dir
  is_data_dir_shared <- FALSE
  is_ecflow_suite_dir_shared <- FALSE
  if (on_cluster) {
    #localhost_name <- Sys.info()[['nodename']]
    localhost_name <- system('hostname -f', intern = TRUE)
    if (Sys.which('ecflow_client') == '') {
      stop("ecFlow must be installed in order to run the computation on clusters.")
    }
    if (is.null(cluster[['queue_host']])) {
      queue_host <- localhost_name
    } else if ((cluster[['queue_host']] %in% c('localhost', '127.0.0.1', localhost_name)) ||
               grepl(paste0('^', localhost_name), cluster[['queue_host']])) { 
      queue_host <- localhost_name
    }
    if (!(cluster[['queue_type']] %in% c('slurm', 'pbs', 'lsf', 'host'))) {
      stop("The only supported 'queue_type's are 'slurm', 'pbs', 'lsf' and 'host'.")
    }
    if (is.null(cluster[['data_dir']])) {
      is_data_dir_shared <- TRUE
    } else {
      if (!is.character(cluster[['data_dir']])) {
        stop("The component 'data_dir' of the parameter 'cluster' must be a character string.")
      }
      remote_data_dir <- cluster[['data_dir']]
    }
    if (is.null(cluster[['temp_dir']])) {
      is_ecflow_suite_dir_shared <- TRUE
    } else {
      if (!is.character(cluster[['temp_dir']])) {
        stop("The component 'temp_dir' of the parameter 'cluster' must be a character string.")
      }
      remote_ecflow_suite_dir <- cluster[['temp_dir']]
    }
    if (!is.null(cluster[['lib_dir']])) {
      if (!is.character(cluster[['lib_dir']])) {
        stop("The component 'lib_dir', of the parameter 'cluster' must be NULL or ",
             "a character string.")
      }
    }
    if (!is.logical(cluster[['bidirectional']])) {
      stop("The component 'bidirectional' of the parameter 'cluster' must be a logical value.")
    }
    if (cluster[['bidirectional']]) {
      cluster[['init_commands']] <- c(cluster[['init_commands']], 
                                      list(paste('module load', cluster[['ecflow_module']])))
    }
    if (!is.list(cluster[['init_commands']]) || 
        !all(sapply(cluster[['init_commands']], is.character))) {
      stop("The component 'init_commands' of the parameter 'cluster' must be a list of ",
           "character strings.")
    }
    if (!is.character(cluster[['r_module']])) {
      stop("The component 'r_module' of the parameter 'cluster' must be a character string.")
    }
    if ((nchar(cluster[['r_module']]) < 1) || (grepl(' ', cluster[['r_module']]))) {
      stop("The component 'r_module' of the parameter 'cluster' must have at least one character ",
           "and contain no blank spaces.")
    }
    if (!is.null(cluster[['CDO_module']])) {
      if (!is.character(cluster[['CDO_module']])) {
        stop("The component 'CDO_module' of the parameter 'cluster' must be a character string.")
      }
      if (nchar(cluster[['CDO_module']]) < 1 || grepl(' ', cluster[['CDO_module']])) {
        .warning(paste0("The component 'CDO_module' of parameter 'cluster' must have ",
                " than 1 and only the first element will be used."))
      }
      cluster[['r_module']] <- paste(cluster[['r_module']], cluster[['CDO_module']])
    }
    if (!is.character(cluster[['ecflow_module']])) {
      stop("The component 'ecflow_module' of the parameter 'cluster' must be a character string.")
    }
    if ((nchar(cluster[['ecflow_module']]) < 1) || 
        (grepl(' ', cluster[['ecflow_module']]))) {
      stop("The component 'ecflow_module' of the parameter 'cluster' must have at least ",
           "one character, and contain no blank spaces.")
    }
    if (is.null(cluster[['cores_per_job']])) {
      cluster[['cores_per_job']] <- threads_compute
    }
    if (!is.numeric(cluster[['cores_per_job']])) {
      stop("The component 'cores_per_job' of the parameter 'cluster' must be numeric.")
    }
    cluster[['cores_per_job']] <- round(cluster[['cores_per_job']])
    if (cluster[['cores_per_job']] > threads_compute) {
      .message("WARNING: 'threads_compute' should be >= cluster[['cores_per_job']].")
    }
    if (!is.list(cluster[['extra_queue_params']]) || 
        !all(sapply(cluster[['extra_queue_params']], is.character))) {
      stop("The component 'extra_queue_params' of the parameter 'cluster' must be a list of ",
           "character strings.")
    }
    if (!(cluster[['special_setup']] %in% c('none', 'marenostrum4'))) {
      stop("The value provided for the component 'special_setup' of the parameter ",
           "'cluster' is not recognized.")
    }
  }
  
  # Check ecflow_suite_dir
  remote_ecflow_suite_dir_suite <- ''
  if (on_cluster) {
    remote_ecflow_suite_dir_suite <- paste0(remote_ecflow_suite_dir, '/STARTR_CHUNKING_', suite_id, '/')
  }
  
  # Check ecflow_server
  if (!is.null(ecflow_server) && !(is.character(ecflow_server))) {
    stop("Parameter 'ecflow_server' must be a character string if specified.")
  }
  
  # Check silent
  if (!is.logical(silent)) {
    stop("Parameter 'silent' must be logical.")
  }
  
  # Check debug
  if (!is.logical(debug)) {
    stop("Parameter 'debug' must be logical.")
  }
  if (silent) {
    debug <- FALSE
  }
  
  # Check wait
  if (!is.logical(wait)) {
    stop("Parameter 'wait' must be logical.")
  }
  
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
  
  if (length(cube_headers) != length(attr(step_fun, 'TargetDims'))) {
    stop("Number of inputs in parameter 'cube_headers' must be equal to the ",
         "number of inputs expected by the function 'step_fun'.")
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
  chunks <- default_chunks
  timings[['nchunks']] <- prod(unlist(chunks))
  
  # Check step_fun
  if (!is(step_fun, 'startR_step_fun')) {
    stop("Parameter 'step_fun' must be of the class 'startR_step_fun', as returned ",
         "by the function Step.")
  }
  
  # Replace 'all's
  chunks_all <- which(unlist(chunks) == 'all')
  if (length(chunks_all) > 0) {
    chunks[chunks_all] <- all_dims[names(chunks)[chunks_all]] 
  }
  # Mount the ecFlow suite
  if (on_cluster) {
    .message(paste0("ATTENTION: Dispatching chunks on a remote cluster",
                    ". Make sure passwordless ",
                    "access is properly set in both directions."))
    
    # Copy load_process_save_chunk_ecflow.R into shared folder
    chunk_script <- file(system.file('chunking/ecFlow/load_process_save_chunk_ecflow.R', 
                                     package = 'startR'))
    chunk_script_lines <- readLines(chunk_script)
    close(chunk_script)
    chunk_script_lines <- gsub('^lib_dir <- *', paste0('lib_dir <- ', 
                                                       paste(deparse(cluster[['lib_dir']]), collapse = '\n')), 
                               chunk_script_lines)
    chunk_script_lines <- gsub('^out_dir <- *', paste0('out_dir <- ', 
                                                       paste(deparse(remote_ecflow_suite_dir_suite), collapse = '\n')), chunk_script_lines)
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
    chunk_script_lines <- gsub('^start_calls_attrs <- *', paste0('start_calls_attrs <- ', paste(deparse(lapply(cube_headers, attributes)), collapse = '\n')), 
                               chunk_script_lines)
    chunk_script_lines <- gsub('^param_dimnames <- *', paste0('param_dimnames <- ', paste(deparse(chunked_dims), collapse = '\n')), 
                               chunk_script_lines)
    chunk_script_lines <- gsub('^threads_load <- *', paste0('threads_load <- ', threads_load), 
                               chunk_script_lines)
    chunk_script_lines <- gsub('^threads_compute <- *', paste0('threads_compute <- ', threads_compute), 
                               chunk_script_lines)
    chunk_script_lines <- gsub('^fun <- *', paste0('fun <- ', paste(deparse(step_fun), collapse = '\n')), 
                               chunk_script_lines)
    chunk_script_lines <- gsub('^params <- *', paste0('params <- ', paste(deparse(list(...)), collapse = '\n')), 
                               chunk_script_lines)
    writeLines(chunk_script_lines, paste0(ecflow_suite_dir_suite, '/load_process_save_chunk_ecflow.R'))
    
    # Copy Chunk.ecf into shared folder
    chunk_ecf_script <- file(system.file('chunking/ecFlow/Chunk.ecf',
                                         package = 'startR'))
    chunk_ecf_script_lines <- readLines(chunk_ecf_script)
    close(chunk_ecf_script)
    if (cluster[['queue_type']] == 'host') {
      chunk_ecf_script_lines <- gsub('^include_queue_header', 
                                     '',
                                     chunk_ecf_script_lines)
    } else {
      chunk_ecf_script_lines <- gsub('^include_queue_header', 
                                     paste0('%include "./', cluster[['queue_type']], '.h"'),
                                     chunk_ecf_script_lines)
    }
    chunk_ecf_script_lines <- gsub('^include_init_commands', 
                                   paste0(paste0(cluster[['init_commands']], collapse = '\n'), '\n'),
                                   chunk_ecf_script_lines)
    chunk_ecf_script_lines <- gsub('^include_module_load', 
                                   paste0('module load ', cluster[['r_module']]),
                                   chunk_ecf_script_lines)
    ecf_vars <- paste0('%', as.vector(sapply(chunked_dims,
                                             function(x) {
                                               c(toupper(x), paste0(toupper(x), '_N'))
                                             })), '%')
    #    if (!is_ecflow_suite_dir_shared && (cluster[['queue_host']] != localhost_name)) {
    #      #transfer_back_line <- paste0('rsync -rav %REMOTE_ECF_HOME% ', localhost_name, 
    #      #                             ':%ECF_HOME%\nrm -f %ECF_HOME%/', 
    #      #                             paste0('*', paste(ecf_vars[((1:(length(ecf_vars) / 2)) * 2) - 1], collapse = '*'), '*.Rds'))
    result_file_id <- paste0('*', 
                             paste(paste0('_', ecf_vars[((1:(length(ecf_vars) / 2)) * 2) - 1], '__'), 
                                   collapse = '*'), '*')
    #      transfer_back_line <- paste0('rsync -rav %REMOTE_ECF_HOME%/%SUITE%/ ', 
    #                                   localhost_name, 
    #                                   ':%ECF_HOME%/%SUITE%/\nscp %REMOTE_ECF_HOME%/', 
    #                                   result_file_id, ' ', localhost_name, 
    #                                   ':%ECF_HOME%\nrm -f %REMOTE_ECF_HOME%/', 
    #                                   result_file_id)
    #    } else {
    #      transfer_back_line <- ''
    #    }
    chunk_ecf_script_lines <- gsub('^Rscript load_process_save_chunk_ecflow.R --args \\$task_path insert_indices', 
                                   paste0('Rscript load_process_save_chunk_ecflow.R --args $task_path ', paste(ecf_vars, collapse = ' ')),
                                   chunk_ecf_script_lines)
    #chunk_ecf_script_lines <- gsub('^include_transfer_back_and_rm', transfer_back_line, chunk_ecf_script_lines)
    writeLines(chunk_ecf_script_lines, paste0(ecflow_suite_dir_suite, '/Chunk.ecf'))
    
    # Copy merge_chunks.R into tmp folder
    #    merge_script <- file(system.file('chunking/merge_chunks.R',
    #                                     package = 'startR'))
    #    merge_script_lines <- readLines(merge_script)
    #    close(merge_script)
    #    merge_script_lines <- gsub('^shared_dir <- *', paste0('shared_dir <- ', 
    #                               paste(deparse(shared_dir_suite), collapse = '\n')), merge_script_lines)
    #    writeLines(merge_script_lines, paste0(shared_dir_suite, '/merge_chunks.R'))
    
    # Copy Merge.ecf into tmp folder
    #TODO: Modify chain of parameters sent to r script when merging 
    #chunks progressively
    #    merge_ecf_script <- file(system.file('chunking/Merge.ecf',
    #                                         package = 'startR'))
    #    merge_ecf_script_lines <- readLines(merge_ecf_script)
    #    close(merge_ecf_script)
    #    writeLines(merge_ecf_script_lines, paste0(shared_dir_suite, '/Merge.ecf')) 
    
    # Copy queue header into shared folder
    #file.copy(system.file(paste0('chunking/', cluster[['queue_type']], '.h'), package = 'startR'),
    #          ecflow_suite_dir_suite)
    chunk_queue_header <- file(system.file(paste0('chunking/ecFlow/', cluster[['queue_type']], '.h'), package = 'startR'))
    chunk_queue_header_lines <- readLines(chunk_queue_header)
    close(chunk_queue_header)
    chunk_queue_header_lines <- gsub('^include_extra_queue_params', 
                                     paste0(paste0(cluster[['extra_queue_params']], collapse = '\n'), '\n'),
                                     chunk_queue_header_lines)
    writeLines(chunk_queue_header_lines, paste0(ecflow_suite_dir_suite, '/', cluster[['queue_type']], '.h'))
    
    # Copy headers
    file.copy(system.file('chunking/ecFlow/head.h', package = 'startR'),
              ecflow_suite_dir_suite)
    file.copy(system.file('chunking/ecFlow/tail.h', package = 'startR'),
              ecflow_suite_dir_suite)
  }
  
  add_line <- function(suite, line, tabs) {
    c(suite, paste0(paste(rep(' ', tabs), collapse = ''), line))
  }
  suite <- NULL
  tabs <- 0
  suite <- add_line(suite, paste0('suite STARTR_CHUNKING_', suite_id), tabs)
  tabs <- tabs + 2
  submit_command <- ''
  if (cluster[['queue_type']] == 'slurm') {
    submit_command <- 'sbatch'
  } else if (cluster[['queue_type']] == 'pbs') {
    submit_command <- 'qsub'
  } else if (cluster[['queue_type']] == 'lsf') {
    submit_command <- 'bsub <'
  } else if (cluster[['queue_type']] == 'host') {
    submit_command <- 'bash'
  }
  if (on_cluster) {
    suite <- add_line(suite, paste0("edit BIDIRECTIONAL '", cluster[['bidirectional']], "'"), tabs)
    suite <- add_line(suite, paste0("edit QUEUE_HOST '", cluster[['queue_host']], "'"), tabs)
    suite <- add_line(suite, paste0("edit ECF_HOST '", localhost_name, "'"), tabs)
    suite <- add_line(suite, paste0("edit EC_HOST_FULL '", localhost_name, "'"), tabs)
    suite <- add_line(suite, paste0("edit RESULT_FILE_ID '", result_file_id, "'"), tabs)
    #} else {
    #  suite <- add_line(suite, paste0("edit ECF_JOB_CMD '", submit_command, " %ECF_JOB% > %ECF_JOBOUT% 2>&1 &'"), tabs)
  }
  suite <- add_line(suite, paste0("edit ECF_HOME '", ecflow_suite_dir_suite, "'"), tabs)
  suite <- add_line(suite, paste0("edit REMOTE_ECF_HOME '", remote_ecflow_suite_dir_suite, "'"), tabs)
  suite <- add_line(suite, paste0("edit CORES_PER_JOB ", cluster[['cores_per_job']], ""), tabs)
  suite <- add_line(suite, paste0("edit JOB_WALLCLOCK '", cluster[['job_wallclock']], "'"), tabs)
  suite <- add_line(suite, paste0("limit max_jobs ", cluster[['max_jobs']]), tabs)
  suite <- add_line(suite, paste0("inlimit max_jobs"), tabs)
  suite <- add_line(suite, "family computation", tabs)
  tabs <- tabs + 2
  
  if (on_cluster) {
    # source $HOME/.profile ;
    sync_command <- ''
    if (!is_ecflow_suite_dir_shared) {
      sync_command <- paste0("rsync -rav ",
                             "%ECF_HOME%/ ",
                             "%QUEUE_HOST%:%REMOTE_ECF_HOME%/ ; ")
    }
    suite <- add_line(suite, paste0("edit ECF_JOB_CMD '",
                                    #"mkdir -p %REMOTE_ECF_HOME%/%SUITE%/ ; ",
                                    sync_command, 
                                    "ssh %QUEUE_HOST% \"",
                                    "date --rfc-3339=seconds > %REMOTE_ECF_HOME%/%ECF_NAME%.submit_time ; ",
                                    submit_command, 
                                    " %REMOTE_ECF_HOME%/%ECF_NAME%.job%ECF_TRYNO% > ",
                                    "%REMOTE_ECF_HOME%/%ECF_NAME%.%ECF_TRYNO% 2>&1 &\" ",
                                    "2>&1'"), tabs)
    if (is_ecflow_suite_dir_shared) {
      suite <- add_line(suite, paste0("edit REPORT_BACK 'FALSE'"), tabs)
    } else {
      suite <- add_line(suite, paste0("edit REPORT_BACK 'TRUE'"), tabs)
    }
  }
  
  # Open nested ecFlow families
  for (i in length(chunked_dims):1) {
    suite <- add_line(suite, paste0('family ', chunked_dims[i], '_CHUNK_', 1), tabs)
    tabs <- tabs + 2
    suite <- add_line(suite, paste0('edit ', toupper(chunked_dims[i]), ' ', 1), tabs)
    suite <- add_line(suite, paste0('edit ', toupper(chunked_dims[i]), '_N ', chunks[[chunked_dims[i]]]), tabs)
  }
  
  # Iterate through chunks
  chunk_array <- array(1:prod(unlist(chunks)), dim = (unlist(chunks)))
  arrays_of_results <- vector('list', length(attr(step_fun, 'OutputDims')))
  names(arrays_of_results) <- names(attr(step_fun, 'OutputDims'))
  for (component in 1:length(arrays_of_results)) {
    arrays_of_results[[component]] <- vector('list', prod((unlist(chunks))))
    dim(arrays_of_results[[component]]) <- (unlist(chunks))
  }
  if (!on_cluster) {
    t_end_bychunks_setup <- Sys.time()
    timings[['bychunks_setup']] <- as.numeric(difftime(t_end_bychunks_setup, 
                                                       t_begin_bychunks_setup, units = 'secs'))
    timings[['transfer']] <- 0
    timings[['queue']] <- 0
    timings[['job_setup']] <- 0
    timings[['transfer_back']] <- 0
    if (!silent) {
      .message(paste0("Processing chunks... ",
                               "remaining time estimate soon..."))
    }
    time_before_first_chunk <- Sys.time()
    time_after_first_chunk <- NULL
  }
  previous_chunk_indices <- rep(1, length(chunks))
  found_first_result <- FALSE
  for (i in 1:length(chunk_array)) {
    chunk_indices <- which(chunk_array == i, arr.ind = TRUE)[1, ]
    names(chunk_indices) <- names(dim(chunk_array))
    # ADD CHUNK SCRIPT TO SUITE
    families_to_jump <- which(chunk_indices != previous_chunk_indices)
    if (length(families_to_jump) > 0) {
      families_to_jump <- max(families_to_jump)
      # Close ecFlow families
      for (j in 1:families_to_jump) {
        tabs <- tabs - 2
        suite <- add_line(suite, paste0('endfamily'), tabs)
      }
      # Open ecFlow families
      for (j in families_to_jump:1) {
        suite <- add_line(suite, paste0('family ', (chunked_dims)[j], '_CHUNK_', chunk_indices[j]), tabs)
        tabs <- tabs + 2
        suite <- add_line(suite, paste0('edit ', toupper((chunked_dims)[j]), ' ', chunk_indices[j]), tabs)
        suite <- add_line(suite, paste0('edit ', toupper((chunked_dims)[j]), '_N ', chunks[[(chunked_dims)[j]]]), tabs)
      }
    }
    suite <- add_line(suite, "task Chunk", tabs)
    
    if (!on_cluster) {
      if (!silent) {
        .message(paste("Loading chunk", i, 
                                "out of", length(chunk_array), "..."))
      }
      data <- vector('list', length(cube_headers))
      t_begin_load <- Sys.time()
      for (input in 1:length(data)) {
        start_call <- cube_headers[[input]]
        dims_to_alter <- which(names(attr(start_call, 'Dimensions')) %in% names(chunks))
        names_dims_to_alter <- names(attr(start_call, 'Dimensions'))[dims_to_alter]
        # If any dimension comes from split dimensions
        split_dims <- attr(start_call, 'SplitDims')
        
        if (length(split_dims) != 0){
          
          for (k in 1:length(split_dims)) {
            if (any(names(split_dims[[k]]) %in% names_dims_to_alter)) {
              chunks_split_dims <- rep(1, length(split_dims[[k]]))
              names(chunks_split_dims) <- names(split_dims[[k]])
              chunks_indices_split_dims <- chunks_split_dims
              split_dims_to_alter <- which(names(split_dims[[k]]) %in% names_dims_to_alter)
              chunks_split_dims[split_dims_to_alter] <- unlist(chunks[names(split_dims[[k]])[split_dims_to_alter]])
              chunks_indices_split_dims[split_dims_to_alter] <- chunk_indices[names(split_dims[[k]])[split_dims_to_alter]]
              start_call[[names(split_dims)[k]]] <- .chunk(chunks_indices_split_dims, chunks_split_dims,
                                                          eval(start_call[[names(split_dims)[k]]]))
              dims_to_alter_to_remove <- which(names_dims_to_alter %in% names(split_dims[[k]]))
              if (length(dims_to_alter_to_remove) > 0) {
                dims_to_alter <- dims_to_alter[-dims_to_alter_to_remove]
                names_dims_to_alter <- names_dims_to_alter[-dims_to_alter_to_remove]
              }
            }
          }
        }
        
        if (length(dims_to_alter) > 0) {
          for (call_dim in names(attr(start_call, 'Dimensions'))[dims_to_alter]) {
            start_call[[call_dim]] <- .chunk(chunk_indices[call_dim], chunks[[call_dim]], 
                                            eval(start_call[[call_dim]]))
          }
        }
        start_call[['silent']] <- !debug
        if (!('num_procs' %in% names(start_call))) {
          start_call[['num_procs']] <- threads_load
        }
        data[[input]] <- eval(start_call)
      }
      t_end_load <- Sys.time()
      timings[['load']] <- c(timings[['load']], 
                             as.numeric(difftime(t_end_load, t_begin_load, units = 'secs')))
      if (!silent) {
        .message(paste("Processing..."))
      }
      #TODO: Find a better way to assign the names of data. When multiple steps for Compute is available, this way may fail.
      names(data) <- names(cube_headers)
      t_begin_compute <- Sys.time()
      result <- multiApply::Apply(data, 
                                  target_dims = attr(step_fun, 'TargetDims'), 
                                  fun = step_fun, ..., 
                                  output_dims = attr(step_fun, 'OutputDims'),
                                  use_attributes = attr(step_fun, 'UseAttributes'),
                                  ncores = threads_compute)
      if (!found_first_result) {
        names(arrays_of_results) <- names(result)
        found_first_result <- TRUE
      }
      for (component in 1:length(result)) {
        arrays_of_results[[component]][[i]] <- result[[component]]
      }
      rm(data)
      gc()
      t_end_compute <- Sys.time()
      timings[['compute']] <- c(timings[['compute']], 
                                as.numeric(difftime(t_end_compute, 
                                                    t_begin_compute, units = 'secs')))
    }
    
    # Time estimate
    if (!on_cluster) {
      if (is.null(time_after_first_chunk)) {
        time_after_first_chunk <- Sys.time()
        if (!silent) {
          estimate <- (time_after_first_chunk - 
                         time_before_first_chunk) *
            (length(chunk_array) - 1)
          units(estimate) <- 'mins'
          .message(
            paste0("Remaining time estimate (at ", format(time_after_first_chunk), ") ", 
                   "(neglecting merge time): ", format(estimate))
          )
        }
      }
    }
    previous_chunk_indices <- chunk_indices
  }
  
  # Close nested ecFlow families
  for (i in length(chunked_dims):1) {
    tabs <- tabs - 2
    suite <- add_line(suite, paste0('endfamily'), tabs)
  }
  
  # Close the ecFlow suite
  tabs <- tabs - 2
  suite <- add_line(suite, paste0('endfamily'), tabs)
  #  suite <- add_line(suite, "family merge", tabs)
  #  tabs <- tabs + 2
  #  suite <- add_line(suite, "trigger computation == complete", tabs)
  #  suite <- add_line(suite, "edit ECF_JOB_CMD 'bash %ECF_JOB% > %ECF_JOBOUT% 2>&1 &'", tabs)
  #  suite <- add_line(suite, "task Merge", tabs)
  #  tabs <- tabs - 2
  #  suite <- add_line(suite, paste0('endfamily'), tabs)
  
  tabs <- tabs - 2
  suite <- add_line(suite, "endsuite", tabs)
  
  # Run ecFlow suite if needed
  if (on_cluster) {
    timings[['cores_per_job']] <- cluster[['cores_per_job']]
    timings[['concurrent_chunks']] <- cluster[['max_jobs']]
    suite_file <- paste0(ecflow_suite_dir_suite, '/startR_chunking.def')
    suite_file_o <- file(suite_file)
    writeLines(suite, suite_file_o)
    close(suite_file_o)
    
    default_ecflow_server <- list(host = localhost_name, port = '5678')
    if (is.null(ecflow_server)) {
      .warning("Parameter 'ecflow_server' has not been specified but execution on ",
               "cluster has been requested. An ecFlow server instance will ",
               "be created on localhost:5678.")
    } else {
      if ('host' %in% names(ecflow_server)) {
        stop("A host has been specified for the 'ecflow_server', but this option is not available yet.")
      }
      default_ecflow_server[names(ecflow_server)] <- ecflow_server
    }
    ecflow_server <- default_ecflow_server
    system(paste0("ecflow_start.sh -p ", ecflow_server[['port']]))
    system(paste0("ecflow_client --load=", suite_file, " --host=",
                  ecflow_server[['host']], " --port=", ecflow_server[['port']]))
    if (!is_ecflow_suite_dir_shared) {
      system(paste0('ssh ', cluster[['queue_host']], ' "mkdir -p ', 
                    remote_ecflow_suite_dir_suite, '"'))
      system(paste0('rsync -ra ', ecflow_suite_dir_suite, 
                    ' ', cluster[['queue_host']], ':', 
                    remote_ecflow_suite_dir_suite))
    }
    t_end_bychunks_setup <- Sys.time()
    timings[['bychunks_setup']] <- as.numeric(difftime(t_end_bychunks_setup,
                                                       t_begin_bychunks_setup, units = 'secs'))
    if (!is_data_dir_shared) {
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
                      ' ; module load transfer ; cd ', remote_ecflow_suite_dir_suite,
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
    #    time_after_first_chunk <- NULL
    system(paste0("ecflow_client --begin=STARTR_CHUNKING_", suite_id, 
                  " --host=", ecflow_server[['host']], " --port=", 
                  ecflow_server[['port']]))
    
    timings[['total']] <- t_begin_total 
    startr_exec <- list(cluster = cluster, ecflow_server = ecflow_server,
                        workflow_manager = 'ecFlow', 
                        suite_id = suite_id, chunks = chunks, 
                        num_outputs = length(arrays_of_results),
                        ecflow_suite_dir = ecflow_suite_dir, 
                        timings = timings)
    class(startr_exec) <- 'startR_exec'
    if (wait) {
      if (!silent) {
        .message(paste0("Remaining time estimate soon... "))
        #        while (is.null(time_after_first_chunk)) {
        #          if (any(grepl('.*\\.Rds$', list.files(ecflow_suite_dir_suite)))) {
        #            time_after_first_chunk <- Sys.time()
        #            estimate <- (time_after_first_chunk -
        #                         time_before_first_chunk) *
        #                        ceiling((prod(unlist(chunks)) - cluster[['max_jobs']]) /
        #                                cluster[['max_jobs']])
        #            units(estimate) <- 'mins'
        #            .message(
        #              paste0('Remaining time estimate (neglecting queue and ',
        #                     'merge time) (at ', format(time_after_first_chunk),
        #                     '): ', format(estimate), ' (', 
        #                     format(time_after_first_chunk - 
        #                            time_before_first_chunk), ' per chunk)')
        #            )
        #          } else if (!cluster[['bidirectional']]) {
        #            rsync_output <- tryCatch({
        #              system(paste0("rsync -ra --ignore-missing-args ", 
        #                            cluster[['queue_host']], ":",
        #                            remote_ecflow_suite_dir_suite, "/*.Rds ",
        #                            ecflow_suite_dir_suite, "/"), intern = TRUE)
        #            }, error = function(e) {
        #              message("Warning: rsync from remote server to collect results failed. ",
        #                      "Retrying soon.")
        #              failed <- TRUE
        #            })
        #            Sys.sleep(cluster[['polling_period']])
        #          }
        #        }
        startr_exec[['t_begin_first_chunk']] <- time_begin_first_chunk
      }
      result <- Collect(startr_exec, wait = TRUE)
      .message("Computation ended successfully.")
      result
    } else {
      startr_exec
    }
  } else {
    timings[['cores_per_job']] <- NA
    timings[['concurrent_chunks']] <- 1
    t_begin_merge <- Sys.time()
    for (component in 1:length(arrays_of_results)) {
      arrays_of_results[[component]] <- .MergeArrayOfArrays(arrays_of_results[[component]])
    }
    t_end_merge <- Sys.time()
    timings[['merge']] <- as.numeric(difftime(t_end_merge, t_begin_merge, units = 'secs'))
    t_end_total <- t_end_merge
    timings[['total']] <- as.numeric(difftime(t_end_total, t_begin_total, units = 'secs'))
    message(paste0("* Computation ended successfully."))
    message(paste0("*   Number of chunks: ", 
                   timings[['nchunks']]))
    message(paste0("*   Max. number of concurrent chunks (jobs): ", 
                   timings[['concurrent_chunks']]))
    message(paste0("*   Requested cores per job: ", 
                   timings[['cores_per_job']]))
    message(paste0("*   Load threads per chunk: ", 
                   timings[['threads_load']]))
    message(paste0("*   Compute threads per chunk: ", 
                   timings[['threads_compute']]))
    message(paste0("*   Total time (s): ", 
                   timings[['total']]))
    message(paste0("*     Chunking setup: ", 
                   timings[['bychunks_setup']]))
    message(paste0("*     Data upload to cluster: ", 
                   timings[['transfer']]))
    message(paste0("*     All chunks: ", 
                   timings[['total']] - 
                     timings[['bychunks_setup']] - 
                     timings[['transfer']] - 
                     timings[['transfer_back']] - 
                     timings[['merge']]))
    message(paste0("*     Transfer results from cluster: ", 
                   timings[['transfer_back']]))
    message(paste0("*     Merge: ", 
                   timings[['merge']]))
    message(paste0("*     Each chunk: "))
    message(paste0("*       queue: "))
    message(paste0("*         mean: ", 
                   mean(timings[['queue']])))
    message(paste0("*         min: ", 
                   min(timings[['queue']])))
    message(paste0("*         max: ", 
                   max(timings[['queue']])))
    message(paste0("*       job setup: "))
    message(paste0("*         mean: ", 
                   mean(timings[['job_setup']])))
    message(paste0("*         min: ", 
                   min(timings[['job_setup']])))
    message(paste0("*         max: ", 
                   max(timings[['job_setup']])))
    message(paste0("*       load: ")) 
    message(paste0("*         mean: ", 
                   mean(timings[['load']])))
    message(paste0("*         min: ", 
                   min(timings[['load']])))
    message(paste0("*         max: ", 
                   max(timings[['load']])))
    message(paste0("*       compute: ")) 
    message(paste0("*         mean: ", 
                   mean(timings[['compute']])))
    message(paste0("*         min: ", 
                   min(timings[['compute']])))
    message(paste0("*         max: ", 
                   max(timings[['compute']])))
    attr(arrays_of_results, 'startR_compute_profiling') <- timings
    arrays_of_results
  }
  #TODO: check result dimensions match expected dimensions
}

ByChunks <- function(step_fun, cube_headers, ..., chunks = 'auto',
                     threads_load = 2, threads_compute = 1,
                     cluster = NULL,
                     ecflow_suite_dir = NULL,
                     ecflow_server = NULL,
                     silent = FALSE, debug = FALSE,
                     wait = TRUE) {

 stop(.Deprecated("ByChunks_ecflow"))
}

