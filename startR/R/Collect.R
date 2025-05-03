#'Collect and merge the computation results
#'
#'The final step of the startR workflow after the data operation. It is used when
#'the parameter 'wait' of Compute() is FALSE. It combines all the chunks of the
#'results as one data array when the execution is done. See more details on 
#'\href{https://earth.bsc.es/gitlab/es/startR/-/blob/master/inst/doc/practical_guide.md}{practical guide}.
#'Collect() calls Collect_ecflow() or Collect_autosubmit() according to the 
#'chosen workflow manager. 
#'@param startr_exec An R object returned by Compute() when the parameter 'wait'
#'  of Compute() is FALSE. It can be directly from a Compute() call or read from
#'  the RDS file.
#'@param wait A logical value deciding whether the R session waits for the 
#'  Collect() call to finish (TRUE) or not (FALSE). If TRUE, it will be a 
#'  blocking call, in which Collect() will retrieve information from the HPC,
#'  including signals and outputs, each polling_period seconds. The the status
#'  can be monitored on the workflow manager GUI. Collect() will not return  
#'  until the results of all the chunks have been received. If FALSE, Collect()
#'  return an error if the execution has not finished, otherwise it will return
#'  the merged array. The default value is TRUE.
#'@param remove A logical value deciding whether to remove of all chunk results 
#'  received from the HPC after data being collected, as well as the local job 
#'  folder under 'ecflow_suite_dir' or 'autosubmit_suite_dir'. To preserve the
#'  data and Collect() them as many times as desired, set remove to FALSE. The
#'  default value is TRUE.
#' @param on_remote A logical value deciding to the function is run locally and
#'   sync the outputs back from HPC (FALSE, default), or it is run on HPC 
#'   (TRUE).
#'@return A list of merged data array.
#'
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
#'  \dontrun{
#'  res <- Compute(wf, chunks = list(longitude = 2, sdate = 2),
#'                 threads_load = 1,
#'                 threads_compute = 4,
#'                 cluster = list(queue_host = 'nord3',
#'                                queue_type = 'lsf',
#'                                temp_dir = '/on_hpc/tmp_dir/',
#'                                cores_per_job = 2,
#'                                job_wallclock = '05:00',
#'                                max_jobs = 4,
#'                                extra_queue_params = list('#BSUB -q bsc_es'),
#'                                bidirectional = FALSE,
#'                                polling_period = 10
#'                 ),
#'                 ecflow_suite_dir = '/on_local_machine/username/ecflow_dir/',
#'                 wait = FALSE)
#'  saveRDS(res, file = 'test_collect.Rds')
#'  collect_info <- readRDS('test_collect.Rds')
#'  result <- Collect(collect_info, wait = TRUE)
#'  }
#'
#'@export
Collect <- function(startr_exec, wait = TRUE, remove = TRUE, on_remote = FALSE) {
  # Parameter checks
  if (!is(startr_exec, 'startR_exec')) {
    stop("Parameter 'startr_exec' must be an object of the class ",
         "'startR_exec', as returned by Compute(..., wait = FALSE).")
  }
  if (!tolower(startr_exec$workflow_manager) %in% c('ecflow', 'autosubmit')) {
    stop("Cannot identify the workflow manager. Check the value of 'startr_exec$workflow_manager', which should be 'ecFlow' or 'Autosubmit'.")
  }
  if (!is.logical(wait)) {
    stop("Parameter 'wait' must be logical.")
  }
  if (!is.logical(remove)) {
    stop("Parameter 'remove' must be logical.")
  }
  if (!is.logical(on_remote)) {
    stop("Parameter 'on_remote' must be logical.")
  }

  if (tolower(startr_exec$workflow_manager) == 'ecflow') {
    res <- Collect_ecflow(startr_exec, wait = wait, remove = remove, on_remote = on_remote)
  } else if (tolower(startr_exec$workflow_manager) == 'autosubmit') {
    res <- Collect_autosubmit(startr_exec, wait = wait, remove = remove, on_remote = on_remote)
  }

  return(res)
}

Collect_ecflow <- function(startr_exec, wait = TRUE, remove = TRUE, on_remote = FALSE) {

  if (!on_remote && Sys.which('ecflow_client') == '') {
    stop("ecFlow must be installed in order to collect results from a ",
         "Compute() execution.")
  }
  cluster <- startr_exec[['cluster']]
  ecflow_server <- startr_exec[['ecflow_server']]
  suite_id <- startr_exec[['suite_id']]
  chunks <- startr_exec[['chunks']]
  num_outputs <- startr_exec[['num_outputs']]
  ecflow_suite_dir <- startr_exec[['ecflow_suite_dir']]
  timings <- startr_exec[['timings']]
  ecflow_suite_dir_suite <- paste0(ecflow_suite_dir, '/STARTR_CHUNKING_', 
                                   suite_id, '/')
  if (!is.null(cluster[['temp_dir']])) { #NOTE: Which case doesn't have temp_dir?
    remote_ecflow_suite_dir <- cluster[['temp_dir']]
    remote_ecflow_suite_dir_suite <- paste0(cluster[['temp_dir']], 
                                            '/STARTR_CHUNKING_', 
                                            suite_id, '/')
  }
  find_task_name <- function(received_file) {
    file_name <- received_file
    parts <- strsplit(file_name, '__')[[1]]
    parts <- parts[c(2:(length(parts) - 1))]
    chunk_indices <- rev(sapply(parts, function(x) {
      as.numeric(strsplit(x, '_')[[1]][2])
    }))
    task_pattern <- paste(paste0('*_', chunk_indices, '/'), 
                          collapse = '')
    task_glob <- paste0(ecflow_suite_dir_suite, '/*/*/', 
                        task_pattern)
    task_path <- Sys.glob(task_glob)
    if (length(task_path) != 1) {
      stop("Unexpected error while receiving results.")
    }
    task_name <- strsplit(task_path, 'computation')[[1]][2]
    task_name <- paste0('/STARTR_CHUNKING_', suite_id, 
                        '/computation', task_name)
    task_name
  }
  done <- FALSE
  attempt <- 1
  if (!on_remote) {
    sum_received_chunks <- sum(grepl('.*\\.Rds$', 
                                     list.files(ecflow_suite_dir_suite)))
  }

  if (cluster[['bidirectional']]) {
    t_transfer_back <- NA
  } else {
    t_transfer_back <- 0
  }
  time_before_first_chunk <- startr_exec[['t_begin_first_chunk']]
  first_chunk_received <- FALSE
  rsync_petition_file_lines <- c('+ *.Rds', '+ *.timings', '+ *.crashed', 
                                 '+ *.running', '- *')
  rsync_petition_file <- tempfile()
  writeLines(rsync_petition_file_lines, rsync_petition_file)
  Sys.sleep(2)
  while (!done) {
    if (!on_remote) {
      if (cluster[['bidirectional']]) {
        status <- system(paste0("ecflow_client --get_state=STARTR_CHUNKING_",
                                suite_id, " --host=",
                                ecflow_server[['host']], " --port=", ecflow_server[['port']]),
                         intern = TRUE)
        if (any(grepl(paste0("suite STARTR_CHUNKING_", suite_id, " #.* state:complete"), status))) {
          done <- TRUE
        } else if (!wait) {
          stop("Computation in progress...")
        }
        if (!first_chunk_received) {
          if (any(grepl('state:complete', status))) {
            if (!is.null(time_before_first_chunk)) {
              time_after_first_chunk <- Sys.time()
              estimate <- (time_after_first_chunk -
                             time_before_first_chunk) *
                ceiling((prod(unlist(chunks)) - cluster[['max_jobs']]) /
                          cluster[['max_jobs']])
              units(estimate) <- 'mins'
              .message(
                paste0('Remaining time estimate (neglecting queue and ',
                       'merge time) (at ', format(time_after_first_chunk),
                       '): ', format(estimate), ' (',
                       format(time_after_first_chunk -
                                time_before_first_chunk), ' per chunk)')
              )
            }
            first_chunk_received <- TRUE
          }
        }
        Sys.sleep(min(sqrt(attempt), 5))
      } else {
      #if (sum_received_chunks == 0) {
      #  # Accounting for the fist chunk received in ByChunks and
      #  # setting it to complete
      #  # ByChunks needs the first chunk to calculate remaining time
      #  received_files <- list.files(ecflow_suite_dir_suite)
      #  received_chunks <- received_files[grepl('Rds$', 
      #                                          received_files)]
      #}
        failed <- FALSE
        t_begin_transfer_back <- Sys.time()
        rsync_output <- tryCatch({
          system(paste0("rsync -rav --include-from=", rsync_petition_file, " '", 
                        cluster[['queue_host']], ":", remote_ecflow_suite_dir_suite, "' ",  
                        ecflow_suite_dir_suite, "/"), intern = TRUE)
        }, error = function(e) {
          message("Warning: rsync from remote server to collect results failed. ",
                  "Retrying soon.")
          failed <- TRUE
        })
        t_end_transfer_back <- Sys.time()
        t_transfer_back <- t_transfer_back + as.numeric(difftime(t_end_transfer_back, 
                                                                 t_begin_transfer_back, units = 'secs'))
        if (!failed) {
          #if (sum_received_chunks == 0) {
          #  rsync_output <- c(rsync_output, received_chunks)
          #}
          received_running <- grepl('running$', rsync_output)
          for (received_chunk_index in which(received_running)) {
            file_name <- rsync_output[received_chunk_index]
            task_name <- find_task_name(file_name)
            system(paste0('ecflow_client --force=active recursive ',
                          task_name, 
                          " --host=", ecflow_server[['host']],
                          " --port=", ecflow_server[['port']]))
          }
          received_crashed <- grepl('crashed$', rsync_output)
          for (received_chunk_index in which(received_crashed)) {
            file_name <- rsync_output[received_chunk_index]
            task_name <- find_task_name(file_name)
            system(paste0('ecflow_client --force=aborted recursive ',
                          task_name, 
                          " --host=", ecflow_server[['host']],
                          " --port=", ecflow_server[['port']]))
          }
          received_chunks <- grepl('Rds$', rsync_output)
          for (received_chunk_index in which(received_chunks)) {
            file_name <- rsync_output[received_chunk_index]
            task_name <- find_task_name(file_name)
            system(paste0('ecflow_client --force=complete recursive ',
                          task_name, 
                          " --host=", ecflow_server[['host']],
                          " --port=", ecflow_server[['port']]))
            sum_received_chunks <- sum_received_chunks + 1
            if (!first_chunk_received) {
              if (!is.null(time_before_first_chunk)) {
                time_after_first_chunk <- Sys.time()
                estimate <- (time_after_first_chunk -
                               time_before_first_chunk) *
                  ceiling((prod(unlist(chunks)) - cluster[['max_jobs']]) /
                            cluster[['max_jobs']])
                units(estimate) <- 'mins'
                .message(
                  paste0('Remaining time estimate (neglecting queue and ',
                         'merge time) (at ', format(time_after_first_chunk),
                         '): ', format(estimate), ' (', 
                         format(time_after_first_chunk - 
                                  time_before_first_chunk), ' per chunk)')
                )
              }
              first_chunk_received <- TRUE
            }
          }
          if (sum_received_chunks / num_outputs == prod(unlist(chunks))) {
            done <- TRUE
          } else if (!wait) {
            stop("Computation in progress...")
          }
        }
        Sys.sleep(cluster[['polling_period']])
      }

    } else {  # on_remote

      sum_received_chunks <- sum(grepl('.*\\.Rds$', list.files(remote_ecflow_suite_dir_suite )))

      if (sum_received_chunks / num_outputs == prod(unlist(chunks))) {
        done <- TRUE
      } else if (!wait) {
        stop("Computation in progress...")
      } else {
        message("Computation in progress,  ", sum_received_chunks, "  of ", prod(unlist(chunks)), " chunks are done.")
        message("Will try again after polling_period...")
        Sys.sleep(cluster[['polling_period']])
      }

    }
    attempt <- attempt + 1
  }
  file.remove(rsync_petition_file)
  timings[['transfer_back']] <- t_transfer_back
  if (!on_remote && !is.null(cluster[['temp_dir']])) {
    system(paste0('ssh ', cluster[['queue_host']], ' "rm -rf ', 
                  remote_ecflow_suite_dir_suite, '"'))
  }
  if (remove) {
    .warning("ATTENTION: The source chunks will be removed from the ",
             "system. Store the result after Collect() ends if needed.")
  }
  if (!on_remote) {
    target_folder <- ecflow_suite_dir
    target_folder_suite <- ecflow_suite_dir_suite
  } else {
    target_folder <- remote_ecflow_suite_dir
    target_folder_suite <- remote_ecflow_suite_dir_suite
  }
  t_begin_merge <- Sys.time()
  result <- .MergeChunks(target_folder, suite_id, remove)
  t_end_merge <- Sys.time()
  timings[['merge']] <- as.numeric(difftime(t_end_merge, t_begin_merge, units = 'secs'))
  received_files <- list.files(target_folder_suite, full.names = TRUE)
  received_timings_files <- received_files[grepl('timings$', received_files)]
  for (timings_file in received_timings_files) {
    times <- readRDS(timings_file)
    timings[['queue']] <- c(timings[['queue']], times['queue'])
    timings[['job_setup']] <- c(timings[['job_setup']], times['job_setup'])
    timings[['load']] <- c(timings[['load']], times['load'])
    timings[['compute']] <- c(timings[['compute']], times['compute'])
  }
  if (remove) {
    if (!on_remote) {
      system(paste0("ecflow_client --delete=force yes /STARTR_CHUNKING_",
                    suite_id, " --host=", ecflow_server[['host']],
                    " --port=", ecflow_server[['port']]))
    }
    unlink(target_folder_suite, recursive = TRUE)
  }
  if (attempt > 2) {
    t_end_total <- Sys.time()
    timings[['total']] <- as.numeric(difftime(t_end_total, timings[['total']], units = 'secs'))
  } else {
    # When attempt <= 2, it means all results were ready possibly from
    # long ago, so is not straightfowrard to work out total time.
    timings[['total']] <- NA
  }
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
  #system("ecflow_client --shutdown --port=5678")
  #system("ecflow_stop.sh -p 5678")
  #result <- readRDS(paste0(ecflow_output_dir, '/result.Rds'))
  #file.remove(paste0(ecflow_output_dir, '/result.Rds'))
  attr(result, 'startR_compute_profiling') <- timings
  result
}



Collect_autosubmit <- function(startr_exec, wait = TRUE, remove = TRUE, on_remote = FALSE) {

  suite_id <- startr_exec[['suite_id']]
  chunks <- startr_exec[['chunks']]
  num_outputs <- startr_exec[['num_outputs']]
  autosubmit_suite_dir <- startr_exec[['autosubmit_suite_dir']]
  autosubmit_suite_dir_suite <- paste0(autosubmit_suite_dir, '/STARTR_CHUNKING_', suite_id, '/')
  remote_autosubmit_suite_dir <- file.path("/esarchive/autosubmit/", suite_id, 'proj')
  remote_autosubmit_suite_dir_suite <- paste0(remote_autosubmit_suite_dir, '/STARTR_CHUNKING_', suite_id, '/')
  run_dir <- startr_exec$cluster[['run_dir']]

  done <- FALSE

  while (!done) { # If wait, try until it is done
    sum_received_chunks <- sum(grepl('.*\\.Rds$', list.files(remote_autosubmit_suite_dir_suite)))
    if (sum_received_chunks / num_outputs == prod(unlist(chunks))) {
      done <- TRUE

    } else if (!wait) {
      stop("Computation in progress...")
    } else {
      message("Computation in progress,  ", sum_received_chunks, "  of ", prod(unlist(chunks)), " chunks are done...\n",
              "Check status on Autosubmit GUI: https://earth.bsc.es/autosubmitapp/experiment/", suite_id)
      Sys.sleep(startr_exec$cluster[['polling_period']])
    }

  } # while !done

  result <- .MergeChunks(remote_autosubmit_suite_dir, suite_id, remove = remove)
  if (remove) {
    .warning("ATTENTION: The source chunks will be removed from the ",
             "system. Store the result after Collect() ends if needed.")
    unlink(paste0(autosubmit_suite_dir_suite),
           recursive = TRUE)
  }

  # Remove bigmemory objects (e.g., a68h_1_1_1_1_1 and a68h_1_1_1_1_1.desc)
  # If run_dir is specified, the files are under run_dir; if not, files are under proj/STARTR_CHUNKING_xxxx/
  if (!is.null(run_dir)) {
    file.remove(
      file.path(run_dir,
        list.files(run_dir)[grepl(paste0("^", suite_id, "_.*"), list.files(run_dir))])
    )
  } else {
    file.remove(
      file.path(remote_autosubmit_suite_dir_suite,
        list.files(remote_autosubmit_suite_dir_suite)[grepl(paste0("^", suite_id, "_.*"), list.files(remote_autosubmit_suite_dir_suite))])
    )
  }

  return(result)
}
