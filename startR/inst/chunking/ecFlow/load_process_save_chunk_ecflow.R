lib_dir <- 
if (!is.null(lib_dir)) {
  if (!dir.exists(lib_dir)) {
    stop("The specified 'lib_dir' does not exist.")
  }
  .libPaths(new = lib_dir)
}
library(startR)

out_dir <- 

debug <- 
start_calls <- 
start_calls_attrs <- 
param_dimnames <- 
fun <- 
params <- 
threads_load <-
threads_compute <-

task_path <- commandArgs(TRUE)[2]

args <- as.integer(commandArgs(TRUE)[-c(1, 2)])

total_specified_dims <- length(args) / 2
chunk_indices <- args[((1:total_specified_dims) - 1) * 2 + 1]
names(chunk_indices) <- param_dimnames
chunks <- as.list(args[((1:total_specified_dims) - 1) * 2 + 2])
names(chunks) <- param_dimnames

t_begin_queue <- as.POSIXct(readLines(paste0(task_path, '.submit_time'))[1])
t_begin_job_setup <- as.POSIXct(readLines(paste0(task_path, '.setup_time'))[1])

t_queue <- as.numeric(difftime(t_begin_job_setup, t_begin_queue, units = 'secs'))
t_end_job_setup <- Sys.time()
t_job_setup <- as.numeric(difftime(t_end_job_setup, t_begin_job_setup, units = 'secs'))

t_begin_load <- Sys.time()
data <- vector('list', length(start_calls))
# Add data names if data input has names
if (!is.null(names(start_calls_attrs))) {
  names(data) <- names(start_calls_attrs)
}
for (input in 1:length(data)) {
  start_call <- start_calls[[input]]
  call_dims <- names(start_calls_attrs[[input]][['Dimensions']])
  dims_to_alter <- which(call_dims %in% param_dimnames)
  names_dims_to_alter <- call_dims[dims_to_alter]
  # If any dimension comes from split dimensions
  split_dims <- start_calls_attrs[[input]][['SplitDims']]
  for (k in 1:length(split_dims)) {
    if (any(names(split_dims[[k]]) %in% names_dims_to_alter)) {
      chunks_split_dims <- rep(1, length(split_dims[[k]]))
      names(chunks_split_dims) <- names(split_dims[[k]])
      chunks_indices_split_dims <- chunks_split_dims
      split_dims_to_alter <- which(names(split_dims[[k]]) %in% names_dims_to_alter)
      chunks_split_dims[split_dims_to_alter] <- unlist(chunks[names(split_dims[[k]])[split_dims_to_alter]])
      chunks_indices_split_dims[split_dims_to_alter] <- chunk_indices[names(split_dims[[k]])[split_dims_to_alter]]
      start_call[[names(split_dims)[k]]] <- startR:::.chunk(chunks_indices_split_dims, chunks_split_dims,
                                                  eval(start_call[[names(split_dims)[k]]]))
      dims_to_alter_to_remove <- which(names_dims_to_alter %in% names(split_dims[[k]]))
      if (length(dims_to_alter_to_remove) > 0) {
        dims_to_alter <- dims_to_alter[-dims_to_alter_to_remove]
        names_dims_to_alter <- names_dims_to_alter[-dims_to_alter_to_remove]
      }
    }
  }
  if (length(dims_to_alter) > 0) {
    for (call_dim in names_dims_to_alter) {
      start_call[[call_dim]] <- startR:::.chunk(chunk_indices[call_dim], chunks[[call_dim]], 
                                      eval(start_call[[call_dim]]))
    }
  }
  if (!('num_procs' %in% names(start_call))) {
    start_call[['num_procs']] <- threads_load
  }
  # Creates a name for the temporal file using the chunks numbers:
  nameMemoryObject <- gsub("[^0-9.-]", "_", gsub(out_dir, "", task_path))
  nameMemoryObject <- substr(nameMemoryObject, 2, nchar(nameMemoryObject))
  removeRS <- function(str) {
    vec <- strsplit(str, "")[[1]]
    res <- vec[1]
    for (i in 2:length(vec)) {
      if (!is.na(as.numeric(vec[i], supressWarnings = TRUE))) {
        res <- c(res, vec[i])
      } else if (res[length(res)] != vec[i]) {
        res <- c(res, vec[i])
      }
    }
    return(paste(res, collapse = "")) 
  }
  nameMemoryObject <- suppressWarnings(removeRS(nameMemoryObject))
  start_call[['ObjectBigmemory']] <- nameMemoryObject
  data[[input]] <- tryCatch(eval(start_call),
                     # Handler when an error occurs:
                     error = function(e) {
                       message(paste("The data cannot be loaded."))
                       message("See the original error message:")
                       message(e)
                       message("\n Current files in /dev/shm:") 
                       noreturn <- lapply(list.files("/dev/shm"), function (x) {
                              info <- file.info(paste0("/dev/shm/", x))
                              message(paste("file:", rownames(info),
                                            "size:",  info$size, 
                                            "uname:", info$uname))})
                       message(getwd())
                       file.remove(nameMemoryObject)
                       file.remove(paste0(nameMemoryObject, ".desc"))
                       message(paste("Files", nameMemoryObject, "has been removed."))
                       stop("The job has failed while loading data. See original error reported above.")
                     })
  warning(attributes(data[[input]])$ObjectBigmemory)
  #write.table(attributes(data[[input]])$ObjectBigmemory,
  #            file = paste0(task_path, '.filename.txt'),
  #            col.names = FALSE, row.names = FALSE, quote = FALSE)
}
t_end_load <- Sys.time()
t_load <- as.numeric(difftime(t_end_load, t_begin_load, units = 'secs'))

t_begin_compute <- Sys.time()
if (!is.null(attr(fun, 'UseLibraries'))) {
  for (i in seq_along(attr(fun, 'UseLibraries'))) {
    require(attr(fun, 'UseLibraries')[i], character.only = TRUE)
  }
}
chunk_indices_apply <- setNames(as.integer(chunk_indices), names(chunk_indices))
chunk_indices_apply <- chunk_indices_apply[names(chunks)[which(chunks > 1)]]
Apply <- multiApply::Apply
res <- do.call("Apply",
               c(
                 list(data,
                      target_dims = attr(fun, 'TargetDims'),
                      fun = fun,
                      output_dims = attr(fun, 'OutputDims'),
                      use_attributes = attr(fun, 'UseAttributes'),
                      extra_info = list(chunk_indices = chunk_indices_apply), 
                      ncores = threads_compute),
                 params
                )
              )
rm(data)
gc()

for (component in names(res)) {
  filename <- paste0(component, '__')
  for (i in 1:total_specified_dims) {
    filename <- paste0(filename, param_dimnames[i], '_', chunk_indices[i], '__')
  }
  # Saving in a temporary file, then renaming. This way, the polling mechanism
  # won't transfer back results before the save is completed.
  saveRDS(res[[component]], file = paste0(out_dir, '/', filename, '.Rds.tmp'))
  file.rename(paste0(out_dir, '/', filename, '.Rds.tmp'), 
              paste0(out_dir, '/', filename, '.Rds'))
}
rm(res)
gc()
t_end_compute <- Sys.time()
t_compute <- as.numeric(difftime(t_end_compute, t_begin_compute, units = 'secs'))

timings <- c(queue = t_queue, job_setup = t_job_setup, 
             load = t_load, compute = t_compute)
saveRDS(timings, file = paste0(out_dir, '/', filename, '.timings'))
