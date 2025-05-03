# Take *_var parameters apart
take_var_params <- function(dim_params) {
 # Take *_var parameters apart
  var_params_ind <- grep('_var$', names(dim_params))
  var_params <- dim_params[var_params_ind]
  # Check all *_var are NULL or vectors of character strings, and 
  # that they all have a matching dimension param.
  i <- 1
  for (var_param in var_params) {
    if (!is.character(var_param)) {
      stop("All '*_var' parameters must be character strings.")
    } else if (!any(grepl(paste0('^', strsplit(names(var_params)[i],
                                               '_var$')[[1]][1], '$'),
                          names(dim_params)))) {
      stop(paste0("All '*_var' parameters must be associated to a dimension parameter. Found parameter '",
                  names(var_params)[i], "' but no parameter '",
                  strsplit(names(var_params)[i], '_var$')[[1]][1], "'."))
    }
    i <- i + 1
  }
  # Make the keys of 'var_params' to be the name of 
  # the corresponding dimension.
  if (length(var_params) < 1) {
    var_params <- NULL
  } else {
    names(var_params) <- gsub('_var$', '', names(var_params))
  }
  return(var_params)
}

# Take *_reorder parameters apart
take_var_reorder <- function(dim_params) {
  # Take *_reorder parameters apart
  dim_reorder_params_ind <- grep('_reorder$', names(dim_params))
  dim_reorder_params <- dim_params[dim_reorder_params_ind]
  # Make the keys of 'dim_reorder_params' to be the name of 
  # the corresponding dimension.
  if (length(dim_reorder_params) < 1) {
    dim_reorder_params <- NULL
  } else {
    names(dim_reorder_params) <- gsub('_reorder$', '', names(dim_reorder_params))
  }
  return(dim_reorder_params)
}

# Take *_depends parameters apart
take_var_depends <- function(dim_params) {
  depends_params_ind <- grep('_depends$', names(dim_params))
  depends_params <- dim_params[depends_params_ind]
  # Check all *_depends are NULL or vectors of character strings, and 
  # that they all have a matching dimension param.
  i <- 1
  for (depends_param in depends_params) {
    if (!is.character(depends_param) || (length(depends_param) > 1)) {
      stop("All '*_depends' parameters must be single character strings.")
    } else if (!any(grepl(paste0('^', strsplit(names(depends_params)[i],
                                               '_depends$')[[1]][1], '$'),
                          names(dim_params)))) {
      stop(paste0("All '*_depends' parameters must be associated to a dimension parameter. Found parameter '",
                  names(depends_params)[i], "' but no parameter '",
                  strsplit(names(depends_params)[i], '_depends$')[[1]][1], "'."))
    }
    i <- i + 1
  }
  # Make the keys of 'depends_params' to be the name of 
  # the corresponding dimension.
  if (length(depends_params) < 1) {
    depends_params <- NULL
  } else {
    names(depends_params) <- gsub('_depends$', '', names(depends_params))
  }
  return(depends_params)
}

# Take *_across parameters apart
take_var_across <- function(dim_params) {
  across_params_ind <- grep('_across$', names(dim_params))
  across_params <- dim_params[across_params_ind]
  # Check all *_across are NULL or vectors of character strings, and 
  # that they all have a matching dimension param.
  i <- 1
  for (across_param in across_params) {
    if (!is.character(across_param) || (length(across_param) > 1)) {
      stop("All '*_across' parameters must be single character strings.")
    } else if (!any(grepl(paste0('^', strsplit(names(across_params)[i],
                                               '_across$')[[1]][1], '$'),
                          names(dim_params)))) {
      stop(paste0("All '*_across' parameters must be associated to a dimension parameter. Found parameter '",
                  names(across_params)[i], "' but no parameter '",
                  strsplit(names(across_params)[i], '_across$')[[1]][1], "'."))
    }
    i <- i + 1
  }
  # Make the keys of 'across_params' to be the name of 
  # the corresponding dimension.
  if (length(across_params) < 1) {
    across_params <- NULL
  } else {
    names(across_params) <- gsub('_across$', '', names(across_params))
  }
  return(across_params)
}

# Leave alone the dimension parameters in the variable dim_params
rebuild_dim_params <- function(dim_params, merge_across_dims,
                               inner_dims_across_files) {
  var_params_ind <- grep('_var$', names(dim_params))
  dim_reorder_params_ind <- grep('_reorder$', names(dim_params))
  tolerance_params_ind <- grep('_tolerance$', names(dim_params))
  depends_params_ind <- grep('_depends$', names(dim_params))
  across_params_ind <- grep('_across$', names(dim_params))
  # Leave alone the dimension parameters in the variable dim_params
  if (length(c(var_params_ind, dim_reorder_params_ind, tolerance_params_ind,
               depends_params_ind, across_params_ind)) > 0) {
    dim_params <- dim_params[-c(var_params_ind, dim_reorder_params_ind,
                                tolerance_params_ind, depends_params_ind,
                                across_params_ind)]
    # Reallocating pairs of across file and inner dimensions if they have
    # to be merged. They are put one next to the other to ease merge later.
    if (merge_across_dims) {
      if (any(!names(inner_dims_across_files) %in% names(dim_params)) |
          any(!unlist(inner_dims_across_files) %in% names(dim_params)))
        stop("All *_across parameters must have value as a file dimension name.")
      for (inner_dim_across in names(inner_dims_across_files)) {
        inner_dim_pos <- which(names(dim_params) == inner_dim_across)
        file_dim_pos <- which(names(dim_params) == inner_dims_across_files[[inner_dim_across]])
        new_pos <- inner_dim_pos
        if (file_dim_pos < inner_dim_pos) {
          new_pos <- new_pos - 1
        }
        dim_params_to_move <- dim_params[c(inner_dim_pos, file_dim_pos)]
        dim_params <- dim_params[-c(inner_dim_pos, file_dim_pos)]
        new_dim_params <- list()
        if (new_pos > 1) {
          new_dim_params <- c(new_dim_params, dim_params[1:(new_pos - 1)])
        }
        new_dim_params <- c(new_dim_params, dim_params_to_move)
        if (length(dim_params) >= new_pos) {
          new_dim_params <- c(new_dim_params, dim_params[new_pos:length(dim_params)])
        }
        dim_params <- new_dim_params
      }
    }
  }
  dim_names <- names(dim_params)
  if (is.null(dim_names)) {
    stop("At least one pattern dim must be specified.")
  }
  return(dim_params)
}

# Look for chunked dims
look_for_chunks <- function(dim_params, dim_names) {
  chunks <- vector('list', length(dim_names))
  names(chunks) <- dim_names
  for (dim_name in dim_names) {
    if (!is.null(attr(dim_params[[dim_name]], 'chunk'))) {
      chunks[[dim_name]] <- attr(dim_params[[dim_name]], 'chunk')
      attributes(dim_params[[dim_name]]) <- attributes(dim_params[[dim_name]])[-which(names(attributes(dim_params[[dim_name]])) == 'chunk')]
    } else {
      chunks[[dim_name]] <- c(chunk = 1, n_chunks = 1)
    }
  }
  return(chunks)
}

# This is a helper function to compute the chunk indices to take once the total
# number of indices for a dimension has been discovered.
  get_chunk_indices <- function(n_indices, chunk, n_chunks, dim_name) {
    if (n_chunks > n_indices) {
      stop("Requested to divide dimension '", dim_name, "' of length ",
           n_indices, " in ", n_chunks, " chunks, which is not possible.")
    }
    chunk_sizes <- rep(floor(n_indices / n_chunks), n_chunks)
    chunks_to_extend <- n_indices - chunk_sizes[1] * n_chunks
    if (chunks_to_extend > 0) {
      chunk_sizes[1:chunks_to_extend] <- chunk_sizes[1:chunks_to_extend] + 1
    }
    chunk_size <- chunk_sizes[chunk]
    offset <- 0
    if (chunk > 1) {
      offset <- sum(chunk_sizes[1:(chunk - 1)])
    }
    indices <- 1:chunk_sizes[chunk] + offset
    array(indices, dim = setNames(length(indices), dim_name))
  }

# Check pattern_dims
# Function found_pattern_dims may change pattern_dims in the parent.env
found_pattern_dims <- function(pattern_dims, dim_names, var_params,
                               dim_params, dim_reorder_params) {
  if (is.null(pattern_dims)) {
    .warning(paste0("Parameter 'pattern_dims' not specified. Taking the first dimension, '",
                    dim_names[1], "' as 'pattern_dims'."))
    assign('pattern_dims', dim_names[1], envir = parent.frame())
    pattern_dims <- dim_names[1]
  } else if (is.character(pattern_dims) && (length(pattern_dims) > 0)) {
    assign('pattern_dims', unique(pattern_dims), envir = parent.frame())
    pattern_dims <- unique(pattern_dims)
  } else {
    stop("Parameter 'pattern_dims' must be a vector of character strings.")
  }
  if (any(names(var_params) %in% pattern_dims)) {
    stop("'*_var' parameters specified for pattern dimensions. Remove or fix them.")
  }
  # Find the pattern dimension with the pattern specifications
  found_pattern_dim <- NULL
  for (pattern_dim in pattern_dims) {
    # Check all specifications in pattern_dim are valid
#    dat <- datasets <- dim_params[[pattern_dim]]
    dat <- dim_params[[pattern_dim]]
    if (is.null(dat) || !(is.character(dat) && all(nchar(dat) > 0)) && !is.list(dat)) {
      stop(paste0("Parameter '", pattern_dim,
                  "' must be a list of lists with pattern specifications or a vector of character strings."))
    }
    if (!is.null(dim_reorder_params[[pattern_dim]])) {
      .warning(paste0("A reorder for the selectors of '", pattern_dim,
                      "' has been specified, but it is a pattern dimension and the reorder will be ignored."))
    }
    if (is.list(dat) || any(sapply(dat, is.list))) {
      if (is.null(found_pattern_dim)) {
        found_pattern_dim <- pattern_dim
      } else {
        stop("Found more than one pattern dim with pattern specifications (list of lists). One and only one pattern dim must contain pattern specifications.")
      }
    }
  }
  if (is.null(found_pattern_dim)) {
   .warning(paste0("Could not find any pattern dim with explicit data set descriptions (in the form of list of lists). Taking the first pattern dim, '", pattern_dims[1], "', as dimension with pattern specifications."))
    found_pattern_dim <- pattern_dims[1]
  }
  return(found_pattern_dim)
}


# The variable 'dat' is mounted with the information (name, path) of each dataset.
# NOTE: This function creates the object 'dat_names' in the parent env.
mount_dat <- function(dat, pattern_dims, found_pattern_dim, dat_names) {

#  dat_info_names <- c('name', 'path')#, 'nc_var_name', 'suffix', 'var_min', 'var_max', 'dimnames')
  dat_to_fetch <- c()
#  dat_names <- c()
  if (!is.list(dat)) {
    dat <- as.list(dat)
  } else {
    if (!any(sapply(dat, is.list))) {
      dat <- list(dat)
    }
  }
  for (i in 1:length(dat)) {
    if (is.character(dat[[i]]) && length(dat[[i]]) == 1 && nchar(dat[[i]]) > 0) {
      if (grepl('^(\\./|\\.\\./|/.*/|~/)', dat[[i]])) {
        dat[[i]] <- list(path = dat[[i]])
      } else {
        dat[[i]] <- list(name = dat[[i]])
      }
    } else if (!is.list(dat[[i]])) {
      stop(paste0("Parameter '", pattern_dims, 
                  "' is incorrect. It must be a list of lists or character strings."))
    }
    #if (!(all(names(dat[[i]]) %in% dat_info_names))) {
    #  stop("Error: parameter 'dat' is incorrect. There are unrecognized components in the information of some of the datasets. Check 'dat' in ?Load for details.")
    #}
    if (!('name' %in% names(dat[[i]]))) {
      dat[[i]][['name']] <- paste0('dat', i)
      if (!('path' %in% names(dat[[i]]))) {
        stop(paste0("Parameter '", found_pattern_dim, 
                    "' is incorrect. A 'path' should be provided for each dataset if no 'name' is provided."))
      }
    } else if (!('path' %in% names(dat[[i]]))) {
      dat_to_fetch <- c(dat_to_fetch, i)
    }
    #if ('path' %in% names(dat[[i]])) {
    #  if (!('nc_var_name' %in% names(dat[[i]]))) {
    #    dat[[i]][['nc_var_name']] <- '$var_name$'
    #  }
    #  if (!('suffix' %in% names(dat[[i]]))) {
    #    dat[[i]][['suffix']] <- ''
    #  }
    #  if (!('var_min' %in% names(dat[[i]]))) {
    #    dat[[i]][['var_min']] <- ''
    #  }
    #  if (!('var_max' %in% names(dat[[i]]))) {
    #    dat[[i]][['var_max']] <- ''
    #  }
    #}
    dat_names <- c(dat_names, dat[[i]][['name']])
  }
  if ((length(dat_to_fetch) > 0) && (length(dat_to_fetch) < length(dat))) {
    .warning("'path' has been provided for some datasets. Any information in the configuration file related to these will be ignored.")
  }
  if (length(dat_to_fetch) > 0) {
    stop("Specified only the name for some data sets, but not the path ",
         "pattern. This option has not been yet implemented.")
  }

  assign('dat_names', dat_names, envir = parent.frame())
  return(dat)
}

# Add attributes indicating whether this dimension selector is value or indice
add_value_indices_flag <- function(x) {
  if (is.null(attr(x, 'values')) || is.null(attr(x, 'indices'))) {
    flag <- (any(x %in% c('all', 'first', 'last')) || is.numeric(unlist(x)))
    attr(x, 'values') <- !flag
    attr(x, 'indices') <- flag
  }
  return(x)
}


# Find the value for the undefined selector (i.e., indices()). Use the value from the first 
# found file.
# Note that "dat[[i]][['path']]" in parent env. is changed in this function.
find_ufd_value <- function(undefined_file_dims, dat, i, replace_values,
                           first_file, file_dims, path_glob_permissive, 
                           depending_file_dims, dat_selectors, selector_checker, chunks) {
  first_values <- vector('list', length = length(undefined_file_dims))
  names(first_values) <- undefined_file_dims
  found_values <- 0
  stop <- FALSE
  try_dim <- 1
  last_success <- 1
  while ((found_values < length(undefined_file_dims)) && !stop) {
    u_file_dim <- undefined_file_dims[try_dim]
    if (is.null(first_values[[u_file_dim]])) {
      path_with_globs_and_tag <- .ReplaceVariablesInString(dat[[i]][['path']], 
                                                           replace_values[-which(file_dims == u_file_dim)],
                                                           allow_undefined_key_vars = TRUE)
      found_value <- .FindTagValue(path_with_globs_and_tag, 
                                   first_file, u_file_dim)
      if (!is.null(found_value)) {
        found_values <- found_values + 1
        last_success <- try_dim
        first_values[[u_file_dim]] <- found_value
        replace_values[[u_file_dim]] <- found_value
      }
    }
    try_dim <- (try_dim %% length(undefined_file_dims)) + 1
    if (try_dim == last_success) {
      stop <- TRUE
    }
  }
  if (found_values < length(undefined_file_dims)) {
    stop(paste0("Path pattern of dataset '", dat[[i]][['name']], 
                "' is too complex. Could not automatically ",
                "detect values for all non-explicitly defined ",
                "indices. Check its pattern: ", dat[[i]][['path']]))
  }
  ## TODO: Replace ReplaceGlobExpressions by looped call to FindTagValue? As done above
  ##       Maybe it can solve more cases actually. I got warnings in ReplGlobExp with a typical
  ##       cmor case, requesting all members and chunks for fixed var and sdate. Not fixing
  ##       sdate raised 'too complex' error.
  # Replace shell globs in path pattern and keep the file_dims as tags
  dat[[i]][['path']] <- .ReplaceGlobExpressions(dat[[i]][['path']], first_file, replace_values, 
                                                file_dims, dat[[i]][['name']], path_glob_permissive)

  # Now time to look for the available values for the non 
  # explicitly defined selectors for the file dimensions.
  #print("H")
  # Check first the ones that do not depend on others.
  ufd <- c(undefined_file_dims[which(!(undefined_file_dims %in% names(depending_file_dims)))],
           undefined_file_dims[which(undefined_file_dims %in% names(depending_file_dims))])
  
  for (u_file_dim in ufd) {
    replace_values[undefined_file_dims] <- first_values
    replace_values[[u_file_dim]] <- '*'
    depended_dim <- NULL
    depended_dim_values <- NA

    #NOTE: Here 'selectors' is always 1. Is it supposed to be like this?
    selectors <- dat_selectors[[u_file_dim]][[1]]
    if (u_file_dim %in% names(depending_file_dims)) {
      depended_dim <- depending_file_dims[[u_file_dim]]
      depended_dim_values <- dat_selectors[[depended_dim]][[1]]
      dat_selectors[[u_file_dim]] <- vector('list', length = length(depended_dim_values))
      names(dat_selectors[[u_file_dim]]) <- depended_dim_values
    } else {
      dat_selectors[[u_file_dim]] <- list()
    }
    if (u_file_dim %in% unlist(depending_file_dims)) {
      depending_dims <- names(depending_file_dims)[which(sapply(depending_file_dims, function(x) u_file_dim %in% x))]
      replace_values[depending_dims] <- rep('*', length(depending_dims))
    }
    # If u_file_dim depends on the same depended dimension as another depending
    # dimension, then the value of the depending dim should be replaced with '*'
    # to avoid only the first value being used, which can result in the wrong
    # path specification.
    other_depending_file_dims <- depending_file_dims[-which(names(depending_file_dims) == u_file_dim)]
    if (length(depending_file_dims) > 1 && 
        any(unlist(other_depending_file_dims) == depended_dim)) {
      depending_dims <- names(other_depending_file_dims)[which(other_depending_file_dims == depended_dim)]
      replace_values[depending_dims] <- rep('*', length(depending_dims))
    }
    for (j in 1:length(depended_dim_values)) {
      parsed_values <- c()
      if (!is.null(depended_dim)) {
        replace_values[[depended_dim]] <- depended_dim_values[j]
      }
      path_with_globs <- .ReplaceVariablesInString(dat[[i]][['path']], replace_values)
      found_files <- Sys.glob(path_with_globs)
      ## TODO: Enhance this error message, or change by warning.
      ##       Raises if a wrong sdate is specified, for example.
      if (length(found_files) == 0) {
        .warning(paste0("Could not find files for any '", u_file_dim, 
                        "' for '", depended_dim, "' = '", 
                        depended_dim_values[j], "'."))
        dat_selectors[[u_file_dim]][[j]] <- NA
      } else {
        for (found_file in found_files) {
          path_with_globs_and_tag <- .ReplaceVariablesInString(dat[[i]][['path']],
                                                               replace_values[-which(file_dims == u_file_dim)],
                                                               allow_undefined_key_vars = TRUE)
          parsed_values <- c(parsed_values, 
                             .FindTagValue(path_with_globs_and_tag, found_file, 
                                           u_file_dim))
        }
        #TODO: selector_checker() doesn't allow selectors to be characters. For selectors
        #      like "member = 'r7i1p1f1", it cannot be defined with values.
        dat_selectors[[u_file_dim]][[j]] <- selector_checker(selectors = selectors,
                                                             var = unique(parsed_values),
                                                             return_indices = FALSE)
        # Take chunk if needed
        dat_selectors[[u_file_dim]][[j]] <- dat_selectors[[u_file_dim]][[j]][get_chunk_indices(length(dat_selectors[[u_file_dim]][[j]]),
                                                                                           chunks[[u_file_dim]]['chunk'],
                                                                                           chunks[[u_file_dim]]['n_chunks'], 
                                                                                           u_file_dim)]
      }
    }
  }
  #NOTE: change 'dat' in parent env. because "dat[[i]][['path']]" is changed.
  assign('dat', dat, envir = parent.frame())
  return(dat_selectors)
}


# Adjust the argument 'return_vars' if users don't assign them properly.
# Force return_vars = (time = NULL) to (time = 'sdate') if one of the situations:
# (1) selector = [sdate = 2, time = 4], or
# (2) time_across = 'sdate'.
correct_return_vars <- function(inner_dim, inner_dims_across_files, found_pattern_dim,
                                file_dim_as_selector_array_dim) {
    # inner_dim is not in return_vars or is NULL
      if (is.character(file_dim_as_selector_array_dim)) { #(1)
        if (any(file_dim_as_selector_array_dim %in% found_pattern_dim)) {
          stop(paste0("Found '", inner_dim, "' selector has dimension of the pattern dim '",
                      found_pattern_dim, 
                      "', which is not allowed. To assign the dependency on the pattern dim, ",
                      "use 'return_vars = list(", inner_dim, " = 'dat')' instead."))
        } else {
          corrected_value <- file_dim_as_selector_array_dim
        }
      } else if (inner_dim %in% inner_dims_across_files) { #(2)
        file_dim_name <- names(which(inner_dim == inner_dims_across_files))
        if (file_dim_name %in% found_pattern_dim) {
          stop(paste0("Found '", inner_dim, "' has across dependency on the pattern dim '",
                      found_pattern_dim, "', which is not allowed."))
        } else {
          corrected_value <- file_dim_name
        }
      }
      .warning(paste0("Found '", inner_dim, "' dependency on file dimension '", corrected_value,
                      "', but '", inner_dim, "' is not in return_vars list or does not include '", corrected_value,
                      "'. To provide the correct metadata, '", corrected_value, "' is included under '", inner_dim,
                      "' in 'return_vars."))
  return(corrected_value)
}

# The time classes that are needed to adjust time zone back to UTC.
time_special_types <- function() {
  list('POSIXct' = as.POSIXct, 'POSIXlt' = as.POSIXlt, 'Date' = as.Date)
}

# Replace the dim names read from netCDF file with the user-specified synonims.
replace_with_synonmins <- function(read_dims, synonims) {
  corrected_dim_name <- sapply(names(read_dims), 
                            function(x) {
                              which_entry <- which(sapply(synonims, function(y) x %in% y))
                              if (length(which_entry) > 0) {
                                names(synonims)[which_entry]
                              } else {
                                x
                              }
                            })
  return(corrected_dim_name)
}


# Prepare vars_to_read for this dataset (i loop) and this file (j loop)
generate_vars_to_read <- function(return_vars, changed_dims, first_found_file, common_return_vars,
                                  common_first_found_file, i) {
  vars_to_read <- NULL
  if (length(return_vars) > 0) {
    #NOTE: because return_vars has changed 'dat' to character(0) above (line 1775),
    #      'dat' won't be included in vars_to_read here.
    vars_to_read <- names(return_vars)[sapply(return_vars, function(x) any(names(changed_dims) %in% x))]
  }
  if (!is.null(first_found_file)) {
    if (any(!first_found_file)) {
      vars_to_read <- c(vars_to_read, names(first_found_file[which(!first_found_file)]))
    }
  }
  if ((i == 1) && (length(common_return_vars) > 0)) {
    vars_to_read <- c(vars_to_read, names(common_return_vars)[sapply(common_return_vars, function(x) any(names(changed_dims) %in% x))])
  }
  if (!is.null(common_first_found_file)) {
    if (any(!common_first_found_file)) {
      vars_to_read <- c(vars_to_read, names(common_first_found_file[which(!common_first_found_file)]))
    }
  }
  return(vars_to_read)
}

# Find the largest dims length within one dataset.
find_largest_dims_length <- function(selectors_total_list, array_of_files_to_load,
                                     selector_indices_save, dat, expected_inner_dims,
                                     synonims, file_dim_reader) {
  # Open and get all the dims from all the files
  data_dims_all_files <- vector('list', length = length(selectors_total_list))

  for (selectors_kk in 1:length(data_dims_all_files)) {
    file_to_open <- do.call("[", c(list(array_of_files_to_load), 
                         as.list(selector_indices_save[[selectors_kk]])))
    data_dims_all_files[[selectors_kk]] <- try(
      file_dim_reader(file_to_open, NULL, selectors_total_list[[selectors_kk]],
                      lapply(dat[['selectors']][expected_inner_dims], '[[', 1),
                      synonims), silent = TRUE)

  }

  # Remove the missing files (i.e., fail try above)
  if (!identical(which(substr(data_dims_all_files, 1, 5) == 'Error'), integer(0))) {
      tmp <- which(substr(data_dims_all_files, 1, 5) == 'Error')
      data_dims_all_files <- data_dims_all_files[-tmp]
  }

  # Find the longest dimensions from all the files
  largest_data_dims <- rep(0, length(data_dims_all_files[[1]]))

  # The inner dim order may differ among files. Need to align them before
  # find out the largest dim length.
  dim_names_first_file <- names(data_dims_all_files[[1]])
  same_dim_order <-lapply(lapply(data_dims_all_files, names),      
                          identical, dim_names_first_file)
  for (to_fix in which(!unlist(same_dim_order))) {
    data_dims_all_files[[to_fix]] <- data_dims_all_files[[to_fix]][match(dim_names_first_file,
                                       names(data_dims_all_files[[to_fix]]))]
  }

  for (kk in 1:length(data_dims_all_files[[1]])) {
    largest_data_dims[kk] <- max(sapply(data_dims_all_files, '[', kk))
  }
  names(largest_data_dims) <- names(data_dims_all_files[[1]])
  return(list(largest_data_dims = largest_data_dims,
              data_dims_all_files = data_dims_all_files))
}

# Gererate vars_to_transform from picked_vars[[i]] and picked_common_vars
generate_vars_to_transform <- function(vars_to_transform, picked_vars, transform_vars, 
                                       picked_vars_ordered) {
  # In Start(), picked_vars can be picked_vars[[i]] or picked_common_vars
  picked_vars_to_transform <- which(names(picked_vars) %in% transform_vars)
  if (length(picked_vars_to_transform) > 0) {
    picked_vars_to_transform <- names(picked_vars)[picked_vars_to_transform]
    new_vars_to_transform <- picked_vars[picked_vars_to_transform]
    which_are_ordered <- which(!sapply(picked_vars_ordered[picked_vars_to_transform], is.null))
            
    if (length(which_are_ordered) > 0) {    
      tmp <- which(!is.na(match(names(picked_vars_ordered), names(which_are_ordered))))
      new_vars_to_transform[which_are_ordered] <- picked_vars_ordered[tmp]
    }
    vars_to_transform <- c(vars_to_transform, new_vars_to_transform)
  }
  return(vars_to_transform)
}

# Turn indices to values for transform_crop_domain
generate_transform_crop_domain_values <- function(transform_crop_domain, 
                                                  picked_vars, 
                                                  transform_var) {
  if (any(transform_crop_domain == 'all')) {
    if (transform_var %in% .KnownLatNames()) {
      transform_crop_domain <- c(-90, 90)
    } else if (transform_var %in% .KnownLonNames()) {
      if (any(picked_vars > 180)) {
        transform_crop_domain <- c(0, 360)
      } else {
        transform_crop_domain <- c(-180, 180)
      }
    } else {
      transform_crop_domain <- c(picked_vars[1], tail(picked_vars, 1))
    }
  } else {  # indices()
    if (is.list(transform_crop_domain)) {
      transform_crop_domain <- picked_vars[unlist(transform_crop_domain)]
    } else {  # vector
      transform_crop_domain <-
        c(picked_vars[transform_crop_domain[1]],
          picked_vars[tail(transform_crop_domain, 1)])
    }
  }
  return(transform_crop_domain)
}

# Out-of-range warning
show_out_of_range_warning <- function(inner_dim, range, bound) {
  # bound: 'lower' or 'upper'
  .warning(paste0("The ", bound, " boundary of selector of ", inner_dim,
                  " is out of range [", min(range), ", ", max(range), "]. ",
                  "Check if the desired range is all included."))
}

# 'sub_sub_array_of_values' is for sri chunking. If this inner dim is chunked,
# the sri has to follow the chunking of fri. Therefore, we save the original 
# value of this chunk here for later use. We'll find the corresponding 
# transformed value within 'sub_sub_array_of_values' and chunk sri. This
# function also returns 'previous_sub_subarray_of_values', which is used for
# checking if there is sri being skipped.
generate_sub_sub_array_of_values <- function(input_array_of_values, sub_array_of_indices,
                                             number_of_chunk) {
  previous_sub_sub_array_of_values <- NULL

  if (is.list(sub_array_of_indices)) {
    sub_sub_array_of_values <- list(input_array_of_values[sub_array_of_indices[[1]]],
                                    input_array_of_values[sub_array_of_indices[[2]]])
    if (number_of_chunk > 1) {
      if (diff(unlist(sub_array_of_indices)) > 0) {
        previous_sub_sub_array_of_values <- 
          input_array_of_values[sub_array_of_indices[[1]] - 1]
      } else {
        previous_sub_sub_array_of_values <-
          input_array_of_values[sub_array_of_indices[[1]] + 1]
      }
    }
  } else {  # is vector
    sub_sub_array_of_values <- input_array_of_values[sub_array_of_indices]
    if (number_of_chunk > 1) {
      if (diff(sub_array_of_indices[1:2]) > 0) {
        previous_sub_sub_array_of_values <- 
          input_array_of_values[sub_array_of_indices[1] - 1]
      } else {
        previous_sub_sub_array_of_values <-
          input_array_of_values[sub_array_of_indices[1] + 1]
      }
    }
  }

  return(list(sub_sub_array_of_values = sub_sub_array_of_values,
              previous_sub_sub_array_of_values = previous_sub_sub_array_of_values))
}


# Generate sub_array_of_fri 
generate_sub_array_of_fri <- function(with_transform, goes_across_prime_meridian, sub_array_of_indices, n, beta,
                                      is_circular_dim, add_beta = TRUE) {
  print_warning <- FALSE
  if (goes_across_prime_meridian) {
    #NOTE: The potential problem here is, if it is global longitude,
    #      and the indices overlap (e.g., lon = [0, 359.723] and 
    #      CircularSort(-180, 180), then sub_array_of_indices = list(649, 649)). 
    #      Therefore, sub_array_of_fri will be c(1:649, 649:1296). We'll get two 649.
    #      The fix below may not be the best solution, but it works for the example above.

    if (sub_array_of_indices[[1]] == sub_array_of_indices[[2]]) {
      # global longitude
      sub_array_of_fri <- 1:n  # n = prod(dim(var_with_selectors))

      if (with_transform & beta != 0 & add_beta) {
        # Warning if transform_extra_cell != 0
        print_warning <- TRUE
      }

    } else {
      # normal case, i.e., not global
      first_index <- min(unlist(sub_array_of_indices))
      last_index <- max(unlist(sub_array_of_indices))
      if (with_transform & add_beta) {
        gap_width <- last_index - first_index - 1
        actual_beta <- min(gap_width, beta)
        sub_array_of_fri <- c(1:(first_index + actual_beta),                                      
                              (last_index - actual_beta):n)
        if (actual_beta != beta) {
          print_warning <- TRUE
        }
      } else {
        sub_array_of_fri <- c(1:first_index, last_index:n)
      }
    }

  } else {
    #NOTE: This if seems redundant.
#    if (is.list(sub_array_of_indices)) {
#      sub_array_of_indices <- sub_array_of_indices[[1]]:sub_array_of_indices[[2]]
#    }
    #NOTE: sub_array_of_indices may be vector or list
    if (with_transform & add_beta) {
      first_index <- min(unlist(sub_array_of_indices))
      last_index <- max(unlist(sub_array_of_indices))
      start_padding <- min(beta, first_index - 1)
      end_padding <- min(beta, n - last_index)
  
      if (!is_circular_dim) {  #latitude or when <var>_reorder is not used
        sub_array_of_fri <- (first_index - start_padding):(last_index + end_padding)
        if (start_padding != beta | end_padding != beta) {
          print_warning <- TRUE
        }
      } else {  #longitude
        if (start_padding == beta & end_padding == beta) {
          # normal regional situation
          sub_array_of_fri <- (first_index - start_padding):(last_index + end_padding)
        } else if (start_padding < beta & end_padding < beta) {
          # global
          sub_array_of_fri <- 1:n
        } else if (start_padding < beta) {
          # left side too close to border, need to go to right side
          sub_array_of_fri <- c((first_index - start_padding):(last_index + end_padding), (n - (beta - start_padding - 1)):n)
          sub_array_of_fri <- unique(sub_array_of_fri)
        } else if (end_padding < beta) {
          # right side too close to border, need to go to left side
          sub_array_of_fri <- c(1: (beta - end_padding), (first_index - start_padding):(last_index + end_padding))
          sub_array_of_fri <- unique(sub_array_of_fri)
        }
      }

    } else {
      if (is.list(sub_array_of_indices)) {
        sub_array_of_fri <- sub_array_of_indices[[1]]:sub_array_of_indices[[2]]
      } else {
        sub_array_of_fri <- sub_array_of_indices
      }
    }
  }
  
  if (print_warning) {
    .warning(paste0("Adding parameter transform_extra_cells =  ", beta,
                    " to the transformed index excesses ",
                    "the border. The border index is used for transformation."))
  }

  return(sub_array_of_fri)
}

# This function merges two dimensions (e.g., time and sdate if "time_across = 'sdate'") into one. 
# The two dimensions have to be next to each other. In Start(), it is used to reshape 
# final_dims_fake if merge_across_dims = TRUE
dims_merge <- function(inner_dims_across_files, final_dims_fake) {
  # inner_dims_across_files would be like: $sdate: "time"
  for (file_dim_across in names(inner_dims_across_files)) {
    inner_dim_pos <- which(names(final_dims_fake) == inner_dims_across_files[[file_dim_across]])
    new_dims <- c()
    # part 1: Put the dims before 'time' in new_dims
    if (inner_dim_pos > 1) {
      new_dims <- c(new_dims, final_dims_fake[1:(inner_dim_pos - 1)])
    }
    # part 2: Merge time and sdate together, and name this dim as 'time'
    # The cross and being crossed dims are next to each other, e.g., [time, sdate]
    new_dims <- c(new_dims, setNames(prod(final_dims_fake[c(inner_dim_pos, inner_dim_pos + 1)]), 
                                     inner_dims_across_files[[file_dim_across]]))
    # part 3: Put the dimes after 'sdate' in new_dims
    if (inner_dim_pos + 1 < length(final_dims_fake)) {
      new_dims <- c(new_dims, final_dims_fake[(inner_dim_pos + 2):length(final_dims_fake)])
    }
    final_dims_fake <- new_dims
  }
  return(final_dims_fake)
}

# This function splits one dimension into two. In Start(), it is used to reshape final_dims_fake
# if split_multiselected_dims = TRUE. 
dims_split <- function(dim_params, final_dims_fake) {
  all_split_dims <- NULL
  for (dim_param in 1:length(dim_params)) {
    split_dims <- dim(dim_params[[dim_param]])
    if (!is.null(split_dims)) {
      if (length(split_dims) > 1) {
        all_split_dims <- c(all_split_dims, setNames(list(split_dims), 
                                                     names(dim_params)[dim_param]))
        if (is.null(names(split_dims))) {
          names(split_dims) <- paste0(names(dim_params)[dim_param], 
                                      1:length(split_dims))
        }
        old_dim_pos <- which(names(final_dims_fake) == names(dim_params)[dim_param])

        # If merge_across_dims and split_multiselected_dims are both used,
        # on one file dim, and this file dim is multi-dim, it doesn't work.
        if (identical(old_dim_pos, integer(0))) {
          stop(paste0("The dimension '", names(dim_params)[dim_param], 
                      "' to be split cannot be found after 'merge_across_dims' ",
                      "is used. Check if the reshape parameters are used appropriately."))
        }
 
        # NOTE: Three steps to create new dims.
        # 1st: Put in the dims before split_dim.
        # 2nd: Replace the old_dim with split_dims.
        # 3rd: Put in the dims after split_dim.
        new_dims <- c()
        if (old_dim_pos > 1) {
          new_dims <- c(new_dims, final_dims_fake[1:(old_dim_pos - 1)])
        }
        new_dims <- c(new_dims, split_dims)
        if (old_dim_pos < length(final_dims_fake)) {
          new_dims <- c(new_dims, final_dims_fake[(old_dim_pos + 1):length(final_dims_fake)])
        }
        final_dims_fake <- new_dims
      }
    }
  }
  return(list(final_dims_fake, all_split_dims))
}


# This function sums up the length of all the inner across dim (e.g., time: list(31, 29, 31, 30))
# and use it to replace the value of that inner dim. That is, it returns the actual length of 
# time rather than using the one including NAs. In Start(), it is used to reshape final_dims_fake
# if merge_across_dims = TRUE, merge_across_dims_narm = TRUE, and split_multiselected_dims = FALSE. 
merge_narm_dims <- function(final_dims_fake, across_inner_dim, length_inner_across_dim) {
  final_dims_fake_name <- names(final_dims_fake)
  pos_across_inner_dim <- which(final_dims_fake_name == across_inner_dim)
  new_length_inner_dim <- sum(unlist(length_inner_across_dim))
  if (pos_across_inner_dim != length(final_dims_fake)) {
    final_dims_fake <- c(final_dims_fake[1:(pos_across_inner_dim - 1)],
                         new_length_inner_dim,
                         final_dims_fake[(pos_across_inner_dim + 1):length(final_dims_fake)])
  } else {
    final_dims_fake <- c(final_dims_fake[1:(pos_across_inner_dim - 1)],
                         new_length_inner_dim)
  }
  names(final_dims_fake) <- final_dims_fake_name
  return(final_dims_fake)
}



# Adjust the dim order. If split_multiselected_dims + merge_across_dims, the dim order may 
# need to be changed. The inner_dim needs to be the first dim among split dims.
reorder_split_dims <- function(all_split_dims, inner_dim_pos_in_split_dims, final_dims_fake) {
  all_split_dims <- c(all_split_dims[inner_dim_pos_in_split_dims],
                      all_split_dims[1:length(all_split_dims)][-inner_dim_pos_in_split_dims])
  split_dims_pos <- which(!is.na(match(names(final_dims_fake), names(all_split_dims))))
  new_dims <- c()
  if (split_dims_pos[1] != 1) {
    new_dims <- c(new_dims, final_dims_fake[1:(split_dims_pos[1] - 1)])
  }
  new_dims <- c(new_dims, all_split_dims)
  if (split_dims_pos[length(split_dims_pos)] < length(final_dims_fake)) {
    new_dims <- c(new_dims, final_dims_fake[(split_dims_pos[length(split_dims_pos)] + 1):length(final_dims_fake)])
  }
  final_dims_fake <- new_dims

  return(list(final_dims_fake, all_split_dims))
}

# Find the final_dims_fake for metadata if it needs to be reshaped
find_final_dims_fake_metadata <- function(merge_across_dims, split_multiselected_dims,
                                          picked_common_vars, across_inner_dim, final_dims_fake,
                                          dims_of_merge_dim, all_split_dims) {
  if (merge_across_dims) {
    if (!split_multiselected_dims) {
      final_dims_fake_metadata <- final_dims_fake[names(final_dims_fake) %in% names(dims_of_merge_dim)]
    } else {
      final_dims_fake_metadata <- final_dims_fake[names(final_dims_fake) %in% names(all_split_dims[[across_inner_dim]])]
    }
  } else if (split_multiselected_dims) {
    target_split_dim_ind <- which(names(dim(picked_common_vars)) == names(all_split_dims))
    margin_dim_ind <- c(1:length(dim(picked_common_vars)))[-target_split_dim_ind]
    if (identical(margin_dim_ind, numeric(0)) | identical(margin_dim_ind, integer(0))) {
      final_dims_fake_metadata <- all_split_dims[[1]]
    } else {
      final_dims_fake_metadata <- .ReplaceElementInVector(dim(picked_common_vars), target = names(all_split_dims), new_val = all_split_dims[[1]])
    }
  }
  
  return(final_dims_fake_metadata)
}

# Build the work pieces.
build_work_pieces <- function(work_pieces, i, selectors, file_dims, inner_dims, final_dims, 
                              found_pattern_dim, inner_dims_across_files, array_of_files_to_load,
                              array_of_not_found_files, array_of_metadata_flags,
                              metadata_file_counter, depending_file_dims, transform, 
                              transform_vars, picked_vars, picked_vars_ordered, picked_common_vars,
                              picked_common_vars_ordered, metadata_folder, debug = debug) {
  sub_array_dims <- final_dims[file_dims]
  sub_array_dims[found_pattern_dim] <- 1
  sub_array_of_files_to_load <- array(1:prod(sub_array_dims), 
                                      dim = sub_array_dims)
  names(dim(sub_array_of_files_to_load)) <- names(sub_array_dims)
  # Detect which of the dimensions of the dataset go across files.
  file_dim_across_files <- lapply(inner_dims, 
                                  function(x) {
                                    dim_across <- sapply(inner_dims_across_files, function(y) x %in% y)
                                    if (any(dim_across)) {
                                      names(inner_dims_across_files)[which(dim_across)[1]]
                                    } else {
                                      NULL
                                    }
                                  })
  names(file_dim_across_files) <- inner_dims
  j <- 1
  while (j <= prod(sub_array_dims)) {
    # Work out file path.
    file_to_load_sub_indices <- which(sub_array_of_files_to_load == j, arr.ind = TRUE)[1, ]
    names(file_to_load_sub_indices) <- names(sub_array_dims)
    file_to_load_sub_indices[found_pattern_dim] <- i
    big_dims <- rep(1, length(dim(array_of_files_to_load)))
    names(big_dims) <- names(dim(array_of_files_to_load))
    file_to_load_indices <- .MergeArrayDims(file_to_load_sub_indices, big_dims)[[1]]
    file_to_load <- do.call('[[', c(list(array_of_files_to_load), 
                            as.list(file_to_load_indices)))
    not_found_file <- do.call('[[', c(list(array_of_not_found_files),
                              as.list(file_to_load_indices)))
    load_file_metadata <- do.call('[', c(list(array_of_metadata_flags), 
                                  as.list(file_to_load_indices)))
    if (load_file_metadata) {
       metadata_file_counter <- metadata_file_counter + 1
       assign('metadata_file_counter', metadata_file_counter, envir = parent.frame())
    }
    if (!is.na(file_to_load) && !not_found_file) {
      # Work out indices to take
      first_round_indices <- lapply(inner_dims, 
                                    function (x) {
                                      if (is.null(file_dim_across_files[[x]])) {
                                        x_dim_name <- attr(attr(selectors[[x]][['fri']], "dim"), "names")
                                        if (!is.null(x_dim_name)) {
                                          which_chunk <- file_to_load_sub_indices[x_dim_name]
                                         if (length(which_chunk) > 1) {
                                            tmp_dim <- attr(selectors[[x]][['fri']], "dim")
                                            vec_ind <- which_chunk[1]
                                            for (i_dim in length(tmp_dim):2) {
                                              vec_ind <- vec_ind + (which_chunk[i_dim] - 1) * prod(tmp_dim[1:(i_dim - 1)])
                                            }
                                            selectors[[x]][['fri']][[vec_ind]]
                                          } else {  #old code
                                            selectors[[x]][['fri']][[which_chunk]]
                                          }
                                        } else {
                                          selectors[[x]][['fri']][[1]]
                                        }
                                      } else {
                                        which_chunk <- file_to_load_sub_indices[file_dim_across_files[[x]]] 
                                        selectors[[x]][['fri']][[which_chunk]]
                                      }
                                    })
      names(first_round_indices) <- inner_dims
      second_round_indices <- lapply(inner_dims, 
                                     function (x) {
                                       if (is.null(file_dim_across_files[[x]])) {
                                         x_dim_name <- attr(attr(selectors[[x]][['sri']], "dim"), "names")
                                         if (!is.null(x_dim_name)) {
                                           which_chunk <- file_to_load_sub_indices[x_dim_name]
                                          if (length(which_chunk) > 1) {
                                            tmp_dim <- attr(selectors[[x]][['sri']], "dim")
                                            vec_ind <- which_chunk[1]
                                            for (i_dim in length(tmp_dim):2) {
                                              vec_ind <- vec_ind + (which_chunk[i_dim] - 1) * prod(tmp_dim[1:(i_dim - 1)])
                                            }
                                            selectors[[x]][['sri']][[vec_ind]]
                                          } else {  #old code
                                             selectors[[x]][['sri']][[which_chunk]]
                                          }
                                         } else {
                                           selectors[[x]][['sri']][[1]]
                                         }
                                       } else {
                                         which_chunk <- file_to_load_sub_indices[file_dim_across_files[[x]]]
                                         selectors[[x]][['sri']][[which_chunk]]
                                       }
                                     })
      if (debug) {
        print("-> BUILDING A WORK PIECE")
        #print(str(selectors))
      }
      names(second_round_indices) <- inner_dims
      if (!any(sapply(first_round_indices, length) == 0)) {
        work_piece <- list()
        work_piece[['first_round_indices']] <- first_round_indices
        work_piece[['second_round_indices']] <- second_round_indices
        work_piece[['file_indices_in_array_of_files']] <- file_to_load_indices
        work_piece[['file_path']] <- file_to_load
        work_piece[['store_dims']] <- final_dims
        # Work out store position
        store_position <- final_dims
        store_position[names(file_to_load_indices)] <- file_to_load_indices
        store_position[inner_dims] <- rep(1, length(inner_dims))
        work_piece[['store_position']] <- store_position
        # Work out file selectors
        file_selectors <- sapply(file_dims, 
                                 function (x) {
                                   vector_to_pick <- 1
                                   if (x %in% names(depending_file_dims)) {
                                     vector_to_pick <- file_to_load_indices[depending_file_dims[[x]]]
                                   }
                                   if (x != found_pattern_dim) {
                                     selectors[[x]][[vector_to_pick]][file_to_load_indices[x]]
                                   } else {
                                     # dat_dim only has one value in each work_piece
                                     selectors[[x]][[vector_to_pick]]
                                   }
                                 })
        names(file_selectors) <- file_dims
        work_piece[['file_selectors']] <- file_selectors
        # Send variables for transformation
        if (!is.null(transform) && (length(transform_vars) > 0)) {
          vars_to_transform <- NULL
          picked_vars_to_transform <- which(names(picked_vars) %in% transform_vars)
          if (length(picked_vars_to_transform) > 0) {
            picked_vars_to_transform <- names(picked_vars)[picked_vars_to_transform]
            vars_to_transform <- c(vars_to_transform, picked_vars[picked_vars_to_transform])
            if (any(picked_vars_to_transform %in% names(picked_vars_ordered))) {
              picked_vars_ordered_to_transform <- picked_vars_to_transform[which(picked_vars_to_transform %in% names(picked_vars_ordered))]
              vars_to_transform[picked_vars_ordered_to_transform] <- picked_vars_ordered[picked_vars_ordered_to_transform]
            }
          }
          picked_common_vars_to_transform <- which(names(picked_common_vars) %in% transform_vars)
          if (length(picked_common_vars_to_transform) > 0) {
            picked_common_vars_to_transform <- names(picked_common_vars)[picked_common_vars_to_transform]
            vars_to_transform <- c(vars_to_transform, picked_common_vars[picked_common_vars_to_transform])
            if (any(picked_common_vars_to_transform %in% names(picked_common_vars_ordered))) {
              picked_common_vars_ordered_to_transform <- picked_common_vars_to_transform[which(picked_common_vars_to_transform %in% names(picked_common_vars_ordered))]
              vars_to_transform[picked_common_vars_ordered_to_transform] <- picked_common_vars_ordered[picked_common_vars_ordered_to_transform]
            }
          }
          work_piece[['vars_to_transform']] <- vars_to_transform
        }
        # Send flag to load metadata
        if (load_file_metadata) {
          work_piece[['save_metadata_in']] <- paste0(metadata_folder, '/', metadata_file_counter)
        }
        work_pieces <- c(work_pieces, list(work_piece))
      }
    }
    j <- j + 1
  }
  return(work_pieces)
}

# Calculate the progress %s that will be displayed and assign them to the appropriate work pieces.
retrieve_progress_message <- function(work_pieces, num_procs, silent) {
  if (length(work_pieces) / num_procs >= 2 && !silent) {
    if (length(work_pieces) / num_procs < 10) {
      amount <- 100 / ceiling(length(work_pieces) / num_procs)
      reps <- ceiling(length(work_pieces) / num_procs)
    } else {
      amount <- 10
      reps <- 10
    }
    progress_steps <- rep(amount, reps)
    if (length(work_pieces) < (reps + 1)) {
      selected_pieces <- length(work_pieces)
      progress_steps <- c(sum(head(progress_steps, reps)),
                          tail(progress_steps, reps))
    } else {
      selected_pieces <- round(seq(1, length(work_pieces), 
                                   length.out = reps + 1))[-1]
    }
    progress_steps <- paste0(' + ', round(progress_steps, 2), '%')
    progress_message <- 'Progress: 0%'
  } else {
    progress_message <- ''
    selected_pieces <- NULL
  }
  piece_counter <- 1
  step_counter <- 1
  work_pieces <- lapply(work_pieces, 
                        function (x) {
                          if (piece_counter %in% selected_pieces) {
                            wp <- c(x, list(progress_amount = progress_steps[step_counter]))
                            step_counter <<- step_counter + 1
                          } else {
                            wp <- x
                          }
                          piece_counter <<- piece_counter + 1
                          wp
                        })
  if (!silent) {
    .message("If the size of the requested data is close to or above the free shared RAM memory, R may crash.")
    .message("If the size of the requested data is close to or above the half of the free RAM memory, R may crash.")
    .message(paste0("Will now proceed to read and process ", length(work_pieces), " data files:"))
    if (length(work_pieces) < 30) {
      lapply(work_pieces, function (x) .message(x[['file_path']], indent = 2))
    } else {
      .message("The list of files is long. You can check it after Start() finishes in the output '$Files'.", indent = 2, exdent = 5)
    }
  }
    
  # Build the cluster of processes that will do the work and dispatch work pieces.
  # The function .LoadDataFile is applied to each work piece. This function will
  # open the data file, regrid if needed, subset, apply the mask, 
  # compute and apply the weights if needed,
  # disable extreme values and store in the shared memory matrix.
  #print("O")
  if (!silent) {
    .message("Loading... This may take several minutes...")
    if (progress_message != '') {
      .message(progress_message, appendLF = FALSE)
    }
  }
  return(work_pieces)
}

# If merge_across_dims = TRUE and merge_across_dims_narm = TRUE, remove the additional NAs 
# due to unequal inner_dim ('time') length across file_dim ('sdate').
remove_additional_na_from_merge <- function(data_array = NULL, merge_dim_metadata = NULL,
                                            inner_dims_across_files, final_dims, length_inner_across_dim) {
  # data_array is a vector from bigmemory::as.matrix
  # merge_dim_metadata is an array

  across_file_dim <- names(inner_dims_across_files)  #TODO: more than one?
  across_inner_dim <- inner_dims_across_files[[1]]  #TODO: more than one?
  # Get the length of these two dimensions in final_dims
  length_inner_across_store_dims <- final_dims[across_inner_dim]
  length_file_across_store_dims <- final_dims[across_file_dim]
       
  # Create a logical array for merge_across_dims
  logi_array <- array(rep(FALSE,
                          length_file_across_store_dims * length_inner_across_store_dims),
                      dim = c(length_inner_across_store_dims, length_file_across_store_dims))
  for (i in 1:length_file_across_store_dims) {  #1:4
    logi_array[1:length_inner_across_dim[[i]], i] <- TRUE
  }
 
  if (!is.null(data_array)) {
    # First, turn the data vector into array with final_dims
    data_array_final_dims <- array(data_array, dim = final_dims)
  }

  # Change the NA derived from additional spaces to -9999, then remove these -9999
  func_remove_blank <- function(data_array, logi_array) {
    # dim(data_array) = [time, file_date]
    # dim(logi_array) = [time, file_date]
    # data_array can be data or metadata; if data, change the blank spaces from 
    # NA to -9999; if metadata (supposed to be 'time'), change the corresponding
    # spaces to -12^10.
    if (is(data_array, "POSIXct")) {
      # change to numeric first
      data_array <- array(as.vector(data_array), dim = dim(data_array))
      data_array[which(!logi_array)] <- -12^10
    } else {
      data_array[which(!logi_array)] <- -9999
    }
    return(data_array)
  }

  if (!is.null(data_array)) {
    data_array_final_dims <- multiApply::Apply(data_array_final_dims,
                                               target_dims = c(across_inner_dim, across_file_dim),  #c('time', 'file_date')
                                               output_dims = c(across_inner_dim, across_file_dim),
                                               fun = func_remove_blank,
                                               logi_array = logi_array)$output1
  }
  if (!is.null(merge_dim_metadata)) {
    tmp_attr <- attributes(merge_dim_metadata)$variables
    merge_dim_metadata <- multiApply::Apply(merge_dim_metadata,
                            target_dims = c(across_inner_dim, across_file_dim),
                            output_dims = c(across_inner_dim, across_file_dim),
                            fun = func_remove_blank,
                            logi_array = logi_array)$output1
  }

  if (!is.null(data_array)) {
    ## reorder back to the correct dim
    tmp <- match(names(final_dims), names(dim(data_array_final_dims)))
    data_array_final_dims <- .aperm2(data_array_final_dims, tmp)
    data_array_tmp <- data_array_final_dims[data_array_final_dims != -9999]  # become a vector
  } else {
    data_array_tmp <- NULL
  }
  if (!is.null(merge_dim_metadata)) {
    # Reorder metadata dim as final dim
    tmp <- match(names(final_dims), names(dim(merge_dim_metadata)))
    merge_dim_metadata <- aperm(merge_dim_metadata, tmp[!is.na(tmp)])
    merge_dim_metadata <- merge_dim_metadata[merge_dim_metadata != -12^10]
    attr(merge_dim_metadata, 'variables') <- tmp_attr
  }

  #NOTE: both outputs are vectors. If 'merge_dim_metadata' is actually time, it is just numeric here.
  return(list(data_array = data_array_tmp, merge_dim_metadata = merge_dim_metadata))
}


# When merge_across_dims = TRUE and split_multiselected_dims = TRUE, rearrange the chunks 
# (i.e., work_piece) is necessary if one file contains values for discrete dimensions
rebuild_array_merge_split <- function(data_array = NULL, metadata = NULL, indices_chunk,
                                      all_split_dims, final_dims_fake, across_inner_dim, length_inner_across_dim) {

  rebuild_data <- ifelse(is.null(data_array), FALSE, TRUE)
  rebuild_metadata <- ifelse(is.null(metadata), FALSE, TRUE)

  # generate the correct order list from indices_chunk 
  final_order_list <- list()
  i <- 1
  j <- 1
  a <- indices_chunk[i]
  while (i <= length(indices_chunk)) {
    while (indices_chunk[i+1] == indices_chunk[i] & i < length(indices_chunk)) {
      a <- c(a, indices_chunk[i+1])
        i <- i + 1
    }
    final_order_list[[j]] <- a
    a <- indices_chunk[i+1]
    i <- i + 1
    j <- j + 1
  }
  names(final_order_list) <- sapply(final_order_list, '[[', 1)
  final_order_list <- lapply(final_order_list, length)
        
  if (!all(diff(as.numeric(names(final_order_list))) > 0)) {
    # shape the vector into the array without split_dims
    split_dims_pos <- match(names(all_split_dims[[1]]), names(final_dims_fake))
    new_dims <- c()
    if (split_dims_pos[1] > 1) {
      new_dims <- c(new_dims, final_dims_fake[1:(split_dims_pos[1] - 1)])
    }
    new_dims <- c(new_dims,  prod(all_split_dims[[1]]))
    names(new_dims)[split_dims_pos[1]] <- across_inner_dim
    if (split_dims_pos[length(split_dims_pos)] < length(final_dims_fake)) {
      new_dims <- c(new_dims, final_dims_fake[(split_dims_pos[length(split_dims_pos)] + 1):length(final_dims_fake)])
    }

    if (rebuild_data) {
      data_array <- array(data_array, dim = new_dims)
      # seperate 'time' dim into each work_piece length
      data_array_seperate <- vector('list', length = length(length_inner_across_dim))
      array_piece <- vector('list', length = length(final_order_list))
    }
    if (rebuild_metadata) {
      metadata <- array(metadata, dim = length(metadata)) #metadata_no_split
      names(dim(metadata)) <- across_inner_dim
      metadata_seperate <- vector('list', length = length(length_inner_across_dim))
      metadata_piece <- vector('list', length = length(final_order_list))
    }

    tmp <- cumsum(unlist(length_inner_across_dim))
    tmp <- c(0, tmp)
    for (i in 1:length(length_inner_across_dim)) {
      if (rebuild_data) {
        data_array_seperate[[i]] <- ClimProjDiags::Subset(data_array,
                                                          across_inner_dim,
                                                          (tmp[i] + 1):tmp[i + 1])
      }
      if (rebuild_metadata) {
        metadata_seperate[[i]] <- ClimProjDiags::Subset(metadata,
                                                        across_inner_dim,
                                                        (tmp[i] + 1):tmp[i + 1])
      }
    }

    # re-build the array: chunk 
    which_chunk <- as.numeric(names(final_order_list))
    sort_which_chunk <- sort(unique(which_chunk))
    which_chunk <- sapply(lapply(which_chunk, '==', sort_which_chunk), which)
    how_many_indices <- unlist(final_order_list)

    if (rebuild_data) {
      ind_in_array_seperate <- as.list(rep(1, length(data_array_seperate)))
    } else if (rebuild_metadata) {
      ind_in_array_seperate <- as.list(rep(1, length(metadata_seperate)))
    }

    for (i in 1:length(final_order_list)) {
      if (rebuild_data) {
        array_piece[[i]] <- ClimProjDiags::Subset(
                              data_array_seperate[[which_chunk[i]]], across_inner_dim,
                              ind_in_array_seperate[[which_chunk[i]]]:(ind_in_array_seperate[[which_chunk[i]]] + how_many_indices[i] - 1))
      }
      if (rebuild_metadata) {
        metadata_piece[[i]] <- ClimProjDiags::Subset(
                                 metadata_seperate[[which_chunk[i]]], across_inner_dim,
                                 ind_in_array_seperate[[which_chunk[i]]]:(ind_in_array_seperate[[which_chunk[i]]] + how_many_indices[i] - 1))
      }
      ind_in_array_seperate[[which_chunk[i]]] <- ind_in_array_seperate[[which_chunk[i]]] + how_many_indices[i]
    }

    # re-build the array: paste
    if (rebuild_data) {
      data_array_tmp <- array_piece[[1]]
    } else {
      data_array_tmp <- NULL
    }
    if (rebuild_metadata) {
      metadata_tmp <- metadata_piece[[1]]
    } else {
      metadata_tmp <- NULL
    }

    if (rebuild_data) {
      along_pos <- which(names(dim(data_array_tmp)) == across_inner_dim)
      length_piece <- length(array_piece)
    } 
    if (rebuild_metadata) {
      along_pos_metadata <- which(names(dim(metadata_tmp)) == across_inner_dim)
      if (!rebuild_data)
        length_piece <- length(metadata_piece)
    }

    if (length_piece > 1) {
      for (i in 2:length_piece) {
        if (rebuild_data) {
          data_array_tmp <- abind::abind(data_array_tmp, array_piece[[i]],
                                         along = along_pos)
        }
        if (rebuild_metadata) {
          metadata_tmp <- abind::abind(metadata_tmp, metadata_piece[[i]],
                                       along = along_pos_metadata)
        }
      }
    }
  } else {
    data_array_tmp <- data_array
    metadata_tmp <- metadata
  }

  return(list(data_array = data_array_tmp, metadata = metadata_tmp))
}


# Create a list of metadata of the variable (e.g., tas)
create_metadata_list <- function(array_of_metadata_flags, metadata_dims, pattern_dims,
                                 loaded_metadata_files, loaded_metadata, dat_names,
                                 dataset_has_files) {
  #NOTE: Here, metadata can be saved in one of two ways: one for $common and the other for $dat
  #      for $common, it is a list of metadata length. For $dat, it is a list of dat length,
  #      and each sublist has the metadata for each dat.
  dim_of_metadata <- dim(array_of_metadata_flags)[metadata_dims]
  if (!any(names(dim_of_metadata) == pattern_dims) |
      (any(names(dim_of_metadata) == pattern_dims) &
       dim_of_metadata[pattern_dims] == 1)) {  # put under $common; old code
    return_metadata <- vector('list',
                              length = prod(dim_of_metadata))
    return_metadata[as.numeric(loaded_metadata_files)] <- loaded_metadata
    dim(return_metadata) <- dim_of_metadata

  } else { # put under $dat. metadata_dims has 'dat' and dat length > 1
    return_metadata <- vector('list',
                              length = dim_of_metadata[pattern_dims])
    names(return_metadata) <- dat_names
    for (kk in 1:length(return_metadata)) {
      return_metadata[[kk]] <- vector('list', length = prod(dim_of_metadata[-1])) # 1 is dat
    }
    loaded_metadata_count <- 1
    for (kk in 1:length(return_metadata)) {
      for (jj in 1:length(return_metadata[[kk]])) {
        if (dataset_has_files[kk]) {
          if (loaded_metadata_count %in% loaded_metadata_files) {
            return_metadata[[kk]][jj] <- loaded_metadata[[which(loaded_metadata_files == loaded_metadata_count)]]
            names(return_metadata[[kk]])[jj] <- names(loaded_metadata[[which(loaded_metadata_files == loaded_metadata_count)]])

          } else {
            return_metadata[[kk]][jj] <- NULL
          }
          loaded_metadata_count <- loaded_metadata_count + 1
        } else {
          return_metadata[[kk]][jj] <- NULL
        }

      }
    }
  }

  return(return_metadata)
}

# This function adds the metadata of the variable (e.g., tas) into the list of picked_vars or
# picked_common_vars. The metadata is only retrieved when 'retrieve = TRUE'.
combine_metadata_picked_vars <- function(return_metadata, picked_vars, picked_common_vars,
                                         metadata_dims, pattern_dims, length_dat) {
#NOTE: The metadata of variables can be saved in one of the two different structures.
#      (1) metadata_dims != 'dat', or (metadata_dims == 'dat' & length(dat) == 1):
#          put under $common
#      (2) (metadata_dims == 'dat' & length(dat) > 1):
#          put under $dat1, $dat2, .... Put it in picked_vars list
#TODO: The current (2) uses the inefficient method. Should define the list structure first
#      then fill the list, rather than expand it in the for loop.

  if (any(metadata_dims == pattern_dims) & length_dat > 1) { # (2)
    for (kk in 1:length(return_metadata)) {
      sublist_names <- lapply(return_metadata, names)[[kk]]
      if (!is.null(sublist_names)) {
        for (jj in 1:length(sublist_names)) {
          if (!is.null(return_metadata[[kk]][[jj]])) {
            picked_vars[[kk]] <- c(picked_vars[[kk]], list(return_metadata[[kk]][[jj]]))
            names(picked_vars[[kk]])[length(picked_vars[[kk]])] <- names(return_metadata[[kk]][jj])
          }
        }
      }
    }
    Variables_list <- c(list(common = picked_common_vars), picked_vars)

  } else {  #(1)
    len <- unlist(lapply(return_metadata, length))
    len <- sum(len) + length(which(len == 0))  #0 means NULL
    name_list <- lapply(return_metadata, names)
    new_list <- vector('list', length = len)
    count <- 1

    for (kk in 1:length(return_metadata)) {
      if (length(return_metadata[[kk]]) == 0) {  #NULL
        count <- count + 1
      } else {
        for (jj in 1:length(return_metadata[[kk]])) {
          new_list[[count]] <- return_metadata[[kk]][[jj]]
          names(new_list)[count] <- name_list[[kk]][jj]
          count <- count + 1
        }
      }
    }
    Variables_list <- c(list(common = c(picked_common_vars, new_list)), picked_vars)
  }

  return(Variables_list)
}

# This function generates a list of 3, containing picked(_common)_vars, 
# picked(_common)_vars_ordered, and picked(_common)_vars_unorder_indices for the 'var_to_read'
# of this dataset (i) and file (j).
generate_picked_var_of_read <- function(var_to_read, var_to_check, array_of_files_to_load,
                                        var_dims, array_of_var_files, file_var_reader,
                                        file_object, synonims, associated_dim_name,
                                        dim_reorder_params, aiat, current_indices, var_params,
                                        either_picked_vars,
                                        either_picked_vars_ordered,
                                        either_picked_vars_unorder_indices) {
  var_file_dims <- NULL

  if (any(names(dim(array_of_files_to_load)) %in% var_to_check)) {
    var_file_dims <- dim(array_of_files_to_load)[which(names(dim(array_of_files_to_load)) %in%
                                                       var_to_check)]
  }
  if (is.null(either_picked_vars)) {

    if (any(names(var_file_dims) %in% names(var_dims))) {
      stop("Found a requested var in 'return_var' requested for a ",
           "file dimension which also appears in the dimensions of ",
           "the variable inside the file.\n", array_of_var_files)
    }
    first_sample <- file_var_reader(NULL, file_object, NULL,
                                    var_to_read, synonims)
    if (any(class(first_sample) %in% names(time_special_types()))) {
      array_size <- prod(c(var_file_dims, var_dims))
      new_array <- rep(time_special_types()[[class(first_sample)[1]]](NA), array_size)
      dim(new_array) <- c(var_file_dims, var_dims)
    } else {
      new_array <- array(dim = c(var_file_dims, var_dims))
    }
    attr(new_array, 'variables') <- attr(first_sample, 'variables')

    either_picked_vars <- new_array
    pick_ordered <- FALSE
    if (var_to_read %in% unlist(var_params)) {
      if (associated_dim_name %in% names(dim_reorder_params) && !aiat) {
        either_picked_vars_ordered <- new_array
        pick_ordered <- TRUE
      }
    }
    if (!pick_ordered) {
      either_picked_vars_ordered <- NULL
    }

  } else {
    array_var_dims <- dim(either_picked_vars)
    full_array_var_dims <- array_var_dims
    if (any(names(array_var_dims) %in% names(var_file_dims))) {
      array_var_dims <- array_var_dims[-which(names(array_var_dims) %in% names(var_file_dims))]
    }
    if (any(names(array_var_dims) != names(var_dims))) {
      stop("Error while reading the variable '", var_to_read, "' from ",
           "the file. Dimensions do not match.\nExpected ",
           paste(paste0("'", names(array_var_dims), "'"), collapse = ', '),
           " but found ",
           paste(paste0("'", names(var_dims), "'"), collapse = ', '),
           ".\n", array_of_var_files)
    }
    if (any(var_dims > array_var_dims)) {
      longer_dims <- which(var_dims > array_var_dims)
      if (length(longer_dims) == 1) {
        longer_dims_in_full_array <- longer_dims
        if (any(names(full_array_var_dims) %in% names(var_file_dims))) {
          candidates <- (1:length(full_array_var_dims))[-which(names(full_array_var_dims) %in% names(var_file_dims))]
          longer_dims_in_full_array <- candidates[longer_dims]
        }
        padding_dims <- full_array_var_dims
        padding_dims[longer_dims_in_full_array] <-
          var_dims[longer_dims] - array_var_dims[longer_dims]

        var_class <- class(either_picked_vars)
        if (any(var_class %in% names(time_special_types()))) {
          padding_size <- prod(padding_dims)
          padding <- rep(time_special_types()[[var_class[1]]](NA), padding_size)
          dim(padding) <- padding_dims
        } else {
          padding <- array(dim = padding_dims)
        }
        tmp_attr <- attributes(either_picked_vars)$variables
        either_picked_vars <- .abind2(either_picked_vars, padding,
                                      names(full_array_var_dims)[longer_dims_in_full_array])
        attr(either_picked_vars, 'variables') <- tmp_attr

      } else {
        stop("Error while reading the variable '", var_to_read, "' from ",
             "the file. Found size (", paste(var_dims, collapse = ' x '),
             ") is greater than expected maximum size (", array_var_dims, ").")
      }
    }
  }

  var_store_indices <- c(as.list(current_indices[names(var_file_dims)]),
                         lapply(var_dims, function(x) 1:x))
  var_values <- file_var_reader(NULL, file_object, NULL, var_to_read, synonims)
  if (var_to_read %in% unlist(var_params)) {
    if ((associated_dim_name %in% names(dim_reorder_params)) && !aiat) {
      ## Is this check really needed?
      if (length(dim(var_values)) > 1) {
        stop("Requested a '", associated_dim_name, "_reorder' for a dimension ",
             "whose coordinate variable that has more than 1 dimension. This is ",
             "not supported.")
      }
      ordered_var_values <- dim_reorder_params[[associated_dim_name]](var_values)
      attr(ordered_var_values$x, 'variables') <- attr(var_values, 'variables')
      if (!all(c('x', 'ix') %in% names(ordered_var_values))) {
        stop("All the dimension reorder functions must return a list with the components 'x' and 'ix'.")
      }
      # Save the indices to reorder the ordered variable values back to original order.
      # 'unorder' refers to the indices of 'ordered_var_values' if it is unordered.
      # This will be used to define the first round indices.
      unorder <- sort(ordered_var_values$ix, index.return = TRUE)$ix
      either_picked_vars_ordered <- do.call('[<-',
                                            c(list(x = either_picked_vars_ordered),
                                                   var_store_indices,
                                                   list(value = ordered_var_values$x)))
      either_picked_vars_unorder_indices <- do.call('[<-',
                                            c(list(x = either_picked_vars_unorder_indices),
                                                   var_store_indices,
                                                   list(value = unorder)))


    }
  }

  either_picked_vars <- do.call('[<-',
                                c(list(x = either_picked_vars),
                                       var_store_indices,
                                       list(value = var_values)))
  # Turn time zone back to UTC if this var_to_read is 'time'
  if (all(class(either_picked_vars) == names(time_special_types))) {
    attr(either_picked_vars, "tzone") <- 'UTC'
  }


  return(list(either_picked_vars = either_picked_vars,
              either_picked_vars_ordered = either_picked_vars_ordered,
              either_picked_vars_unorder_indices = either_picked_vars_unorder_indices))
}


# Trnasforms a vector of indices v expressed in a world of 
# length N from 1 to N, into a world of length M, from
# 1 to M. Repeated adjacent indices are collapsed.
transform_indices <- function(v, n, m) {
  #unique2 turns e.g. 1 1 2 2 2 3 3 1 1 1 into 1 2 3 1
  unique2 <- function(v) {
    if (length(v) < 2) {
      v
    } else {
      v[c(1, v[2:length(v)] - v[1:(length(v) - 1)]) != 0]
    }
  }
  unique2(round(((v - 1) / (n - 1)) * (m - 1))) + 1 # this rounding may generate 0s. what then?
}

replace_character_with_indices <- function(selectors, data_dims, chunk_amount) {
  if (selectors == 'all') {
    selectors <- indices(1:(data_dims * chunk_amount))
  } else if (selectors == 'first') {
    selectors <- indices(1)
  } else if (selectors == 'last') {
    selectors <- indices(data_dims * chunk_amount)
  }
  return(selectors)
}
