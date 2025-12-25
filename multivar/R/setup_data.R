setup_data <- function (data, standardize, lag, horizon) {
  
   if (is.null(data)){
    stop(paste0(
      "multivar ERROR: neither a data directory nor a data list is specified. ",
      "Please either specify a directory of data or a list of individual data files."
    )
    )
  }
  
  #-------------------------------------------------------------#
  # If the data is already in list form.
  #-------------------------------------------------------------#
  if (!is.list(data)){
      stop(paste0(
        "multivar ERROR: data must be supplied as a list of matrices."
      ))
  } else {
      ts_list  <- list()
  
      # if the user-supplied list does not have names, add names
      if(is.null(names(data))){
  
        names(data) <- paste0("dataset", 1:length(data))
  
      }
  
      ts_list  <- data
  }
  # if (is.list(data)){
  # 
  #   ts_list  <- list()
  # 
  #   # if the user-supplied list does not have names, add names 
  #   if(is.null(names(data))){ 
  #     
  #     names(data) <- paste0("dataset", 1:length(data)) 
  #     
  #   }
  # 
  #   ts_list  <- data
  # 
  # #-------------------------------------------------------------#
  # # If the data is not a list (we assume it is a directory).
  # #-------------------------------------------------------------#  
  # } else if (!is.list(data)){
  # 
  #   files <- list.files(data, full.names = TRUE) 
  #   
  #   #-------------------------------------------------------------#
  #   # Throw errors specific to data specified as a directory.
  #   #-------------------------------------------------------------# 
  #   if (is.null(sep)){
  #     stop(paste0(
  #       "multivar ERROR: a data directory is specified but the sep argument is not. ",
  #       "Please specify a sep argument before continuing."
  #     ))
  #   }
  #   
  #   if (is.null(header)){
  #     stop(paste0(
  #       "multivar ERROR: a data directory is specified but a header argument is not. ",
  #       "Please specify a logical value for header before continuing."
  #     ))
  #   }
  # 
  #   #-------------------------------------------------------------#
  #   # Create a list of dataframes.
  #   #-------------------------------------------------------------# 
  #   
  #   ts_list <- list()
  #   for (i in 1:length(files)){
  #     ts_list[[i]] <- read.table(files[i], sep = ctrlOpts$sep, header = ctrlOpts$header)
  #   }
  #   
  #   names(ts_list) <- tools::file_path_sans_ext(basename(files))
  # 
  # } else {
  # 
  #   stop(paste0(
  #     "multivar ERROR: Format of data argument not recognized. "
  #   ))
  # 
  # }
  
  #-------------------------------------------------------------#
  # Ensure all datafiles share the same column order.
  #
  #   # Now we also make all datasets have the same column order.
  #-------------------------------------------------------------# 
  varnames       <- colnames(ts_list[[1]])
  n_orig_vars    <- ncol(ts_list[[1]])
  
  if(is.null(varnames)){
    varnames <- c(paste0("V", seq(1,n_orig_vars)))
    ts_list <- lapply(ts_list, function(x) { colnames(x) <- varnames;  x })
  } 
  
  
  #-------------------------------------------------------------#
  # Final data manipulation.
  #-------------------------------------------------------------#
  if (ncol(ts_list[[1]]) == 1) {
    stop(paste0("multivar ERROR: only one column of data read in. ",
                "Check if sep argument properly specified."))
  }
  
  

  if(standardize){
    ts_list <- lapply(ts_list, function(df) {scale(df)})
  }

  
  #-------------------------------------------------------------#
  # Final data checks
  #-------------------------------------------------------------#
  
  n_subjects   <- length(ts_list)
  cols         <- numeric()
  missingData  <- numeric()
  constantCols <- logical()
  numericCols  <- logical()
  
  # check for obvious errors in data
  for (k in 1:length(ts_list)){
    data.file <- ts_list[[k]]
    cols[k]   <- ncol(data.file)
    missingData[k]  <- any(is.na(data.file))
    constantCols[k] <- any(apply(data.file, 2, sd, na.rm = TRUE) == 0)
    numericCols[k]  <- any(apply(data.file, 2, is.numeric) == FALSE)
  }
  
  
  # if (n_subjects != 1) {
  #   if (sd(cols) != 0) {
  #     stop(paste0('multivar ERROR: not all data files have the same number of columns. ',
  #                 'Please fix or remove file before continuing.'))
  #   }
  # 
  #   if (any(cols != missingData)) {
  #     stop(paste0('multivar ERROR: missing data not currently supported. '))
  #   }  
  #   
  #   if (any(constantCols == TRUE)){
  #     stop(paste0('multivar ERROR: at least one data file contains a column with constant values. ',
  #                 'Please fix or remove files listed below before continuing. \n', 
  #                 paste0(names(ts_list)[constantCols == TRUE], collapse = "\n")))
  #   }
  #   
  #   if (any(numericCols == TRUE)){
  #     stop(paste0('multivar ERROR: at least one data file contains a column with non-numeric values. ',
  #                 'Please fix or remove files listed below before continuing. \n', 
  #                 paste0(names(ts_list)[numericCols == TRUE], collapse = "\n")))
  #   }
  #   
  # } 
  
  # will need to be updated for lags > 1
  ts_list <- lapply(ts_list, function(df){
    if(horizon > 0){
      H  <- df[((nrow(df)-horizon+1):nrow(df)),, drop = FALSE]
      df <- df[-((nrow(df)-horizon+1):nrow(df)),, drop = FALSE]
    } else {
      H  <- NA
    }
    A  <- Matrix(df[1:(nrow(df)-1), ], sparse = TRUE)
    b  <- Matrix(df[2:(nrow(df)  ), ], sparse = TRUE)
    colnames(A) <- colnames(b) <- colnames(df)
    list(b = b, A = A, H = H)
  })
  
  
  return(ts_list)
  
}