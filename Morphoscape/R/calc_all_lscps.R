calc_all_lscps <- function(kr_data, grid_weights, file = NULL){
  
  if (!inherits(kr_data, "kriged_surfaces")){
    stop("'kr_data' must be a kriged_surfaces object, the output of a call to krige_surf().", call. = FALSE)
  }
  
  if (!inherits(grid_weights, "grid_weights")){
    stop("'grid_weights' must be a grid_weights object, the output of a call to generate_weights().", call. = FALSE)
  }
  
  func.names <- names(kr_data$autoKrige)
  
  if (ncol(grid_weights) != length(func.names)) {
    stop("'grid_weights' must have weights for each functional characteristic in 'kr_data'.", call. = FALSE)
  }
  
  if (!identical(colnames(grid_weights), func.names)) {
    colnames(grid_weights) <- func.names
  }
  
  grid_weights <- grid_weights[,func.names, drop = FALSE]
  
  wtd_lscps <- lapply(kr_data$dataframes, function(x) {
    as.matrix(x[func.names]) %*% t(grid_weights)
  })

  all_lscps <- list(dataframes = kr_data$dataframes, 
                    wtd_lscps = wtd_lscps,
                    grid_weights = grid_weights)
  
  class(all_lscps) <- "all_lscps"
  
  if (length(file) > 0) {
    if (length(file) != 1 || !is.character(file)) {
      warning("'file' must be a string containing the file path for the file to be saved. No file will be saved.", call. = FALSE)
    }
    else if (endsWith(tolower(file), ".rds")) {
      saveRDS(all_lscps, file = file)
    }
    else if (endsWith(tolower(file), "rdata")) {
      save(all_lscps, file = file)
    }
    else {
      warning("'file' must have an .rds or .rdata file extension. No file will be saved.", call. = FALSE)
    }
  }
  
  return(all_lscps)
}

print.all_lscps <- function(x, ...) {
  cat("An all_lscps object\n")
  cat("- functional characteristics:\n\t", paste(names(x[["dataframes"]][["grid"]])[-(1:2)], collapse = ", "), "\n", sep = "")
  cat("- number of landscapes:\n\t", nrow(x[["grid_weights"]]), "\n", sep = "")
  cat("- weights incremented by:\n\t",
      round(x[["grid_weights"]][2,2], 4),
      "\n", sep = "")
  if (!is.null(x[["dataframes"]][["new_data"]])) {
    cat("- new data:\n\t", nrow(x[["dataframes"]][["new_data"]]), " rows\n", sep = "")
  }
  
  invisible(x)
}
