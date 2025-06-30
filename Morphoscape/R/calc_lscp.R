calc_lscp <- function(data, weights, ...) {
  UseMethod("calc_lscp")
}

calc_lscp.kriged_surfaces <- function(data, weights, ...) {

  fn_dataframe <- data$dataframes
  func.names <- names(data$autoKrige)
  
  if (missing(weights) || !is.numeric(weights) || length(weights) != length(func.names)) {
    stop("'weights' must be a vector of weights, one for each functional characteristic in 'data'.", call. = FALSE)
  }
  names(weights) <- func.names
  
  out <- calc.W.kr(weights,
                   fnc_data = fn_dataframe,
                   func.names = func.names)
  
  #Output is class "wtd_lscp"
  out
}

calc_lscp.all_lscps <- function(data, weights, i, ...) {
  
  fn_dataframe <- data$dataframes
  func.names <- colnames(data$grid_weights)
  
  if (missing(weights) && missing(i)) {
    stop("'weights' or 'i' must be supplied.", call. = FALSE)
  }
  if (!missing(weights) && !missing(i)) {
    warning("'weights' and 'i' both supplied; ignoring 'i'.", call. = FALSE)
  }
  
  if (!missing(weights)) {
    if (length(weights) != length(func.names) || !is.numeric(weights)) {
      stop("'weights' must be a vector of weights, one for each functional characteristic in 'data'.", call. = FALSE)
    }
  }
  else { #use n
    if (length(i) != 1 || !is.numeric(i) || !i %in% seq_len(nrow(data$grid_weights))) {
      stop("'i' must be the index of a set of weights in the grid_weights object supplied to calc_all_lscps().", call. = FALSE)
    }
    weights <- data$grid_weights[i,]
  }
  
  names(weights) <- func.names
  
  out <- calc.W.kr(weights,
                   fnc_data = fn_dataframe,
                   func.names = func.names)
  
  #Output is class "wtd_lscp"
  out
}

#Calculate Zprime values for a given vector of W
#fnc_data is list containing grid and (optionally) new_data
calc.W.kr <- function(W, fnc_data, func.names) {
  
  Wprime <- lapply(fnc_data, function(data) {
    data[func.names] <- sweep(data[func.names], 2, W, FUN = "*")
    Z <- rowSums(data[func.names])
    
    cbind(data, Z = Z)
  })
  
  out <- list(W = W, Wprime = Wprime)
  class(out) <- "wtd_lscp"
  
  out
}

print.wtd_lscp <- function(x, ...) {
  cat("A wtd_lscp object\n")
  cat("- weights:\n")
  print(round(x[["W"]], 4))
  if (!is.null(x[["Wprime"]][["new_data"]])) {
    cat("- new data:\n\t", nrow(x[["Wprime"]][["new_data"]]), " rows\n\t",
        "average Z = ", round(mean(x[["Wprime"]][["new_data"]][["Z"]]), 3), "\n", sep = "")
  }
}