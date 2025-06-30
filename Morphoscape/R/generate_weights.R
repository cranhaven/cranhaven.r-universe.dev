generate_weights <- function(step, n, data = NULL, nvar = NULL, varnames = NULL, verbose = TRUE) {
  
  if (!missing(step) && !is.null(step) && !missing(n) && !is.null(n)) {
    stop("Only one of 'step' or 'n' can be supplied.", call. = FALSE)
  }
  if (!missing(step)) {
    if (!is.numeric(step) || length(step) != 1 || step < 0 || step > 1) {
      stop("'step' must be a single numeric value between 0 and 1.", call. = FALSE)
    }
    n <- round(1/step)
  }
  else if (!missing(n)) {
    if (!is.numeric(n) || length(n) != 1) {
      stop("'n' must be a single numeric value.", call. = FALSE)
    }
    n <- round(n)
  }
  else {
    stop("One of 'step' or 'n' must be supplied.", call. = FALSE)
  }
  
  nvar.specified <- !is.null(nvar)
  varnames.specified <- !is.null(varnames)
  
  if (!is.null(data)) {
    if (inherits(data, "kriged_surfaces")) {
      varnames <- names(data[["autoKrige"]])
    }
    else if (inherits(data, "fnc_df")) {
      varnames <- attr(data, "func.names")
    }
    else {
      stop("'data' must be a kriged_surfaces or fnc_df object.", call. = FALSE)
    }
    nvar <- length(varnames)
    if (nvar.specified || varnames.specified) {
      warning(sprintf("'data' specified; ignoring %s.",
                      paste(c("'nvar'", "'varnames'")[c(nvar.specified, varnames.specified)],
                            collapse = " and ")),
              call. = FALSE)
    }
  }
  else if (varnames.specified) {
    if (!is.atomic(varnames)) {
      stop("'varnames' must be a vector of names.", call. = FALSE)
    }
    varnames <- as.character(varnames)
    
    if (!is.null(nvar)) {
      warning("'varnames' specified; ignoring 'nvar'.", call. = FALSE)
    }
      nvar <- length(varnames)
  }
  else if (nvar.specified) {
    if (!is.numeric(nvar) || length(nvar) != 1) {
      stop("'nvar' must be a number corresponding to the number of desired columns.", call. = FALSE)
    }
    varnames <- seq_len(nvar)
  }
  else {
      stop("'data', 'varnames', or 'nvar' must be specified.", call. = FALSE)
  }
  
  weights <- parti(n, nvar)/n
  
  colnames(weights) <- varnames
  
  class(weights) <- c("grid_weights", class(weights))
  
  if (verbose) message(sprintf("%s rows generated", nrow(weights)))
  
  return(weights)
}

#Computes all possible partitions of n integers that sum to k
parti <- function(n, k) {
  if (n < 0) {
    message("error: n<0")
    return(NA)
  }
  if (k == 1)
    return(matrix(n, 1, 1))
  M <- cbind(parti(n, k - 1), 0)
  if (n > 0)
    for (i in (1:n)) M <- rbind(M, cbind(parti(n - i, k - 1), i))
  M
  
}