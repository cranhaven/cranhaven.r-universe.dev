#'Compute effective sample size with classical method
#'
#'Compute the number of effective samples along one dimension of an array. This
#'effective number of independent observations can be used in
#'statistical/inference tests.\cr
#'The calculation is based on eno function from Caio Coelho from rclim.txt.
#'
#'@param data A numeric array with named dimensions.
#'@param time_dim A function indicating the dimension along which to compute 
#'  the effective sample size. The default value is 'sdate'.
#'@param na.action A function. It can be na.pass (missing values are allowed) 
#'  or na.fail (no missing values are allowed). See details in stats::acf(). 
#'  The default value is na.pass.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return An array with the same dimension as parameter 'data' except the 
#'  time_dim dimension, which is removed after the computation. The array  
#'  indicates the number of effective sample along time_dim.
#'
#'@examples
#'set.seed(1)
#'data <- array(rnorm(800), dim = c(dataset = 1, member = 2, sdate = 4, 
#'                                  ftime = 4, lat = 10, lon = 10))
#'na <- floor(runif(40, min = 1, max = 800))
#'data[na] <- NA
#'res <- Eno(data)
#'
#'@importFrom stats acf na.pass na.fail
#'@import multiApply
#'@export
Eno <- function(data, time_dim = 'sdate', na.action = na.pass, ncores = NULL) {

  # Check inputs 
  ## data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  if (is.null(dim(data))) {  #is vector
    dim(data) <- c(length(data))
    names(dim(data)) <- time_dim
  }
  if (any(is.null(names(dim(data)))) | any(nchar(names(dim(data))) == 0)) {
    stop("Parameter 'data' must have dimension names.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(data))) {
    stop("Parameter 'time_dim' is not found in 'data' dimension.")
  }
  ## na.action
  if (as.character(substitute(na.action)) != "na.pass" &
      as.character(substitute(na.action)) != "na.fail") {
      stop("Parameter 'na.action' must be a function either na.pass or na.fail.")
  }
  if (as.character(substitute(na.action)) == "na.fail" && anyNA(data)) {
    stop("Calculation fails because NA is found in paratemter 'data', ",
         "which is not accepted when ",
         "parameter 'na.action' = na.fail.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }

  ###############################
  # Calculate Eno

  eno <- Apply(data = list(data),
               target_dims = time_dim,
               output_dims = NULL, 
               fun = .Eno,
               na.action = na.action,
               ncores = ncores)$output1

  return(eno)
}

.Eno <- function(x, na.action) {
  n <- length(sort(x))
  if (n > 1) {
    a <- acf(x, lag.max = n - 1, plot = FALSE,
             na.action = na.action)$acf[2:n, 1, 1]
    s <- 0
    for (k in 1:(n - 1)) {
      s <- s + (((n - k) / n) * a[k])
    }
    eno <- min(n / (1 + (2 * s)), n)
  } else {
    eno <- NA
  }

  return(eno)
}

