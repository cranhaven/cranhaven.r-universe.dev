#'Percentage of anomalies which agrees with the sign of the mean anomaly for 
#'multidimensional arrays
#'
#'@description This function computes the mean and the percentage of agreement 
#'between anomalies.
#'
#'@param ano A multidimensional array.
#'@param membersdim The dimension in which models are stored.
#'@param na.rm A logical indicating whether missing values should be removed. If 
#'  \code{na.rm} is FALSE an NA value in any of the arguments will cause a value 
#'  of NA to be returned, otherwise (TRUE by default) NA values are ignored.
#'@param ncores The number of cores to be used when computing the agreement.
#'
#'@return An array of one dimension less than the \code{ano} object, except for 
#'one dimensional arrays or vectors, for which an array of dimension 1 called 
#''var' is returned.
#'
#'@import multiApply
#'@examples
#'# Example with random sample:
#'a <- NULL
#'for(i in 1:20) { a <- c(a, rnorm(6)) }
#'dim(a) <- c(lat = 2, lon = 3, var = 4, mod = 5)
#'
#'agree <- AnoAgree(ano = a, membersdim = which(names(dim(a)) == 'mod'), 
#'                  na.rm = TRUE, ncores = NULL)
#'print(agree)
#'
#'a <- rnorm(6)
#'agree <- AnoAgree(ano = a, membersdim = 1, na.rm = TRUE, ncores = NULL)
#'print(agree)
#'@export
AnoAgree <- function(ano, membersdim, na.rm = TRUE, ncores = NULL) {
  if (is.null(ano)) {
    stop("Parameter 'ano' cannot be NULL.")
  }
  if (!is.numeric(ano)) {
    stop("Parameter 'ano' must be a numeric array.")
  }
  if (!is.numeric(membersdim)) {
    stop("Parameter 'membersdim' must be an integer.")
  }
  if (is.null(dim(ano))) {
      if (membersdim == 1) {
          dim(ano) <- c(mod = length(ano), var = 1)
      } else { 
          dim(ano) <- c(mod = length(ano), var = 1)
          membersdim = 1
      }
  } else if (length(dim(ano)) == 1) {
      if (membersdim == 1) {
          dim(ano) <- c(dim(ano), var = 1)
      } else { 
          dim(ano) <- c(dim(ano), var = 1)
          membersdim = 1
      }
  }
  if (!is.logical(na.rm)) {
    stop("Parameter 'na.rm' must be logical.")
  }
  if (length(na.rm) > 1) {
    na.rm = na.rm[1]
    warning("Parameter 'na.rm' has length > 1 and only the first element will be used.")
  }
  if (!is.numeric(ncores) & !is.null(ncores)) {
    stop("Parameter 'ncores' must be numeric.")
  }
  if (length(ncores) == 0 & !is.null(ncores)) {
    stop("Parameter 'ncores' must be of length 1.")
  }
  if (length(ncores) > 1) {
    ncores = ncores[1]
    warning("Parameter 'ncores' has length > 1 and only the first element will be used.")
  }
  if (!is.null(ncores)) {
    ncores <- round(ncores)
    if (ncores == 0) {
      ncores = NULL
    }
  }
    margins <- 1 : length(dim(ano))
    margins <- margins[-membersdim]
    ano_agree <- Apply(data = list(ano), margins = list(margins), 
                       fun = .AnoAgree, 
                       na.rm = na.rm, 
                       ncores = ncores)[[1]]
    if (!is.null(names(dim(ano))) & length(dim(ano)) > 0) {
      dim_names <- names(dim(ano))
      names(dim(ano_agree)) <- dim_names[-membersdim]
    }
  ano_agree
}
.AnoAgree <- function(data, na.rm = na.rm) {
  data <- unlist(data)
  ano_mean <- mean(data, na.rm = na.rm)
  if (!is.na(ano_mean)) {
    if (ano_mean > 0) {
      ano_agree <- 100 * sum(data[!is.na(data)] > 0) / length(data[!is.na(data)]) 
    } else if (ano_mean < 0) {
      ano_agree <- 100 * sum(data[!is.na(data)] < 0)  / length(data[!is.na(data)]) 
    } else if (ano_mean == 0) {
      warning("Anomaly mean is equal to 0.")
    }
  } else {
    ano_agree <- NA
  }
  return(ano_agree)
}
