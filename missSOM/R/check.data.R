## several checks for input data of the imputeSOM and map functions

check.data <- function(data) {
  ## Check whether data is a matrix or data frame
  if (!is.matrix(data) & !is.data.frame(data))
    stop("Argument 'data' should be a matrix or data frame")

  ## Check whether data is numeric
  if (!all(sapply(data, is.numeric)))
    stop("Argument data should be numeric")
  
  ## Convert data frame to matrix
  data <- as.matrix(data)
  
  data
}

## Objective: identify columns with too many NA values

## Check for empty columns in the data 
check.empty.columns <- function(data, maxNA.fraction) {
  nacolumns <- which(apply(data, 2, function(j) sum(is.na(j)) / length(j)) >= maxNA.fraction)
  
  nacolumns
}

remove.data.na <- function(data, nacolumns) {
  if (length(nacolumns) > 0) 
    data <- data[, -nacolumns]
  
  ## check to see if there are empty data 
  ## because of the maxNA.fraction
  if (is.null(ncol(data)))
    stop("Empty data layer - check maxNA.fraction argument")
  
  data
}

