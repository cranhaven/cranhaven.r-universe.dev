# Calculation of species accumulation curves and cummulative richness estimates
#   as produced by EstimateS.

# See the EstimateS Users Guide:
# http://viceroy.eeb.uconn.edu/estimates/EstimateSPages/EstSUsersGuide/EstimateSUsersGuide.htm
# downloaded 2013-03-14.

# Helper functions, pretty trivial but useful to plug into richCurve
richSobs <- function(incVec) {
  # Convert a matrix/dataframe into a vector and round: 
  incVec <- round(incVec)
  if(is.matrix(incVec) || is.data.frame(incVec))
    incVec <- rowSums(incVec)
  sum(incVec > 0)
}

richSingle <- function(cntVec) {
  # Convert a matrix/dataframe into a vector and round: 
  cntVec <- round(cntVec)
  if(is.matrix(cntVec) || is.data.frame(cntVec))
    cntVec <- rowSums(cntVec)
  sum(cntVec == 1)
}

richDouble <- function(cntVec) {
  # Convert a matrix/dataframe into a vector and round: 
  cntVec <- round(cntVec)
  if(is.matrix(cntVec) || is.data.frame(cntVec))
    cntVec <- rowSums(cntVec)
  sum(cntVec == 2)
}


richUnique <- function(incMat)
  sum(rowSums(round(incMat) > 0) == 1)

richDuplicate <- function(incMat)
  sum(rowSums(round(incMat) > 0) == 2)


richCurve <- function(obsMat, FUNC, runs=10, ...)  {

  m <- ncol(obsMat)
  K <- length(FUNC(obsMat))
  out <- array(NA, c(runs, m, K))
  if(K > 1)
    dimnames(out) <- list(NULL, NULL, names(FUNC(obsMat)))
  shuff <- obsMat
  for(i in 1:runs)  {
    for(j in 1:m)   {
      out[i, j, ] <- FUNC(shuff[, 1:j, drop=FALSE], ...)
    }
    shuff <- shuff[, sample(m)]
  }
  list(mean = apply(out, 2:3, mean, na.rm=TRUE),
        SD  = apply(out, 2:3, sd, na.rm=TRUE))
}

