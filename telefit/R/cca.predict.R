#' Make predictions using canonical correlation analysis (CCA)
#'
#' Canonical correlation analysis (CCA) is sometimes referred to as a 
#' double-barreled principal component analysis.  Loosely, it fits a linear 
#' regression model to the scores of principal component decompositions for
#' of the predictors \code{X} and responses \code{Y}.  Oftentimes, only the 
#' largest \eqn{k} principal components are used to make predictions.
#'  
#' 
#' CCA has been used to predict a teleconnected response (like precipitation) 
#' using the remote field generating the teleconnection 
#' (like ocean temperatures).  In this application, principal components are 
#' often referred to as empirical orthogonal functions (EOFs).
#' 
#' 
#' @export
#' 
#' @param X An \code{(nvars x nobs)} data frame or matrix in which each column
#'    contains all observations of measured (predictor) variables for a given 
#'    timepoint or sample.  For example, if X represents a spatial variable that
#'    was recorded at several timepoints, then each row of X should contain the
#'    variable's measurement for all timepoints at a single location.
#'    
#' @param Y An \code{(nvars x nobs)} data frame or matrix in which each column
#'    contains all observations of measured (response) variables for a given
#'    timepoint or sample.
#'    
#' @param X.new An \code{(nvars x nobs.new)} data frame or matrix of values to
#'    use to predict Y.new using CCA.
#'    
#' @param k.x An integer less than \code{(nobs)} indicating how many eigenvectors
#'    of \code{(X)} to use in the CCA.
#'    
#' @param k.y An integer less than \code{(nobs)} indicating how many eigenvectors
#'    of \code{(Y)} to use in the CCA.
#'  
#' @return Y.new Predicted values for Y.new
#'
#' @references Cook, E.R., Briffa, K.R., and Jones, P.D., 1994, Spatial regression methods in dendroclimatology: A review and comparison of two techniques: International Journal of Climatology, v. 14, p. 379-402.
#' 
#' @references Glahn, H.R., 1968, Canonical Correlation and Its Relationship to Discriminant Analysis and Multiple Regression: Journal of the Atmospheric Sciences, v. 25, p. 23-31.
#' 
#' @examples 
#' 
#' data("coprecip")
#' attach(coprecip)
#' 
#' # compute CCA predictions of Y (CO precipitation) given Z (Pacific ocean SSTs)
#' # using 2 principal components (aka. EOFs)
#' preds = cca.predict(X = Z, Y = Y, X.new = Z, k.x = 2, k.y = 2)
#' 
#' # compute R^2
#' 1 - var(as.numeric(preds-Y)) / var(as.numeric(Y))
#' 

cca.predict = function(X, Y, X.new, k.x, k.y) {
  
  nXp = scale(t(X))
  nYq = scale(t(Y))
  
  nXp.new = sweep(t(X.new), 2, attr(nXp, 'scaled:center'), '-')
  nXp.new = sweep(nXp.new, 2, attr(nXp, 'scaled:scale'), '/')
  
  n = nrow(nXp)
  p = ncol(nXp)
  q = ncol(nYq)
  
  cor.eigen = function(X, scale = F) {
    # efficiently compute non-zero eigenvectors of the correlation matrix for X
    
    if(scale) { X = scale(X) }
    
    if(nrow(X) < ncol(X)) {
      X = X / sqrt(n-1)
      e = eigen(X %*% t(X))
      e$values[e$values<0]=0
      list(vectors = sweep(t(X) %*% e$vectors, 2, sqrt(e$values), FUN = "/"), 
           values = e$values)
    } else {
      eigen(t(X)%*%X/(n-1))
    }
  }
  
  pRp.e = cor.eigen(nXp)
  qRq.e = cor.eigen(nYq)
  
  pLp = diag(pRp.e$values)
  pEp = pRp.e$vectors
  qMq = diag(qRq.e$values)
  qFq = qRq.e$vectors
  
  nUpp = scale(nXp %*% pEp[,1:k.x])
  nVqq = scale(nYq %*% qFq[,1:k.y])
  
  nUpp.new = sweep(nXp.new %*% pEp[,1:k.x], 2, attr(nUpp, 'scaled:center'), '-')
  nUpp.new = sweep(nUpp.new, 2, attr(nUpp, 'scaled:scale'), '/')
  
  pRp = t(nUpp) %*% nUpp / (n-1)
  pRq = t(nUpp) %*% nVqq / (n-1)
  qRq = t(nVqq) %*% nVqq / (n-1)
  
  # Cook eq. 19
  B.e = eigen(solve(qRq) %*% t(pRq) %*% solve(pRp) %*% pRq)
  B = B.e$vectors
  B.e$values[B.e$values<0]=0
  qLambdaq = diag(B.e$values)
  
  # Glahn (1968) eq. 13
  A = solve(pRp) %*% pRq %*% B %*% diag(1/sqrt(B.e$values))
  
  V.hat = nUpp.new %*% A %*% qLambdaq %*% solve(B)
  
  # unscale and backtransform V.hat
  V.hat = sweep(V.hat, 2, attr(nVqq, 'scaled:scale'), "*")
  V.hat = sweep(V.hat, 2, attr(nVqq, 'scaled:center'), "+")
  Y.hat = V.hat %*% t(qFq[,1:k.y])
  
  t(Y.hat)
}
