
# Procrustes rotation of factor loading matries

PROCRUSTES <- function (loadings, target, type='orthogonal', verbose=TRUE) {
  
  #   type = 'orthogonal' or 'oblique'
  # orthogonal Procrustes   
  
  # McCrae et al 1996 JPSP, 70, p. 566; based on Schoneman 1966, Psychometrika, 31, 1-10
  if  (type == 'orthogonal') {
    s= t(loadings)%*%target
    w1 = s %*% t(s)
    v1 = t(s) %*% s
    w <- eigen(w1) $vectors
    ew <- diag(eigen(w1) $values)
    v <- eigen(v1) $vectors
    ev <- diag(eigen(v1) $values)
    o = t(w) %*% s %*% v
    k = diag(  ((diag(o)) / abs(diag(o))) , nrow = nrow(o), ncol =nrow(o))
    ww = w %*% k
    t1 = ww %*% t(v)
    procrust = loadings %*% t1 
  }
  
  # oblique Procrustes   
  # Hurley & Cattell 1962, Behavioral Science, 7, 258
  if (type == 'oblique') {
    cat1 = (solve(t(loadings) %*% loadings)) %*% t(loadings) %*% target
    cat2 = sqrt (colSums(cat1 * cat1))
    numrows = nrow(cat1) - 1
    cat3 = cat2
    for (t in 1:numrows) { cat3 = rbind(cat3, cat2) }
    procrust =  loadings %*% (cat1 / cat3)   
  }
  
  colnames(procrust) <- colnames(loadings)
  
  # congruence coefficient
  rtproc = sum(target*procrust) / sqrt (sum(target*target)  * sum(procrust*procrust))
  
  #  % var in target vs % var in resid matrix.
  resid = target - procrust
  nvars = nrow(target)
  pertarget =  sum(target*target) / nvars
  perresid =  sum((target - procrust) * (target - procrust)) / nvars 
  
  #root mean square residual.
  rmsr = sqrt (sum(resid*resid) / (nvars * ncol(target)))
  
  if (verbose ) {
    if (type=='orthogonal') {cat('\n\nOrthogonal Procrustes Rotation') }
    if (type=='oblique')    {cat('\n\nOblique Procrustes Rotation:') }
    cat('\n\nOriginal Loadings:\n\n'); print(round(loadings,3), print.gap=3)
    cat('\n\nTarget:\n\n'); print(round(target,3), print.gap=3)
    cat('\n\nProcrustes-Rotated Loadings:\n\n'); print(round(procrust,3), print.gap=3)
    cat('\n\nCongruence Between the Target & Procrustes-Rotated Loadings = ', round(rtproc,3))
    cat('\n\nProportion of Variance in Target = ', round(pertarget,3))
    cat('\n\nProportion of Variance in Residual = ', round(perresid,3))
    cat('\n\nRoot Mean Square Residual = ', round(rmsr,3),'\n')
  }
  
  procrustesOutput <- list(loadingsPROC=procrust, congruence=rtproc, rmsr=rmsr, residmat=resid)
  
  return(invisible(procrustesOutput))
  
}
