
CONGRUENCE <- function (target, loadings, verbose=TRUE) {
  
  if (all(is.na(loadings)))
    message('\n\nThe provided "loadings" is NA. The CONGRUENCE analyses cannot proceed.')
  if (all(is.na(target)))
    message('\n\nThe provided "target" is NA. The CONGRUENCE analyses cannot proceed.')
  
  # aligning matrices by factor & computing factor congruence
  # factor solution congruence before alignment
  rcBefore <- sum(target*loadings) / sqrt (sum(target*target) * sum(loadings*loadings))
  nrows <- nrow(target)
  ncols <- ncol(target)
  target <- as.matrix(target)
  loadings <- as.matrix(loadings)
  if (ncol(loadings) > 1) {
    signs <- matrix(0,1,ncols)
    mmm <- matrix(-9999,nrows,ncols)
    
    # matrix of position permutations
    # zzz <- as.matrix(permn(1:ncols))
    # fin <- t(array(unlist(zzz), dim <- c(ncols, nrow(zzz))))
    # # # http://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r
    # a <- seq(1:5)
    # eg <- expand.grid(a,a,a,a,a)
    # xx <- eg[apply(eg, 1, anyDuplicated) == 0, ]
    # # A check that every element occurs the same number of times in each position
    # apply(t(array(unlist(xx), dim = c(5, nrow(xx)))), 2, tabulate, nbins = 5)
    # # http://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r
    # a <- seq(1:3)
    # eg <- expand.grid(a,a,a)
    # xx <- eg[apply(eg, 1, anyDuplicated) == 0, ]
    # http://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r
    
    a <- seq(1:ncols)
    eg <- eval(parse(text=(paste('expand.grid(', 
                                 paste(replicate(ncols, 'a'), collapse = ',') ,')'))))
    fin <- eg[apply(eg, 1, anyDuplicated) == 0, ]
    
    # A check that every element occurs the same number of times in each position
    #apply(t(array(unlist(fin), dim = c(ncols, nrow(fin)))), 2, tabulate, nbins = ncols)
    # matrix of possible factor signs
    
    nos  <- matrix(-9999, 1, ncols)
    for (jj in 1:ncols) { nos[1,jj] <- 2^jj }
    seqsize <- (2^(ncols)) / nos
    signs <- matrix(-9999, (2^(ncols)), ncols)
    for (jj in 1:ncols) { 
      grp <- rbind(matrix(1,seqsize[1,jj],1), matrix(-1,seqsize[1,jj],1))
      signs[,jj] <- as.matrix(rep(grp, (nos[1,jj]/2)), (2^(ncols)) , 1)
    }
    
    # only half the matrix of signs is needed
    signs <- signs[1:(nrow(signs)/2),]
    
    # finding the permutation, & set of signs, with the highest congruence
    rc <- 0
    for (ii in 1:nrow(fin)) {
      for (iisigns in 1:nrow(signs)) {
        for (jj in 1:ncols) { mmm[,jj] <- loadings[,fin[ii,jj]] * signs[iisigns,jj] }
        rcnew <- sum(target*mmm) / sqrt (sum(target*target) * sum(mmm*mmm))
        if (abs(rcnew) > abs(rc)) { 
          rc <- rcnew
          ans <- rbind(fin[ii,])
          signsnew <- rbind(signs[iisigns,]) 
        }
      }
    }
    if (rc < 0) signsnew <- signsnew * -1 
    
    # rearranging the loading matrix
    loadingsAfter <- matrix(-9999,nrows,ncols)
    for (jj in 1:ncols) { loadingsAfter[,jj] <- loadings[,ans[1,jj]]*signsnew[1,jj] }
    
  }
  
  if (ncol(loadings) == 1 & rcBefore > 0) loadingsAfter <- loadings
  if (ncol(loadings) == 1 & rcBefore < 0) loadingsAfter <- loadings * -1
  #if (ncol(loadings) == 1 & rcBefore >0) { loadingsAfter <- loadings } else {  loadingsAfter <- loadings * -1}
  #if (rcBefore < 0)  loadingsAfter <- loadingsAfter * -1
  
  # factor congruences after alignment
  rcFactors <- colSums(target*loadingsAfter) / sqrt (colSums(target*target) * colSums(loadingsAfter*loadingsAfter))
  
  # factor solution congruence after alignment
  rcAfter <- sum(target*loadingsAfter) / sqrt (sum(target*target) * sum(loadingsAfter*loadingsAfter))
  
  # % var in target & in resid matrix
  resid <- abs(target - loadingsAfter)
  pertarget <- sum(target*target) / nrows 
  perresid  <- sum(resid*resid) / nrows 
  
  # root mean square residual
  rmsr <- sqrt (sum(resid*resid) / (nrows * ncols))
  
  if (verbose ) {
    cat('\n\nFactor solution congruences before & after factor alignment:')
    cat('\n\nFactor soution congruence before alignment = ', round(rcBefore,2))
    cat('\n\nFactor soution congruence after alignment = ', round(rcAfter,2))
    cat('\n\nFactor congruences after alignment: \n\n'); print(round(rcFactors,2))
    cat('\n\nProportion of variance in target matrix = ', round(pertarget,2))
    cat('\n\nProportion of variance in residual matrix = ', round(perresid,2))
    cat('\n\nRoot mean square residual = ', round(rmsr,2), '\n')
  }
  
  congruenceOutput <- list(rcBefore=rcBefore, rcAfter=rcAfter, rcFactors=rcFactors, rmsr=rmsr, 
                           residmat=resid, loadingsNew=loadingsAfter)
  
  return(invisible(congruenceOutput))
}
