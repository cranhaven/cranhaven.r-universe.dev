unperm <- function(blk, permZ, X, Z){
  for(p in 1:nrow(blk)){
    if(blk[[p,1]] == "s" & length(permZ[[p]] > 0)){
      per <- permZ[[p]]
      X[[p]] <- X[[p]][per,per]
      Z[[p]] <- X[[p]][per,per]
    }
  }
  
  return(list(X=X,Z=Z))
}