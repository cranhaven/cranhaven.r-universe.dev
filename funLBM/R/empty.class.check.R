empty.class.check <- function(W,Pw=c(),display=FALSE){
  nb = 0
  while (min(colSums(W))<1 & nb <10){
    ll = which(colSums(W)<1)
    for (l in ll){
      if (length(Pw)==0){ind = sample(nrow(W),1)}
      else ind = which.max(Pw[,l])
      #ind = sample(nrow(W),2)
      W[ind,] = matrix(0,1,ncol(W)); W[ind,l] = 1
    }
    if(display) cat('Warning: Class is becoming empty! A regularization has been done.\n')
    nb = nb + 1
  }
  W
}
