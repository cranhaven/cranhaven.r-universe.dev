"allelefreqs" <-
function(dat){
f <- apply(dat, 2, function(x) { 
  a <- table(x)/sum(table(x))
  nms <- names(a)
  if (length(nms)>2) stop("There are more than 2 alleles in the data set!")
  if (length(nms)==1) { if (nms=="1") b <- 0 else b <- 1 }
  else b <- a[[1]] } )
np <- c(which(f==0),which(f==1))
if (length(np)>0) flag <- FALSE else flag <- TRUE   
return(list(freqs=f, all.polym=flag, non.polym=sort(np)))  
}

