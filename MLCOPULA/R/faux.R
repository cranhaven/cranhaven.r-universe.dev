faux <- function(arbol,m){
  names <- colnames(m) 
  indx1 <- match(arbol$from,names)
  indx2 <- match(arbol$to,names)
  
  aux1 <- c()
  aux2 <- c()
  
  
  for(i in 1:length(indx1)){
    if(indx1[i] > indx2[i]){
      aux1[i] <- indx2[i]
      aux2[i] <- indx1[i]
    }else{
      aux1[i] <- indx1[i]
      aux2[i] <- indx2[i]
    }
  }
  
  arbol_new <- data.frame(from = names[aux1], to = names[aux2])
  return(arbol_new)
}
