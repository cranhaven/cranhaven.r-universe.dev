npffAH <- function(A,H,P){
  
  K <- gram2edm(P %*% t(P))
  Sol <- sum((H * (A - K))^2)
  
  return(Sol)
  
}
