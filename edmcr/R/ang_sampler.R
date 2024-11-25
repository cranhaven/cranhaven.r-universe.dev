ang_sampler <- function(seq, phi_con, psi_con){
  
  len <- nrow(phi_con)
  phi <- rep(0, len)
  psi <- rep(0, len)
  
  for(i in 1:len){
    if(seq[i] == 15){
      phi[i] <- -60
      psi[i] <- -50
    }else if(i < len){
      if(seq[i+1] == 15){
        phi[i] <- -120
        psi[i] <- 120
      }else{
        phi[i] <- runif(1, phi_con[i,1], phi_con[i,2])
        psi[i] <- runif(1, psi_con[i,1], psi_con[i,2])
      }
    }
  }
  
  return(list(phi=phi, psi=psi))
  
}