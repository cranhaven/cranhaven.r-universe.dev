pl <- function(bw0, bwreg, response, dat, delta, lambda, ns){

  if(is.na(bw0)) return(0)
  xb <- predict(bwreg)
  x0 <- model.matrix.gam(bwreg)
  resi <- response - xb
  
  w0 <- bwreg$weights
  W0 <- diag(w0)

  dd <- bwreg$model
  
  forml <- pl_formula(bwreg)
  
  if(is.null(lambda)){
    xp <- expectreg.ipc(forml, expectiles = 0.5, data = dd, KMweights = dd$`(weights)`, smooth = "fixed", hat1 = TRUE)
  } else{
    xp <- expectreg.ipc(forml, lambda = lambda, expectiles = 0.5, data = dd, KMweights = dd$`(weights)`, smooth = "fixed", hat1 = TRUE)
  }
  
  A2 <- xp$hat

  nonzero <- w0 != 0
  KMnz <- dat$KMweights[nonzero]
  responsenz <- response[nonzero] 
  xbnz <- xb[nonzero] 
  
  sc2 <- numeric()
  j <- 1
  for(k in 1:length(resi)){
    
    if(delta[k] == 1){
      if(nonzero[k]){
        resi_cv2 <- responsenz - xbnz + A2[, j] * (responsenz[j] - xbnz[j]) / (1 - A2[j, j])
        sc2[k] <- 1 / (length(response) * bw0) * sum(KMnz[-j] * dnorm((resi_cv2[-j] - resi[k]) / bw0))
        j <- j + 1
      } else{
        sc2[k] <- 1 / (length(response) * bw0) * sum(dat$KMweights[-k] * dnorm((resi[-k] - resi[k]) / bw0))
      }
    } else{
      sc2[k] <- 1 - 1 / length(response) * sum(dat$KMweights[-k] * pnorm((resi[k] - resi[-k]) / bw0))
    }
    
    
  }
  
  
  if(any(is.na(sc2)) || any(sc2 <= 0)){
    #warning("negative or zero values in pseudo-likelihood")
    return(-10^10)
  } else{
    
    return(sum(log(sc2)))
  }
  
}