FMshnReg <- function(y, x1, alpha = NULL, Abetas = NULL, medj=NULL, pii = NULL, g = NULL, get.init = TRUE, algorithm = "K-means", accuracy = 10^-6, show.envelope="FALSE", iter.max = 100)
{

  if((length(g)!= 0) && (g < 1)) { stop("g must be greater than 0.\n") }

  if(get.init == TRUE)
  {
    initvalues <- initial.values(y,x1,g,algorithm)
    out <- EMmixlogbs(y, x1, initvalues$alpha, initvalues$Abetas, initvalues$medj, initvalues$pii, nu=3 ,g, mfamily="Normal", accuracy, iter.max)
  }else{
    out <- EMmixlogbs(y, x1, alpha, Abetas, medj, pii, nu=3, g, mfamily="Normal", accuracy, iter.max)
  }

  #Running the algorithm
  cat('\n')
  cat('------------------------------------------------------------\n')
  cat('Finite Mixture of Sinh Normal Regression Model\n')
  cat('------------------------------------------------------------\n')
  cat('\n')
  cat('Observations =',length(y))
  cat('\n')
  cat('\n')
  cat('-----------\n')
  cat('Estimates\n')
  cat('-----------\n')
  cat('\n')
  print(round(out$result$ttable,5))
  cat('\n')
  #if(criteria==TRUE)
  #{
    cat('------------------------\n')
    cat('Model selection criteria\n')
    cat('------------------------\n')
    cat('\n')
    critFin <- c(out$result$lk, out$result$aic, out$result$bic, out$result$edc)
    critFin <- round(t(as.matrix(critFin)),digits=3)
    dimnames(critFin) <- list(c("Value"),c("Loglik", "AIC", "BIC","EDC"))
    print(critFin)
    cat('\n')
  #}
  cat('-------\n')
  cat('Details\n')
  cat('-------\n')
  cat('\n')
  cat("Convergence reached? =",(out$result$iter < iter.max))
  cat('\n')
  cat('EM iterations =',out$result$iter,"/",iter.max)
  cat('\n')
  cat('Criteria =',round(out$result$criterio,9))
  cat('\n')
  cat("Processing time =",out$result$time,units(out$result$time))
  cat('\n\n')

  if(show.envelope==TRUE)
    envelop(y,x1, model=out,replicas=100)
}

#FMshnReg(y, x1, alpha = NULL, Abetas = NULL, medj=NULL, pii = NULL, g = 3, get.init = TRUE, algorithm = "K-means", accuracy = 10^-6, iter.max = 100)
