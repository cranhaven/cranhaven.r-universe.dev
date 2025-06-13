#Source: bestmodels.R
#includes functions: best_glp_models, best_models_serial, best_models_parll,
# KEval
Kbest_glp_models <- function(z, glp = c("ARTFIMA", "ARFIMA", "ARIMA"), parMax=4, 
                            likAlg = c("exact", "Whittle"), d = 0, ...) {
#ranked AIC/BIC glp models with K parameters
  stopifnot(is.numeric(z))
  stopifnot(length(z)>=50)
  p <- q <- parMax
  ps <- 0:p
  qs <- 0:q
#set likelihood to -1e200
  LL_model <- matrix(-1e200, nrow=p+1, ncol=q+1)
  bicPenalty <- aicPenalty <- LL_model
  rn <- paste0(paste0("AR(", ps),")")
  cn <- paste0(paste0("MA(", qs),")")
  rownames(LL_model) <- rn
  colnames(LL_model) <- cn
  startTime <- proc.time()[3]
  glpOrder <-switch(glp, "ARTFIMA"=2, "ARFIMA"=1, "ARIMA"=0)
  for (i in 1:(p+1)) {
    for (j in 1:(q+1)) {
      if ( glpOrder+ps[i]+qs[j] > parMax ) {
        next
      }
      LL_model[i,j] <- artfima(z, glp=glp, arimaOrder=c(ps[i], d, qs[j]),
                               likAlg=likAlg, ...)$LL
      aicPenalty[i,j] <- 2*(ps[i]+qs[j]+glpOrder+2) #add 2. Cf arima()
      bicPenalty[i,j] <- log(length(z))*(ps[i]+qs[j]+glpOrder+2)
    }
  }
  totTime <- proc.time()[3] - startTime
  #
  aic_model <- (-2*LL_model) + aicPenalty
  indaic <- min(aic_model)==aic_model
  pOpt = ps[as.logical(apply(indaic, 1, sum))]
  qOpt = qs[as.logical(apply(indaic, 2, sum))]
  bestaicModel <- paste0(glp, "(", pOpt, ",", qOpt, ")" )
  bestaic <- min(aic_model)
  #
  bic_model <- (-2*LL_model) + bicPenalty
  indbic <- min(bic_model)==bic_model
  pOpt = ps[as.logical(apply(indbic, 1, sum))]
  qOpt = qs[as.logical(apply(indbic, 2, sum))]
  bestbicModel <- paste0(glp, "(", pOpt, ",", qOpt, ")" )
  bestbic <- min(bic_model)
  ans <- list(LL=LL_model, artfima_time = totTime, 
              aic=list(bestaic=bestaic, bestaicModel=bestaicModel, 
                       aic=aic_model),
              bic=list(bestbic=bestbic, bestbicModel=bestbicModel, 
                       bic=bic_model)
  )
  ans
}

best_models_serial <- function(z, parMax=4, nbest=4, 
            likAlg = c("exact", "Whittle"), plausibilityQ=TRUE, d=0, ...) {
  outARIMA <- Kbest_glp_models(z, glp="ARIMA", parMax=parMax, 
                              likAlg = likAlg, d = d)
  outARFIMA <- Kbest_glp_models(z, glp="ARFIMA", parMax=parMax, 
                              likAlg = likAlg, d = d)
  outARTFIMA <- Kbest_glp_models(z, glp="ARTFIMA", parMax=parMax, 
                              likAlg = likAlg, d = d)
  #
  aics <- c(as.vector(outARIMA$aic$aic), as.vector(outARFIMA$aic$aic),
            as.vector(outARTFIMA$aic$aic))
  bics <- c(as.vector(outARIMA$bic$bic), as.vector(outARFIMA$bic$bic),
            as.vector(outARTFIMA$bic$bic))
  p1 <- nrow(outARIMA$bic$bic)
  q1 <- ncol(outARIMA$bic$bic)
  armaLab1 <- as.vector(outer(0:(p1-1), 0:(q1-1), FUN=function(x,y) 
    paste0("ARIMA(",  x,",",d,",",y, ")")))
  p1 <- nrow(outARFIMA$bic$bic)
  q1 <- ncol(outARFIMA$bic$bic)
  armaLab2 <- as.vector(outer(0:(p1-1), 0:(q1-1), FUN=function(x,y) 
    paste0("ARFIMA(", x,",",d,",",y, ")" )))
  p1 <- nrow(outARTFIMA$bic$bic)
  q1 <- ncol(outARTFIMA$bic$bic)
  armaLab3 <- as.vector(outer(0:(p1-1), 0:(q1-1), FUN=function(x,y) 
    paste0("ARTFIMA(", x,",",d,",",y, ")")))
  armaLab <- c(armaLab1, armaLab2, armaLab3)
  ind <- order(aics)
  aics <- aics[ind]
  armaLab <- armaLab[ind]
  names(aics) <- armaLab
  aics <- aics[1:nbest]
  armaLab <- c(armaLab1, armaLab2, armaLab3)
  ind <- order(bics)
  bics <- bics[ind]
  armaLab <- armaLab[ind]
  names(bics) <- armaLab
  bics <- bics[1:nbest]
  ans <- list(aic=aics, bic=bics)
  class(ans) <- "bestmodels"
  ans 
}

bestModels <- function(z, parMax = 4, nbest=4, likAlg = c("exact", "Whittle"), 
                        d = 0, ...) {
  stopifnot(is.numeric(z))
  stopifnot(length(z)>=50)
  best_models_serial(z, parMax=parMax, nbest=nbest, likAlg = likAlg, d = d, ...)  
}

best_glp_models <- function(z, glp = c("ARTFIMA", "ARFIMA", "ARIMA"), p=2, q=2, 
                            likAlg = c("exact", "Whittle"), d = 0, ...) {
  stopifnot(p>0 || q>0)
  stopifnot(is.numeric(z))
  stopifnot(length(z)>=50)
  ps <- 0:p
  qs <- 0:q
  LL_model <- matrix(numeric(0), nrow=p+1, ncol=q+1)
  bicPenalty <- aicPenalty <- LL_model
  rn <- paste0(paste0("AR(", ps),")")
  cn <- paste0(paste0("MA(", qs),")")
  rownames(LL_model) <- rn
  colnames(LL_model) <- cn
  startTime <- proc.time()[3]
  glpOrder <-switch(glp, "ARTFIMA"=2, "ARFIMA"=1, "ARIMA"=0)
  for (i in 1:(p+1)) {
    for (j in 1:(q+1)) {
      LL_model[i,j] <- artfima(z, glp=glp, arimaOrder=c(ps[i], d, qs[j]),
                               likAlg=likAlg, ...)$LL
      aicPenalty[i,j] <- 2*(ps[i]+qs[j]+glpOrder+2) #add 2. Cf arima()
      bicPenalty[i,j] <- log(length(z))*(ps[i]+qs[j]+glpOrder+2)
    }
  }
  totTime <- proc.time()[3] - startTime
  #
  aic_model <- (-2*LL_model) + aicPenalty
  indaic <- min(aic_model)==aic_model
  pOpt = ps[as.logical(apply(indaic, 1, sum))]
  qOpt = qs[as.logical(apply(indaic, 2, sum))]
  bestaicModel <- paste0(glp, "(", pOpt, ",", qOpt, ")" )
  bestaic <- min(aic_model)
  #
  bic_model <- (-2*LL_model) + bicPenalty
  indbic <- min(bic_model)==bic_model
  pOpt = ps[as.logical(apply(indbic, 1, sum))]
  qOpt = qs[as.logical(apply(indbic, 2, sum))]
  bestbicModel <- paste0(glp, "(", pOpt, ",", qOpt, ")" )
  bestbic <- min(bic_model)
  ans <- list(LL=LL_model, artfima_time = totTime, 
              aic=list(bestaic=bestaic, bestaicModel=bestaicModel, 
                       aic=aic_model),
              bic=list(bestbic=bestbic, bestbicModel=bestbicModel, 
                       bic=bic_model)
  )
  ans
}
