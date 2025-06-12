#' Predict functional time series using ARH RKHS.
#' 
#' using an ARH of order 1 obtain 1 step ahead forecast and 1-\eqn{alpha}
#' predictive confidence bands for the forecasted function.
#' 
#' 
#' @param model a arh_rkhs object containing the functional objects and the
#' lambda coefficients of the d dimensional RKHS representation and the
#' autocorrelation operator.
#' @param newdata an optional data frame in which to look for variables with
#' which to predict. If missing, the fitted values are used.
#' @param bands logical variable indicating if the predictive confidence band
#' is computed. Default = FALSE.
#' @param B number of bootstrap replicates for the band construction. Needed if
#' bands = TRUE. Default = 100.
#' @param level confidence level for the band construction. Needed if bands =
#' TRUE. Default = 0.95.
#' @param kvec number of neighbour points to consider in the computation of the
#' minimum entropy set.
#' @return \item{forecast}{1 step ahead forecast.} \item{fitted}{fitted
#' values.} \item{UB}{upper bound of the 1-\eqn{alpha} predictive confidence
#' band.} \item{LB}{lower bound of the 1-\eqn{alpha} predictive confidence
#' band.} \item{bootsrap.pred}{bootstrap pseudo replicates.}
#' \item{bootsrap.pred.inband}{bootstrap pseudo replicates included in the
#' 1-\eqn{alpha} predictive confidence band.} \item{res}{estimation residuals.}
#' @author N. Hernández and J. Cugliari
#' @export
#' @references N. Hernández, J. Cugliari, J. Jacques. Simultaneous Predictive
#' Bands for Functional Time Series using Minimum Entropy Sets. arXiv:2105.13627 (2021).
#' @importFrom stats quantile var
predict_rkhs <-
function(model, newdata, bands=FALSE, B=100, level=0.95, kvec=round(sqrt(2*B))) {  # this will be called as 'predict'
  
  if (missing(newdata) || is.null(newdata)) {
    
    # dimensions
    n = nrow(model$fdata$data); p = ncol(model$fdata$data)
    lambda.prev = model$rho %*% t(model$lambda_cent) + as.numeric(model$lambda_me)
    prev = t(lambda.prev) %*% t(model$fdata$rk$U %*% sqrt(model$fdata$rk$D))
    names <- c('x_n+1', 'x_n', paste0('x_n-', 3:n - 2))
    
  } else {
    
    n = nrow(newdata); p = ncol(newdata)
    fd.newdata <- fdata_rkhs(newdata, rk = model$fdata$rk)
    newdata.aux <- arh_rkhs(fd.newdata)
    lambda.prev = model$rho %*% t(newdata.aux$lambda_cent) + as.numeric(newdata.aux$lambda_me)
    prev = t(lambda.prev) %*% t(model$fdata$rk$U %*% sqrt(model$fdata$rk$D))
    names <- c('x_n+1', 'x_n', paste0('x_n-', 3:n - 2))
    
  }
  
  # forecast and fitted values
  forecast <- matrix(prev[nrow(prev),], nrow = 1)
  prev <- prev[-nrow(prev),]
  rownames(forecast) <- names[1]
  rownames(prev) <- rev(names[2:n])
  result <- list(forecast = forecast,
                 fitted = prev)
  
  if (bands == TRUE) {
    
    # Residuals and resample
    fitted = prev
    res = model$fdata$data[2:n,] - fitted
    res = sweep(res, 2, apply(res, 2, mean))
    
    res.fdata <- fdata_rkhs(curves = res, rk = model$fdata$rk)
    var.coef<-var(res.fdata$lambda)
    desc <- svd(var.coef, nv = 0)
    var.coef.inv <- desc$u %*% diag(sqrt(1/desc$d)) %*% t(desc$u)
    res.coef.sd <- t(var.coef.inv %*% t(res.fdata$lambda))
    res.sd <- res.coef.sd %*% t(model$fdata$rk$U %*% sqrt(model$fdata$rk$D))
    
    nn = n-1
    Boot.id=replicate(B, sample(1:nn, nn, replace=TRUE), simplify=TRUE)
    
    # bootstraped fts
    fd.boot = array(0,dim=c(n,p,B))
    for (i in 1:B){
      fd.boot[1,,i]=model$fdata$data[1,]
      for (j in 2:n){
        lambda.prev = model$rho %*% model$lambda_cent[(j-1),] + as.numeric(model$lambda_me)
        fd.boot[j,,i] = t(lambda.prev) %*% t(model$fdata$rk$U %*% sqrt(model$fdata$rk$D) ) + res.sd[Boot.id[(j-1),i],]
      }
    }
    
    # bootstrap autocorrelation operator and bootstrap predicted functions
    r = nrow(model$rho)
    rho.boot=array(0,dim=c(r,r,B))
    fd.boot.fcast=matrix(0,B,p)
    for (i in 1:B){
      aux.B = fdata_rkhs(fd.boot[,,i], rk = model$fdata$rk)
      fd.boot.fcast[i,] = predict_rkhs(arh_rkhs(aux.B), newdata = model$fdata$data, bands = F)$forecast
    }
    
    # constructing the bands using entropy sets
    lambda.boot.fcast = fdata_rkhs(fd.boot.fcast, rk = model$fdata$rk)$lambda
    MES = entropy(lambda.boot.fcast, alpha=2, scale=F, K=kvec)
    if (length(level)>1) {
      
      UB=LB=UB.c=LB.c=list()
      for (l in 1:length(level)){
        index = which(MES[[1]]<=quantile(MES[[1]],level[l]))
        UB[[l]]        <- apply(fd.boot.fcast[index,],2,max)
        LB[[l]]        <- apply(fd.boot.fcast[index,],2,min)
        UB.c[[l]]      <- forecast+(UB[[l]]-LB[[l]])/2
        LB.c[[l]]      <- forecast-(UB[[l]]-LB[[l]])/2
      }
    } else {
      index     <- which(MES[[1]]<=quantile(MES[[1]],level))
      UB        <- apply(fd.boot.fcast[index,],2,max)
      LB        <- apply(fd.boot.fcast[index,],2,min)
      UB.c      <- as.numeric(forecast+(UB-LB)/2)
      LB.c      <- as.numeric(forecast-(UB-LB)/2)
    }
    names(UB.c)     =names(LB.c)     <-paste0('Band ',level*100,'%')
    
    result$bootsrap.pred = fd.boot.fcast
    result$bootsrap.pred.inband = fd.boot.fcast[index,]
    result$UB      = UB.c
    result$LB      = LB.c
    result$Boot.res = res
    
  }
  return(result)
  
}
