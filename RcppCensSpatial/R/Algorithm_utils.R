
# Estimate parameters using MCEM algorithm
# -----------------------------------------------------------------------------
MCEM_Spatial = function(y, x, cens, LI, LS, coords, init.phi, init.nugget, type.sc, kappa,
                        lower, upper, MaxIter, nMin, nMax, tol, show.SE){
  if (sum(cens)==0) {
    t1 = Sys.time()
    output = Spatial_model(y, x, coords, init.phi, init.nugget, lower, upper, type.sc, kappa, MaxIter, tol, show.SE)
    t2 = Sys.time()
    ptime = t2 - t1

  } else {
    t1 = Sys.time()
    output = MCEMspatial(y, x, cens, LI, LS, coords, init.phi, init.nugget, lower, upper, type.sc, kappa,
                        MaxIter, nMin, nMax, tol, show.SE)
    t2 = Sys.time()
    ptime = t2 - t1
  }
  likeli = log_likelihood(y, cens, LI, LS, x, output$beta, output$sigma2, output$phi, output$tau2,
                          coords, type.sc, kappa)
  if (show.SE){
    output = list(Theta=output$Theta, theta=output$theta, beta=output$beta, sigma2=output$sigma2,
                  phi=output$phi, tau2=output$tau2, EY=output$EY, EYY=output$EYY, SE=output$SE,
                  InfMat=output$InfMat, loglik=likeli$loglik, AIC=likeli$AIC, BIC=likeli$BIC,
                  Iter=output$Iterations, time=ptime, X=x, coord=coords, type=type.sc, kappa=kappa)
  } else {
    output = list(Theta=output$Theta, theta=output$theta, beta=output$beta, sigma2=output$sigma2,
                  phi=output$phi, tau2=output$tau2, EY=output$EY, EYY=output$EYY, loglik=likeli$loglik,
                  AIC=likeli$AIC, BIC=likeli$BIC, Iter=output$Iterations, time=ptime, X=x,
                  coord=coords, type=type.sc, kappa=kappa)
  }
  return (output)
}


# Estimate parameters using SAEM algorithm
# -----------------------------------------------------------------------------
SAEM_Spatial = function(y, x, cens, LI, LS, coords, init.phi, init.nugget, type.sc, kappa,
                        lower, upper, MaxIter, M, pc, tol, show.SE){
  if (sum(cens)==0) {
    t1 = Sys.time()
    output = Spatial_model(y, x, coords, init.phi, init.nugget, lower, upper, type.sc, kappa, MaxIter, tol, show.SE)
    t2 = Sys.time()
    ptime = t2 - t1

  } else {
    t1 = Sys.time()
    output = SAEMspatial(y, x, cens, LI, LS, coords, init.phi, init.nugget, lower, upper, type.sc,
                         kappa, MaxIter, pc, M, tol, show.SE)
    t2 = Sys.time()
    ptime = t2 - t1
  }
  likeli = log_likelihood(y, cens, LI, LS, x, output$beta, output$sigma2, output$phi, output$tau2,
                          coords, type.sc, kappa)
  if (show.SE){
    output = list(Theta=output$Theta, theta=output$theta, beta=output$beta, sigma2=output$sigma2,
                  phi=output$phi, tau2=output$tau2, EY=output$EY, EYY=output$EYY, SE=output$SE,
                  InfMat=output$InfMat, loglik=likeli$loglik, AIC=likeli$AIC, BIC=likeli$BIC,
                  Iter=output$Iterations, time=ptime, X=x, coord=coords, type=type.sc, kappa=kappa)
  } else {
    output = list(Theta=output$Theta, theta=output$theta, beta=output$beta, sigma2=output$sigma2,
                  phi=output$phi, tau2=output$tau2, EY=output$EY, EYY=output$EYY, loglik=likeli$loglik,
                  AIC=likeli$AIC, BIC=likeli$BIC, Iter=output$Iterations, time=ptime, X=x,
                  coord=coords, type=type.sc, kappa=kappa)
  }
  return (output)
}


# Estimate parameters using EM algorithm
# -----------------------------------------------------------------------------
EM_Spatial = function(y, x, cens, LI, LS, coords, init.phi, init.nugget, type.sc, kappa,
                      lower, upper, MaxIter, tol, show.SE){
  if (sum(cens)==0) {
    t1 = Sys.time()
    output = Spatial_model(y, x, coords, init.phi, init.nugget, lower, upper, type.sc, kappa, MaxIter, tol, show.SE)
    t2 = Sys.time()
    ptime = t2 - t1

  } else {
    t1 = Sys.time()
    output = EMspatial(y, x, cens, LI, LS, coords, init.phi, init.nugget, lower, upper, type.sc, kappa, MaxIter, tol, show.SE)
    t2 = Sys.time()
    ptime = t2 - t1
  }
  likeli = log_likelihood(y, cens, LI, LS, x, output$beta, output$sigma2, output$phi, output$tau2,
                          coords, type.sc, kappa)
  if (show.SE){
    output = list(Theta=output$Theta, theta=output$theta, beta=output$beta, sigma2=output$sigma2,
                  phi=output$phi, tau2=output$tau2, EY=output$EY, EYY=output$EYY, SE=output$SE,
                  InfMat=output$InfMat, loglik=likeli$loglik, AIC=likeli$AIC, BIC=likeli$BIC,
                  Iter=output$Iterations, time=ptime, X=x, coord=coords, type=type.sc, kappa=kappa)
  } else {
    output = list(Theta=output$Theta, theta=output$theta, beta=output$beta, sigma2=output$sigma2,
                  phi=output$phi, tau2=output$tau2, EY=output$EY, EYY=output$EYY, loglik=likeli$loglik,
                  AIC=likeli$AIC, BIC=likeli$BIC, Iter=output$Iterations, time=ptime, X=x,
                  coord=coords, type=type.sc, kappa=kappa)
  }
  return (output)
}


# Log-likelihood function
# -----------------------------------------------------------------------------
log_likelihood = function(y, cc, lower, upper, x, beta, sigma2, phi, tau2, coords, type, kappa){
  mean = x%*%beta
  distM = crossdist(coords)
  Variance = varianceMat(phi, tau2, sigma2, kappa, distM, type)$Sigma

  if (sum(cc)==0) {
    logver = dmvnorm(c(y), c(mean), Variance, TRUE, TRUE)
  } else {
    invObs = solve(Variance[cc==0,cc==0])
    meanC = mean[cc==1] + Variance[cc==1,cc==0]%*%invObs%*%(y[cc==0]-mean[cc==0])
    meanC = as.vector(meanC)
    varC = Variance[cc==1,cc==1] - Variance[cc==1,cc==0]%*%invObs%*%Variance[cc==0,cc==1]

    logden2 = log(pmvnorm(lower[cc==1], upper[cc==1], meanC, NULL, varC)[1])
    logver = dmvnorm(c(y[cc==0]), c(mean[cc==0]), as.matrix(Variance[cc==0,cc==0]), TRUE, TRUE) + logden2
  }
  results = list(loglik=logver, AIC=-2*logver+2*(length(beta)+3), BIC=-2*logver+(length(beta)+3)*log(length(y)))
  return (results)
}

# Convergence plot
# -----------------------------------------------------------------------------
plot.convergence = function(model){
  Theta = model$Theta
  X = model$X
  q = length(model$beta)
  Iter = model$Iter

  myplot = vector("list",q+3)
  if (all(X[,1]==1)){ namesE = c(seq(0,(q-1)),0,0,0) } else { namesE = c(seq(1,q),0,0,0) }
  listabeta = rbind(namesE,Theta)
  listabeta = as.list(data.frame(listabeta[,1:q]))
  myplot[1:q] = lapply(listabeta, function(.x) ggplot(data.frame(.x[-1]),aes(x=seq(0,Iter),y=.x[-1])) +
                         geom_line() + labs(x="Iteration", y=bquote(beta[.(.x[1])])) + theme_bw())

  myplot[[q+1]] = ggplot(data.frame(Theta),aes(x=seq(0,Iter),y=Theta[,q+1])) + geom_line() + labs(x="Iteration", y=bquote(sigma^2)) + theme_bw()
  myplot[[q+2]] = ggplot(data.frame(Theta),aes(x=seq(0,Iter),y=Theta[,q+2])) + geom_line() + labs(x="Iteration", y=bquote(phi)) + theme_bw()
  myplot[[q+3]] = ggplot(data.frame(Theta),aes(x=seq(0,Iter),y=Theta[,q+3])) + geom_line() + labs(x="Iteration", y=bquote(tau^2)) + theme_bw()

  grid.arrange(grobs=myplot, ncol=3)
}

# Prediction in new locations
# -----------------------------------------------------------------------------
predict.new = function(model, x.new, coord.new){
  mediaO = model$X%*%model$beta
  mediaP = x.new%*%model$beta
  coordAll = rbind(model$coord, coord.new)
  pInd = c(rep(0,length(mediaO)), rep(1,length(mediaP)))
  distM = crossdist(coordAll)
  Variance = varianceMat(model$phi, model$tau2, model$sigma2, model$kappa, distM, model$type)$Sigma
  invObs = solve(Variance[pInd==0,pInd==0])
  yPred = mediaP + Variance[pInd==1,pInd==0]%*%invObs%*%(model$EY - mediaO)
  varPred = Variance[pInd==1,pInd==1] - Variance[pInd==1,pInd==0]%*%invObs%*%Variance[pInd==0,pInd==1]

  return (list(coord=coord.new, predValues=yPred, sdPred=matrix(sqrt(diag(varPred)))))
}

# Generating spatial censored data
# -----------------------------------------------------------------------------
random.cens = function(beta, sigma2, phi, tau, x, coords, type.cens, pcens, npred, type.S, kappa){
  x = as.matrix(x)
  n = nrow(as.matrix(x))
  distM = crossdist(coords)
  varianceC = varianceMat(phi, tau, sigma2, kappa, distM, type.S)$Sigma
  mediaC = x%*%beta
  y = t(rmvnorm(1, mediaC, varianceC))
  y1 = y[1:(n-npred),]

  if (pcens==0){
    cc = rep(0, n-npred)
    TrainingData = list(y=y1, ci=cc, coords=coords[1:(n-npred),], x=x[1:(n-npred),])

  } else {  # Censored data
    if (type.cens=="left"){
      cutoff = as.numeric(quantile(y1, pcens))
      cc = (y1 <= cutoff) + 0
      y1[cc==1] = cutoff
      LI = rep(-Inf, n-npred)
      LS = rep(cutoff, n-npred)
    } else {
      if (type.cens=="right"){
        cutoff = as.numeric(quantile(y1, 1-pcens))
        cc = (y1 >= cutoff) + 0
        y1[cc==1] = cutoff
        LI = rep(cutoff, n-npred)
        LS = rep(Inf, n-npred)
      }
    } # End if
    TrainingData = list(y=y1, ci=cc, lcl=LI, ucl=LS, coords=coords[1:(n-npred),], x=x[1:(n-npred),])
  } # End if pcens>0

  if (npred > 0){
    y2 = y[(n-npred+1):n,1]
    TestData = list(y=y2, coords=coords[(n-npred+1):n,], x=x[(n-npred+1):n,])
    return(list(Data=TrainingData, TestData=TestData))
  } else {
    return(TrainingData)
  }
}


# Effective range for spatial correlation
#------------------------------------------------------------------------------
Effective.range = function(cor, phi, kappa, Sp.model) {
  Eff.range = EffectiveRange(cor=cor, phi=phi, kappa=kappa, Sp.model=Sp.model)
  Eff.range = as.numeric(Eff.range)
  return (Eff.range)
}
