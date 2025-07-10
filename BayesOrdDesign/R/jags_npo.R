jags_npo_model = function(dat, U){
  dat.ord <- dat
  dat.ord$treatment <- factor(dat.ord$treatment)
  dat.ord$response <- factor(dat.ord$response)
  #fit.prior <- clm(response ~ treatment, nominal = ~ treatment,
  #                 data=dat.ord, threshold = "flexible")
  #print(fit.prior)

  modelstring = " model {
  for (c in 1:(C-1)){
  logit(Q[1,c]) <- gamma[c]
  logit(Q[2,c]) <- gamma[c] - delta[c]
  }
  for (j in 1:2){
  prob[j,1] <- Q[j,1]
  for (c in 2:(C-1)){
  prob[j,c] <- Q[j,c] - Q[j,(c-1)]
  }
  prob[j,C] <- 1- Q[j, (C-1)]
  }
  for (i in 1:N){
  y[i] ~ dcat(prob[x[i],])
  }

  # Prior
  for (c in 1:(C-1)){
  gamma.star[c] ~ dnorm(0, 0.1)
  delta[c] ~ dnorm(b[c], sd.prior[c])
  }
  gamma[1:(C-1)] <- sort(gamma.star)
  # Mean Utilities
  for (j in 1:2){
  u[j] <- inprod(U[1:C], prob[j,1:C])
  }

}"
  x <- as.numeric(dat.ord$treatment)
  y <- as.numeric(dat.ord$response)
  data <- list(x = x, y = y, N = length(x), C = length(unique(dat.ord$response)), U = U)
  #hyper <- list(b = -as.numeric(fit.prior$alpha.mat[2, 1:(data$C-1)]),
  #              sd.prior = as.vector(sqrt(diag(as.matrix(fit.prior$vcov))[data$C:dim(fit.prior$vcov)[1]]))
  #)
  C = length(unique(dat.ord$response))
  hyper <-list(b=rep(0, (C-1)), sd.prior = rep(0.1,(C-1)))
  init = list(delta = rep(0,(C-1)), gamma.star = rnorm(C-1))
  model = rjags::jags.model(textConnection(modelstring), data = append(data, hyper),
                     n.chains = 1, inits = init, n.adapt = 500)
  update(model, n.iter = 2000)
  output=rjags::coda.samples(model = model, variable.names = c( "gamma","delta", "u"),
                      n.iter = 20000, thin = 1)
  ess = coda::effectiveSize(output)
  estimation = summary(output)$statistics
  samples = as.matrix(output)
  pp.u = mean(samples[,dim(samples)[2]] > samples[,(dim(samples)[2]-1)])
  SummaryOutput = list(estimation, ess, output, pp.u)
  names(SummaryOutput) = c("EST", "ESS", "Samples", "PPUtilities")
  return(SummaryOutput)
}



