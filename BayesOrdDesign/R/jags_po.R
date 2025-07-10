
jags_po_model = function(dat){
  dat.ord <- dat
  dat.ord$treatment <- factor(dat.ord$treatment)
  dat.ord$response <- factor(dat.ord$response)
  fit.prior <- ordinal::clm(response ~ treatment, data=dat.ord, threshold = "flexible")

  modelstring = " model {
  for (i in 1:N){
  mu[i] <- x[i]*delta
  logit(Q[i,1]) <- gamma[1] - mu[i]
  prob[i,1] <- Q[i,1]
  for (c in 2:(C-1)){
  logit(Q[i,c]) <- gamma[c] - mu[i]
  prob[i,c] <- Q[i,c] - Q[i,(c-1)]
  }
  prob[i,C] <- 1-Q[i,(C-1)]
  y[i] ~ dcat(prob[i,1:C])
  }

  for (c in 1:(C-1)){
  gamma.star[c] ~ dnorm(0, 0.1)
  }
  gamma[1:(C-1)] <- sort(gamma.star)
  for (j in 1:4){
  z[j] ~ dnorm(0, 1/(sigma^2))
  }
  delta ~ dnorm(b, sd.prior)
  alpha ~ dnorm(0, 1)
}"
  x <- as.numeric(dat.ord$treatment)-1
  y <- as.numeric(dat.ord$response)
  data <- list(x = x, y = y, N = length(x),
               C = length(unique(dat.ord$response)))
  hyper <- list(#gamma1.prior = as.numeric(fit.prior$coefficients[1]),
                #gamma5.prior = as.numeric(fit.prior$coefficients[5]),
                b = as.numeric(fit.prior$beta),
                sd.prior = sqrt(fit.prior$vcov[data$C, data$C]),
                sigma = 1.5
                )
  init = list( gamma.star = rnorm(data$C-1), delta = rnorm(1), z = rnorm(4))
  model = rjags::jags.model(textConnection(modelstring), data = append(data, hyper),
                     n.chains = 3, inits = init, n.adapt = 500)
  update(model, n.iter = 2000)
  output=rjags::coda.samples(model = model, variable.names = c("gamma", "delta", "z"),
                      n.iter = 20000, thin = 1)
  ess = coda::effectiveSize(output)
  estimation = summary(output)$statistics
  SummaryOutput = list(estimation, ess, output)
  names(SummaryOutput) = c("EST", "ESS","Samples")
  return(SummaryOutput)
}


