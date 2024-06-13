



samplePosterior <- function(parms_prior, data, model, measure, n_samples, mcmc_initial){
  if(model == 'Independent')
    return(samplePosteriorIndependent(parms_prior, data, measure, n_samples))
  if(model == 'Sarmanov')
    return(samplePosteriorSarmanov(parms_prior, data, measure, n_samples, mcmc_initial))

}


samplePosteriorIndependent <- function(parms, data, measure, n_samples) {

  parms_posterior <- parmsPosterior(parms, data)
  p1 <- rbeta(n_samples, shape1=parms_posterior['alpha1'], shape2=parms_posterior['beta1'])
  p2 <- rbeta(n_samples, shape1=parms_posterior['alpha2'], shape2=parms_posterior['beta2'])

  if (measure=="OR")
    return(p2/(1-p2)*(1-p1)/p1)
  if (measure=="RR")
    return(p2/p1)
  if (measure=="RD")
    return(p2-p1)
}




samplePosteriorSarmanov<-function(parms, data,measure,n_samples, mcmc_initial) {


  parms_posterior  <- parmsPosterior(parms, data)
  samples <- arms(mcmc_initial,
                  logDensitySarmanovP1P2,
                  supoort,
                  n_samples,
                  parms_posterior)

  if (measure=="OR")
    return(samples[,2]/(1-samples[,2])*(1-samples[,1])/samples[,1])
  if (measure=="RR")
    return(samples[,2]/samples[,1])
  if (measure=="RD")
    return(samples[,2]-samples[,1])

}



logDensitySarmanovP1P2 <- function(x,parm){
  p1=x[1]
  p2=x[2]
  a1=parm[1]
  b1=parm[2]
  a2=parm[3]
  b2=parm[4]
  rho=parm[5]
  mu1 <- a1/(a1+b1)
  mu2 <- a2/(a2+b2)
  delta1 <- sqrt(mu1*(1-mu1)/(a1+b1+1))
  delta2 <- sqrt(mu2*(1-mu2)/(a2+b2+1))
  return(log(dbeta(p1,shape1=a1,shape2=b1)*dbeta(p2,shape1=a2,shape2=b2)))
}

