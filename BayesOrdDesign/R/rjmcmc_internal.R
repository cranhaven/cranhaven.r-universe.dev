rjmcmc_inter = function(or_alt, sd, pro_ctr, n, U){


  if(all(or_alt == or_alt[1])){
    dat = data_gene(or_alt, sd, pro_ctr, n)
  }else{
    dat = data_gene_npo(or_alt, sd, pro_ctr, n)
  }


  L = length(unique(dat$response)) #counts of outcome level
  C = 2*(length(unique(dat$response))-1) # counts of parameters and thresholds

  po_fit = jags_po_model(dat)
  npo_fit = jags_npo_model(dat, U)

  C1 = as.matrix(po_fit$Samples)
  draw1 = function(){
    r = C1[sample(dim(C1)[1], 1, replace=T), ]
    #r = r[c(2:6,1,7:10)]
    r = r[c(2:L,1,(L+1):dim(C1)[2])]
  }

  C2 = as.matrix(npo_fit$Samples)
  draw2 = function(){
    r = C2[sample(dim(C2)[1], 1, replace=T), c(-11,-12)]
    #r = r[c(6:10,1:5)]
    r = r[c(L:(dim(C2)[2]-2),1:(L-1))]
  }


  # bijective function
  g1 = function(psi){
    w = sum(psi[L:C])/(C-L+1)
    theta = c(psi[1], psi[2], psi[3], psi[4], psi[5],
              w, w-psi[7], w-psi[8], w-psi[9], w-psi[10])
    return(theta)
  }

  ginv1 = function(theta){
    w = sum(theta[L:C])
    psi = c(theta[1], theta[2], theta[3], theta[4], theta[5],
            w, theta[6]-theta[7], theta[6]-theta[8],
            theta[6]-theta[9], theta[6]-theta[10])
    return(psi)
  }


  g2 = function(psi){
    theta = psi
    return(theta)
  }
  ginv2 = function(theta){
    psi = theta
    return(psi)
  }

  x = as.numeric(dat$treatment)-1
  y = as.numeric(dat$response)

  # Likelihood PO
  L1 = function(theta){
    C = length(unique(y))
    N = nrow(dat)
    prob = c()
    for (i in 1:N){
      if (y[i] == 1){prob[i] = plogis(theta[y[i]] - theta[6]*x[i])
      }else if(y[i] == C){
        prob[i] = 1 - plogis(theta[y[i]-1]-theta[6]*x[i])
      }else{
        prob[i] = plogis(theta[y[i]] - theta[6]*x[i]) - plogis(theta[y[i]-1] - theta[6]*x[i])
      }
    }

    ll = sum(log(prob), na.rm = TRUE)
    return(ll)
  }


  # Likelihood NPO
  L2 = function(theta){
    C = length(unique(y))
    N = nrow(dat)
    prob = c()
    for (i in 1:N){
      if (y[i] == 1){prob[i] = plogis(theta[y[i]] - theta[y[i]+5]*x[i])
      }else if(y[i] == C){
        prob[i] = 1 - plogis(theta[y[i]-1] - theta[y[i]+5-1]*x[i])
      }else{
        prob[i] = plogis(theta[y[i]] - theta[y[i]+5]*x[i]) - plogis(theta[y[i]-1] - theta[y[i]+5-1]*x[i])
      }
    }

    ll = sum(log(prob), na.rm = TRUE)
    return(ll)
  }


  fit.prior.po = ordinal::clm(response ~ treatment,
                     data=dat, threshold = "flexible")

  fit.prior.npo = ordinal::clm(response ~ treatment, nominal = ~ treatment,
                      data=dat, threshold = "flexible")

  # Prior PO
  p.prior1 = function(theta){
    sum(dnorm(theta[1:(L-1)], 0, 1, log = TRUE))
    #+ dnorm(theta[6], 0, 1, log = TRUE)
    + dnorm(theta[L], as.numeric(fit.prior.po$beta),
            sqrt(fit.prior.po$vcov[L, L]), log = TRUE)
    #+ sum(dnorm(theta[7:10], 0, sigma, log = TRUE))
  }

  # Prior NPO
  p.prior2 = function(theta){
    sum(dnorm(theta[1:(L-1)], 0, 1, log = TRUE))
    + sum(dnorm(theta[L:10], 0, 1, log = TRUE))
    #+ sum(dnorm(theta[6:10], -as.numeric(fit.prior.npo$alpha.mat[2,]),
    #            as.vector(sqrt(diag(as.matrix(fit.prior.npo$vcov))[L:dim(fit.prior.npo$vcov)[1]])),
    #            log = TRUE))
  }

  sigma = 1.5

  goals_post = rjmcmc::rjmcmcpost(post.draw = list(draw1, draw2), g = list(g1, g2),
                          ginv = list(ginv1, ginv2), likelihood = list(L1, L2),
                          param.prior = list(p.prior1, p.prior2),
                          model.prior = c(0.5, 0.5), chainlength = 1e3,
                          save.all = TRUE)

  prob = goals_post$result$`Posterior Model Probabilities`
  cat("The probability of choosing PO model is ", prob[1], " while the probability
      of choosing NPO model is ", prob[2], ".", sep = "")

  return(goals_post)
}



