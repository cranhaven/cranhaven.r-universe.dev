#' Perform reversible-jump MCMC post-process to select appropriate model between
#' proportional odds (PO) model and non-proportional odds (NPO) model
#'
#' @description Performs Bayesian multi-model inference, estimating posterior
#' model probabilities for 2 candidate models.
#' @param g1 specify the bi-jections from the universal parameter psi to PO model parameter set
#' @param ginv1 specify the bi-jections from the PO model parameter set to psi. It is the inverse transformation of g1.
#' @param g2 specify the bi-jections from the universal parameter psi to NPO model parameter set
#' @param ginv2 specify the bi-jections from the NPO model parameter set to psi. It is the inverse transformation of g2.
#' @param or_alt effect size to be detected (under H_1)
#' in terms of odds ratio
#' @param sd the standard error
#' @param pro_ctr distribution of clinical categories for the
#' control group
#' @param n sample size for each group and each interim look
#' @param U the desirability of each outcome level
#' @return rjmcmc_func() returns the selection probabilities for PO and NPO model
#' @export
#'
#' @examples
#' \donttest{
#'
#' g1 = function(psi){
#'   w = sum(psi[6:10])/5
#'   theta = c(psi[1], psi[2], psi[3], psi[4], psi[5],
#'             w, w-psi[7], w-psi[8], w-psi[9], w-psi[10])
#'   return(theta)
#' }
#'
#' ginv1 = function(theta){
#'
#'   w = sum(theta[6:10])
#'   psi = c(theta[1], theta[2], theta[3], theta[4], theta[5],
#'           w, theta[6]-theta[7], theta[6]-theta[8],
#'           theta[6]-theta[9], theta[6]-theta[10])
#'   return(psi)
#' }
#'
#'
#' g2 = function(psi){
#'   theta = psi
#'   return(theta)
#' }
#' ginv2 = function(theta){
#'   psi = theta
#'   return(psi)
#' }
#'
#' out = rjmcmc_func(g1, ginv1, g2, ginv2, or_alt = c(1.4,1.4,1.4,1.4,1.4), sd = 0.2,
#'                   pro_ctr = c(0.58,0.05,0.17,0.03,0.04,0.13),
#'                   n = 100, U = c(100,80,65,25,10,0))
#'                   }

rjmcmc_func = function(g1, ginv1, g2, ginv2, or_alt, sd, pro_ctr, n, U){

  dat = data_gene(or_alt, sd, pro_ctr, n)

  L = length(unique(dat$response)) #counts of outcome level
  C = 2*(length(unique(dat$response))-1) # counts of parameters and thresholds

  po_fit = jags_po_model(dat)
  npo_fit = jags_npo_model(dat, U)

  C1 = as.matrix(po_fit$Samples)
  draw1 = function(){
    r = C1[sample(dim(C1)[1], 1, replace=T), ]
    r = r[c(2:L,1,(L+1):dim(C1)[2])]
  }

  C2 = as.matrix(npo_fit$Samples)
  draw2 = function(){
    r = C2[sample(dim(C2)[1], 1, replace=T), c(-11,-12)]
    r = r[c(L:(dim(C2)[2]-2),1:(L-1))]
  }


  x = as.numeric(dat$treatment)-1
  y = as.numeric(dat$response)

  # Likelihood PO
  L1 = function(theta){
    C = length(unique(y))
    N = nrow(dat)
    prob = c()
    for (i in 1:N){
      if (y[i] == 1){prob[i] = plogis(theta[y[i]] - theta[L]*x[i])
      }else if(y[i] == C){
        prob[i] = 1 - plogis(theta[y[i]-1]-theta[L]*x[i])
      }else{
        prob[i] = plogis(theta[y[i]] - theta[L]*x[i]) - plogis(theta[y[i]-1] - theta[L]*x[i])
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
    + dnorm(theta[L], as.numeric(fit.prior.po$beta),
            sqrt(fit.prior.po$vcov[L, L]), log = TRUE)
  }

  # Prior NPO
  p.prior2 = function(theta){
    sum(dnorm(theta[1:(L-1)], 0, 1, log = TRUE))
    + sum(dnorm(theta[L:C], 0, 1, log = TRUE))
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
