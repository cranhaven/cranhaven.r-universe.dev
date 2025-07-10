
#' Perform Reversible-Jump MCMC Post-Processing
#' 
#' Performs Bayesian multimodel inference, estimating Bayes factors and 
#' posterior model probabilities for N candidate models. Using the 'universal 
#' parameter' restriction in Barker & Link (2013), RJMCMC is treated as a Gibbs 
#' sampling problem, where the algorithm alternates between updating the model 
#' and the model specific parameters. Transformation Jacobians are computed 
#' using automatic differentiation so do not need to be specified.
#' 
#' @param post.draw A list of N functions that randomly draw from the posterior 
#'   distribution under each model. Generally these functions sample from the 
#'   output of a model fitted using MCMC.
#' @param g A list of N functions specifying the bijections from the universal 
#'   parameter \code{psi} to each model-specific parameter set.
#' @param ginv A list of N functions specifying the bijections from each 
#'   model-specific parameter set to \code{psi}. These are the inverse 
#'   transformations of \code{g}.
#' @param likelihood A list of N functions specifying the log-likelihood 
#'   functions for the data under each model.
#' @param param.prior A list of N functions specifying the log-prior 
#'   distributions for each model-specific parameter vector.
#' @param model.prior A numeric vector of the prior model probabilities. Note 
#'   that this argument is not required to sum to one as it is automatically 
#'   normalised.
#' @param chainlength How many iterations to run the Markov chain for.
#' @param TM.thin How regularly to calculate transition matrices as the chain 
#'   progresses.
#' @param save.all A logical determining whether to save the value of the 
#'   universal parameter at each iteration, as well as the corresponding 
#'   likelihoods, priors and posteriors. If \code{TRUE}, the output object 
#'   occupies significantly more memory.
#' @param progress A logical determining whether a progress bar is drawn.
#' @return Returns an object of class \code{rj} (see \code{\link{rjmethods}}). 
#'   If \code{save.all=TRUE}, the output has named elements \code{result}, 
#'   \code{densities}, \code{psidraws}, \code{progress} and \code{meta}. If 
#'   \code{save.all=FALSE}, the \code{densities} and \code{psidraws} elements 
#'   are omitted.
#'   
#'   \code{result} contains useful point estimates, \code{progress} contains
#'   snapshots of these estimates over time, and \code{meta} contains
#'   information about the function call.
#'   
#' @references Barker, R. J. and Link, W. A. (2013) Bayesian multimodel 
#'   inference by RJMCMC: A Gibbs sampling approach. \emph{The American 
#'   Statistician, 67(3), 150-156}.
#'   
#' @seealso \code{\link{adiff}} \code{\link{getsampler}} 
#'   \code{\link{defaultpost}}
#'   
#' @examples
#' ## Comparing two binomial models -- see Barker & Link (2013) for further details.
#' 
#' y=c(8,16); sumy=sum(y)
#' n=c(20,30); sumn=sum(n)
#' 
#' L1=function(p){if((all(p>=0))&&(all(p<=1))) sum(dbinom(y,n,p,log=TRUE)) else -Inf}
#' L2=function(p){if((p[1]>=0)&&(p[1]<=1)) sum(dbinom(y,n,p[1],log=TRUE)) else -Inf}
#' 
#' g1=function(psi){p=psi}
#' g2=function(psi){w=n[1]/sum(n); p=c(w*psi[1]+(1-w)*psi[2],psi[2])}
#' ginv1=function(p){p}
#' ginv2=function(p){c(sum(n)/n[1]*p[1]-n[2]/n[1]*p[2],p[2])}
#' 
#' p.prior1=function(p){sum(dbeta(p,1,1,log=TRUE))}
#' p.prior2=function(p){dbeta(p[1],1,1,log=TRUE)+dbeta(p[2],17,15,log=TRUE)}
#' 
#' draw1=function(){rbeta(2,y+1,n-y+1)}
#' draw2=function(){c(rbeta(1,sumy+1,sumn-sumy+1),rbeta(1,17,15))}
#' 
#' out=rjmcmcpost(post.draw=list(draw1,draw2), g=list(g1,g2), ginv=list(ginv1,ginv2),
#'                likelihood=list(L1,L2), param.prior=list(p.prior1,p.prior2),
#'                model.prior=c(0.5,0.5), chainlength=1500)
#' 
#' @export
rjmcmcpost=function(post.draw, g, ginv, likelihood, param.prior, model.prior, chainlength=10000, TM.thin=chainlength/10, save.all=FALSE, progress=TRUE){
  n.models = length(post.draw)
  nTM = chainlength/TM.thin; if(nTM<1){ stop("TM.thin must be less than chainlength.") }
  TM = BF = rep(list(matrix(NA, n.models, n.models)), nTM)
  if(save.all){ 
    store = rep(list(matrix(NA, chainlength, n.models*3, dimnames=list(NULL, c(paste0("Posterior M", 1:n.models), paste0("Likelihood M", 1:n.models), paste0("Prior M", 1:n.models))))), n.models)
    psistore = rep(list(matrix(NA, chainlength, length(ginv[[1]](post.draw[[1]]())))), n.models)
  }
    
  message('Reversible-Jump MCMC Post-Processing')
  for(j in 1:n.models){
    message('Row ', j, appendLF=FALSE)
    wuse = trunc(getOption("width")-20L)  # Set up progress bar
    if(progress){ pb = utils::txtProgressBar(min=0, max=chainlength, initial=0, char="*", style=3, width=wuse) }
    
    term = matrix(NA,chainlength,n.models)
    ginverse = ginv[[j]]
    
    for(i in 1:chainlength){   
      cc = post.draw[[j]]()
      psi = ginverse(cc)
      if(save.all){ psistore[[j]][i,] = psi }
      
      for(k in 1:n.models){
        gk = g[[k]]
        like = likelihood[[k]]
        prior = param.prior[[k]]
        p = gk(psi)
        detJ = log(abs(det(attr(adiff(gk, psi), "gradient"))))
        term[i,k] = like(p) + prior(p) + detJ + log(model.prior[k])
        if(save.all){ store[[j]][i, k+n.models*(0:2)] = c(term[i,k], like(p), prior(p)) }
      }
      term[i,] = term[i,] - max(term[i,])
      term[i,] = exp(term[i,])/sum(exp(term[i,]))
      if(any(is.na(term[i,]))){ warning(paste("NAs in chain for model",j)); break }
      if(progress){ utils::setTxtProgressBar(pb, value=i) }
      if(i%%TM.thin == 0){
        TM[[i/TM.thin]][j,]=apply(term[1:i,], 2, mean)
      }
    }
    if(progress){ close(pb) }
  }
  
  prob = matrix(NA,nTM,n.models)
  for(i in 1:nTM){
    ev = eigen(t(TM[[i]]))
    prob.us = ev$vector[,which(abs(ev$values-1) < 1e-8)]
    prob[i,] = prob.us/sum(prob.us)
    for(j in 1:n.models){
      BF[[i]][,j] = prob[i,]/prob[i,j] * model.prior[j]/model.prior
    }
  }
  if(save.all){ return(rj(list(result=list("Transition Matrix" = TM[[nTM]], "Posterior Model Probabilities"=prob[nTM,], 
                                           "Bayes Factors" = BF[[nTM]], "Second Eigenvalue" = ev$value[2]), 
                               densities = store, psidraws = psistore, progress=list(TM=TM, prb=prob), 
                               meta=list(chainlength=chainlength, TM.thin=TM.thin)))) 
  } else {
    return(rj(list(result=list("Transition Matrix" = TM[[nTM]], "Posterior Model Probabilities"=prob[nTM,], 
                               "Bayes Factors" = BF[[nTM]], "Second Eigenvalue" = ev$value[2]), 
                   progress=list(TM=TM, prb=prob), meta=list(chainlength=chainlength, TM.thin=TM.thin))))
  }
}