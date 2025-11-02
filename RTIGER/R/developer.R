# Psi should be in the same form as the observations in the normal function
# List with samples with lists with Chromosome with a nstates x T Matrix

#'
#' Function to developers. It runs one EM step
#'
#' @param psi list of psi probabilities.
#' @param rigidity Rigidity value.
#' @param nstates Number of states.
#' @param transition transition matrix
#' @param start initial probabilities
#'
#' @return List with updates probabilites
#'
#'
#' @usage dev(psi, rigidity = NULL, nstates = 3, transition = NULL, start = NULL)
#'
#'
#'
#' @export dev
#'
dev= function(psi,rigidity=NULL,nstates=3,transition=NULL,start=NULL){

  #Checks:
  if(is.null(rigidity)) stop("Rigidity must be specified. This is a data specific parameter. Check vignette.\n")
  if(!is.integer(rigidity))  rigidity = as.integer(rigidity)
  if(!is.integer(nstates))  nstates = as.integer(nstates)

  parameter=list(rigidity=rigidity,nstates=nstates,transition=transition,pi=start)
  fitted=julia_call("EMdev",psi,parameter)
  gamma=fitted$gamma
  newpara=fitted$parameter
  newtransition=newpara$transition
  newstart=newpara$pi
  return (list(gamma=gamma,transitionUpdate=newtransition,startUpdate=newstart))

}


