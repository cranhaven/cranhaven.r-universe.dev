#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################

#' S3 class AM_prior
#' @description Object type returned by \code{AM_prior_*} commands.
#' @seealso \code{\link{AM_prior_K_Delta}}, \code{\link{AM_prior_K_Pois}},  \code{\link{AM_prior_K_NegBin}}
#' @name AM_prior
#' @return \code{\link{AM_prior}}
NULL






#' plot AM_prior
#' 
#'
#' plot the prior on the number of clusters for a given \code{\link{AM_prior}} object.
#'  
#'@param x an \code{\link{AM_prior}} object. See \code{\link{AM_prior_K_Delta}}, \code{\link{AM_prior_K_NegBin}},
#'			\code{\link{AM_prior_K_Pois}} for more details.
#'@param ... all additional parameters are ignored.
#'@return NULL. Called for side effects.
#'  
#'@method plot AM_prior 
#'@importFrom graphics image
#'@importFrom graphics par
#'@importFrom grDevices gray.colors
#'@export
plot.AM_prior=function(x,...){
	oldpar <- par(no.readonly = TRUE)   
  	on.exit(par(oldpar)) 
	n = length(x)
	par(mar=c(3,3,1.5,0.5)+0.1)
	par(mgp=c(2, 1.0, 0))
	title = sprintf("Prior on the number of clusters,\nn=%d with %s", n, attr(x,'type'))
	plot(1:n, x, type = "n", bty = "l", xlab = "k", 
			ylab = expression(paste("P(",K[n],"=k)")),
			main = title,
			xlim=c(.5,20),cex.main=0.8)
	lines(1:n,x,type="h",lwd=3)
	#legend(x="topright",legend=c("INFORMATION"),col=c(1,2,3),lty=1,lwd=2,cex=1.5)
}


#'  summary information of the AM_prior object 
#'  
#'
#' Given an \code{\link{AM_prior}} object, this function prints the summary information of the specified prior on the number of clusters. 
#'  
#'@param object an \code{\link{AM_prior}} object. See \code{\link{AM_prior_K_Delta}}, \code{\link{AM_prior_K_NegBin}},
#'			\code{\link{AM_prior_K_Pois}} for more details.
#'@param ... all additional parameters are ignored.
#'@return NULL. Called for side effects. 
#'  
#'@method summary AM_prior 
#'@seealso \code{\link{AM_prior}}
#'@export
summary.AM_prior = function(object, ...){
	quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975) 
	cat("\n", "AM_Prior = ", attr(object,'type'), "\n", sep = "")	
	cat(" - n    = ", attr(object,'n'), "\n", sep = "")
	cat(" - mean = ", mean(object, na.rm = TRUE), "\n", sep = "")	
	cat(" - var  = ", var(object, na.rm = TRUE), "\n", sep = "")	
	cat(" - Quantiles:\n\n")
	print(quantile(object, quantiles), ...)
	cat("\n")
}



#' Computes the prior number of clusters
#'
#'
#' This function computes the prior on the number of clusters, i.e. occupied components of the mixture for a Finite Dirichlet process when the prior on the component-weights of the mixture is a 
#' Dirichlet with parameter \code{gamma} (i.e. when unnormalized weights are distributed as Gamma(\eqn{\gamma},1)). This function can be used when the prior on the number of 
#' components is Shifted Poisson of parameter \code{Lambda}. See \insertCite{argiento2019infinity}{AntMAN} for more details.
#' 
#' There are no default values.
#'
#' @param n        The sample size.
#' @param Lambda   The \code{Lambda} parameter of the Poisson.
#' @param gamma    The \code{gamma} parameter of the Dirichlet distribution. 
#'
#' @return an \code{\link{AM_prior}} object, that is a vector of length n, reporting the values of the prior on the number of clusters induced by the prior on \code{M} and \code{w}, i.e. \code{p^*_k} for \code{k=1,...,n}. See \insertCite{argiento2019infinity}{AntMAN} for more details.
#'
#' @keywords prior number of clusters
#'
#' @export
#' 
#' @examples
#' n <- 82
#' Lambda <- 10
#' gam_po <- 0.1550195
#' prior_K_po <-  AM_prior_K_Pois(n,gam_po,Lambda)
#' plot(prior_K_po)

AM_prior_K_Pois <- function (n,gamma,Lambda) {
	res = (prior_K_Pois(n,gamma,Lambda));
	return ( structure(
					res, 
					type =  sprintf("%s(Gamma=%f,Lambda=%f)","Poisson", gamma,Lambda) ,
					n = n,
					gamma = gamma,
					Lambda = Lambda,
					class = "AM_prior")
	) ;
}



#' computes the prior number of clusters
#'
#'
#' This function computes the prior on the number of clusters, i.e. occupied component of the mixture for a Finite Dirichlet process when the 
#' prior on the component-weights of the mixture is a Dirichlet with parameter \code{gamma} (i.e. when unnormalized weights are distributed as 
#' Gamma(\eqn{\gamma},1)). This function can be used when the prior on the number of components is Negative Binomial with parameter \eqn{r>0} and
#' \eqn{0<p<1}, with mean \eqn{mu =1+ r*p/(1-p)}. See \insertCite{argiento2019infinity}{AntMAN} for more details. 
#' 
#' There are no default values.
#'
#' @param n      The sample size.
#' @param r      The dispersion parameter \code{r} of the Negative Binomial.
#' @param p      The probability of failure parameter \code{p} of the Negative Binomial.
#' @param gamma  The \code{gamma} parameter of the Dirichlet distribution.
#'
#' @return an \code{\link{AM_prior}} object, that is a vector of length n, reporting the values \code{V(n,k)} for \code{k=1,...,n}.
#'
#' @keywords prior number of cluster
#'
#' @export
#' 
#' @examples
#' n <- 50
#' gamma <- 1
#' r <- 0.1
#' p <- 0.91
#' gam_nb <- 0.2381641
#' prior_K_nb <-  AM_prior_K_NegBin(n,gam_nb,r,p)
#' plot(prior_K_nb)

AM_prior_K_NegBin <- function (n,gamma, r, p){
	res = (prior_K_NegBin(n,gamma, r, p));
	return ( structure(
					res, 
					type =  sprintf("%s(Gamma=%f,r=%f,p=%f)","NegBin", gamma,r,p) ,
					n = n,
					gamma = gamma,
					r = r,
					p = p,
					class = "AM_prior")
	) ;
}



#' Computes the prior on the number of clusters
#'
#'
#' This function computes the prior on the number of clusters, i.e. occupied components of the mixture for a Finite Dirichlet process 
#' when the prior on the component-weights of the mixture is a Dirichlet with parameter \code{gamma} (i.e. when unnormalised weights 
#' are distributed as Gamma(\eqn{\gamma},1)). This function can be used when the number of components is fixed to \eqn{M^*}, i.e. 
#' a Dirac prior assigning mass only to \eqn{M^*} is assumed. See \insertCite{argiento2019infinity}{AntMAN} There are no default values.
#'
#' @param n        The sample size.
#' @param Mstar    The number of component of the mixture. 
#' @param gamma    The \code{gamma} parameter of the Dirichlet distribution. 
#'
#' @return an \code{\link{AM_prior}} object, that is a vector of length n, reporting the values \code{V(n,k)} for \code{k=1,...,n}.
#'
#' @keywords prior number of cluster
#'
#' @export
#' 
#' @examples
#' n <- 82
#' gam_de <- 0.1743555
#' Mstar <- 12
#' prior_K_de <- AM_prior_K_Delta(n,gam_de, Mstar)
#' plot(prior_K_de)

AM_prior_K_Delta <- function (n,gamma,Mstar){
	res = prior_K_Delta(n,gamma,Mstar);
	return ( structure(
					res, 
					type =  sprintf("%s(Gamma=%f,Mstar=%d)","Delta", gamma,Mstar) ,
					n = n,
					gamma = gamma,
					Mstar = Mstar,
					class = "AM_prior")
	) ;
}

