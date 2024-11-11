\name{fitdmm}

\alias{depmix}
\alias{fitdmm}

\alias{loglike}
\alias{posterior}
\alias{computeSes}
\alias{bootstrap}

\alias{summary.fit}
\alias{oneliner}

\title{Fitting Dependent Mixture Models}

\description{
  	
	\code{fitdmm} fits mixtures of hidden/latent Markov
	models on arbitrary length time series of mixed categorical and
	continuous data.  This includes latent class models and finite mixture
	models (for time series of length 1), which are in effect independent
	mixture models.
	
	\code{posterior} computes the most likely
	latent state sequence for a given dataset and model.
}
\usage{
	fitdmm(dat, dmm, printlevel = 1, poster = TRUE, tdcov = 0,
                 ses = TRUE, method = "optim", vfactor=15, der = 1, iterlim = 100,
                 kmst = !dmm$st, kmrep = 5, postst = FALSE)
	loglike(dat, dmm, tdcov = 0, grad = FALSE, hess = FALSE, set
                 = TRUE, grInd = 0, sca = 1, printlevel = 1)
	posterior(dat,dmm,tdcov=0,printlevel=1)
	computeSes(dat,dmm) 
	bootstrap(object,dat,samples=100, pvalonly=0,\dots)
	\method{summary}{fit}(object, precision=3, fd=1, \dots)
	oneliner(object,precision=3)
	
}
\arguments{
  
	\item{dat}{ An object (or list of objects) of class \code{md}, see
	markovdata.  If dat is a list of objects of class \code{md} a
	multigroup model is fitted on these data sets.}

	\item{dmm}{ An object (or a list of objects) of class \code{dmm}, see
	dmm.  If dmm is a list of objects of class \code{dmm}, these are taken
	to components of a mixture of dmm's model and will be coerced to class
	\code{mixdmm}.  In any case, the model that is fitted a multigroup
	mixture of dmm's with default ngroups=1 and number of components=1.}

	\item{printlevel}{\code{printlevel} controls the output provided by the
	C-routines that are called to optimize the parameters.  The default of
	1 provides minmal output: just the initial and final loglikelihood of
	the model.  Setting higher values will provide more output on the
	progress the iterations.}
	
	\item{poster}{By default posteriors are computed, the result of which
	can be found in fit\$post.}
	
	\item{method}{This is the optimization algorithm that is used.  donlp2
	from the Rdonlp2 package is the default method.  There is optional
	support for NPSOL.}
	
	\item{der}{Specifies whether derivatives are to be used in optimization.}
	
	\item{vfactor}{vfactor controls optimization in optim and nlm. Since in 
	those routines there is no possibility for enforcing constraints, 
	constraints are enforced by adding a penalty term to the loglikelihood. 
	The penalty term is printed at the end of optimization if it is not close
	enough to zero. This may have several reasons. When parameters are 
	estimated at bounds for example. This can be solved by fixing those 
	parameters on their boundary values. When this is not acceptable vfactor 
	may be increased such that the penalty is larger and the probability that 
	they actually hold in the fitted model is correspondingly higher.}
	
	\item{tdcov}{Logical, when set to TRUE, given that the model and data have 
	covariates, the corresponding parameters will be estimated.}

	\item{ses}{Logical, determines whether standard errors are computed after
	optimization.}
	
	\item{iterlim}{The iteration limit for npsol, defaults to 100, which may
	be too low for large models.}
	
	\item{grad}{logical; if TRUE the gradients are returned.}

	\item{hess}{logical; if TRUE the hessian is returned; it is not
	implemented currently and hence setting it to true will produce a 
	warning.}
	
	\item{set}{Whith the default value TRUE, the data and models parameters
	are sent to the C/C++ routines before computing the loglikelihood.
	When set is FALSE, this is not done.  If an incorrect model was set
	earlier in the C-routines this may cause serious errors and/or
	crashes.}
	
	\item{sca}{If set to -1.0 the negative loglikelihood, gradients and
	hessian are returned.}
	
	\item{object}{An object of class \code{fit}, ie the return value of 
	fitdmm.}
	
	\item{kmst,postst}{These arguments control the generation of starting
	values by kmeans and posterior estimates respectively.}

	\item{kmrep}{If no starting values are provided, \code{kmrep} sets of
	starting values are generated using kmeans in appropriate cases. The
	best resulting set of starting values is optimized further.}

	\item{grInd}{Logical argument; if TRUE, individual contributions of
	each independent realization to the gradient vector will be returned.}
	
	\item{fd}{Print the finite difference based standard errors in the summary
	if both those and bootstrapped standard errors are available.}
	
	\item{samples}{The number of samples to be used in bootstrapping.}
	
	\item{pvalonly}{Logical, if 1 only a bootstrapped pvalue is returned and not 
	fitted paramaters to compute standard errors, optimization is truncated
	when the loglikelihood is better than the original loglikelihood.}
		
	\item{precision}{Precision sets the number of digits to be printed in the
	summary functions.}

	\item{...}{Used in summary.}
}
\details{
	
The function \code{fitdmm} optimizes the parameters of a mixture of
\code{dmm}s using a general purpose optimization routine subject to linear
constraints on the parameters. 

} 

\value{

\code{fitdmm} returns an object of class \code{fit} which has a summary
method that prints the summary of the fitted model, and the following fields:
  
	\item{date,timeUsed,totMem}{The date that the model was fitted, the
	time it took to so and the memory usage.}
	
	\item{loglike}{The loglikelihood of the fitted model.}

	\item{aic}{The AIC of the fitted model.}
  
	\item{bic}{The BIC of the fitted model.} 

  	\item{mod}{The fitted model.}
	
	\item{post}{See function posterior for details.}


\code{loglike} returns a list of the following: 

	\item{logl}{The loglikelihood.}

	\item{gr,grset}{\code{gr} contains the gradients. \code{grset} is a logical vector
	giving information as to which gradients are set, currently all gradients are set 
	except the gradients for the mixing proportions.}
	
	\item{hs,hsset}{\code{hs} contains the hessian. \code{hsset} is a logical giving 
	information as to which elements are computed.}

\code{posterior} returns lists of the following: 

	\item{states}{A matrix of dimension 2+sum(nstates) by
	sum(length(ntimes)) containing in the first column the a posteriori
	component, in the second column the a posteriori state and in the
	remaining column the posterior probabilities of all states.}

	\item{comp}{Contains the posterior component number for each
	independent realization; all ones for a single component model.}

\code{computeSes} returns a vector of length \code{npars} with the standard
errors and a matrix \code{hs} with the hessian used to compute them.  The
routine is not fail safe and can produce errors, ie when the (corrected)
hessian is singular; a warning is issued when the hessian is close to being
singular.

\code{bootstrap} returns an object of class \code{fit} with three extra
fields, the bootstrapped standard errors, bse, a matrix with
goodness-of-fit measures of the bootstrap samples, ie logl, AIC and BIC and
pbetter, which is the proportion of bootstrap samples that resulted in
better fits than the original model.

\code{summary.fit} pretty-prints the outputs.

\code{oneliner} returns a vector of loglike, aic, bic, mod$npars,
mod$freepars, date.

} 



\author{Ingmar Visser \email{i.visser@uva.nl}, Development of this pacakge
was supported by European Commission grant 51652 (NEST) and by a VENI grant
from the Dutch Organization for Scientific Research (NWO).}

\references{
	
	Lawrence R. Rabiner (1989).  A tutorial on hidden Markov models and
	selected applications in speech recognition.  \emph{Proceedings of
	IEEE}, 77-2, p.  267-295.
	
	Theodore C. Lystig and James P. Hughes (2002).  Exact computation of
	the observed information matrix for hidden Markov models.
	\emph{Journal of Computational and Graphical Statistics}.
	
} 


\note{\code{fitdmm} fits time series of arbitrary length and mixtures of
\code{dmm}s, where, to the best of my knowledge, other packages are limited
due to the different optimization routines that are commonly used for these
types of models.}


\seealso{\code{\link[depmix]{dmm}},\code{\link[depmix]{markovdata}}}

\examples{

# COMBINED RT AND CORRECT/INCORRECT SCORES from a 'switching' experiment

data(speed)
mod <- dmm(nsta=2,itemt=c(1,2)) # gaussian and binary items
ll <- loglike(speed,mod)
fit1 <- fitdmm(dat=speed,dmm=mod)
summary(fit1)
ll <- loglike(speed,fit1)

# bootstrap
\dontrun{
pst <- posterior(dat=speed,dmm=fit1)
bs <- bootstrap(fit1,speed,samples=50)
} # end not run


# add some constraints using conpat
conpat=rep(1,15)
conpat[1]=0
conpat[14:15]=0
conpat[8:9]=0
# use starting values from the previous model fit, except for the guessing 
# parameters which should really be 0.5
stv=c(1,.896,.104,.084,.916,5.52,.20,.5,.5,6.39,.24,.098,.90,0,1)
mod=dmm(nstates=2,itemt=c("n",2),stval=stv,conpat=conpat)

fit2 <- fitdmm(dat=speed,dmm=mod)
summary(fit2)


# add covariates to the model to incorporate the fact the accuracy pay off changes per trial
# 2-state model with covariates + other constraints

\dontrun{

conpat=rep(1,15)
conpat[1]=0
conpat[8:9]=0
conpat[14:15]=0
conpat[2]=2
conpat[5]=2
stv=c(1,0.9,0.1,0.1,0.9,5.5,0.2,0.5,0.5,6.4,0.25,0.9,0.1,0,1)
tdfix=rep(0,15)
tdfix[2:5]=1
stcov=rep(0,15)
stcov[2:5]=c(-0.4,0.4,0.15,-0.15)

mod<-dmm(nstates=2,itemt=c("n",2),stval=stv,conpat=conpat,tdfix=tdfix,tdst=stcov,
modname="twoboth+cov")

fit3 <- fitdmm(dat=speed,dmm=mod,tdcov=1,der=0,ses=0,vfa=80)
summary(fit3)

# split the data into three time series
data(speed)
r1=markovdata(dat=speed[1:168,],item=itemtypes(speed))
r2=markovdata(dat=speed[169:302,],item=itemtypes(speed))
r3=markovdata(dat=speed[303:439,],item=itemtypes(speed))

# define 2-state model with constraints
conpat=rep(1,15)
conpat[1]=0
conpat[8:9]=0
conpat[14:15]=0
stv=c(1,0.9,0.1,0.1,0.9,5.5,0.2,0.5,0.5,6.4,0.25,0.9,0.1,0,1)
mod<-dmm(nstates=2,itemt=c("n",2),stval=stv,conpat=conpat)

# define 3-group model with equal transition parameters, and no 
# equalities between the obser parameters
mgr <-mgdmm(dmm=mod,ng=3,trans=TRUE,obser=FALSE)

fitmg <- fitdmm(dat=list(r1,r2,r3),dmm=mgr)
summary(fitmg)

} # end not run

# LEARNING DATA AND MODELS (with absorbing states)

\dontrun{

data(discrimination)

# all or none model with error prob in the learned state
fixed = c(0,0,0,1,1,1,1,0,0,0,0)
stv = c(1,1,0,0.03,0.97,0.1,0.9,0.5,0.5,0,1)
allor <- dmm(nstates=2,itemtypes=2,fixed=fixed,stval=stv,modname="All-or-none")

# Concept identification model: learning only after an error
st=c(1,1,0,0,0,0.5,0.5,0.5,0.25,0.25,0.05,0.95,0,1,1,0,0.25,0.375,0.375)
# fix some parameters
fx=rep(0,19)
fx[8:12]=1
fx[17:19]=1
# add a couple of constraints
conr1 <- rep(0,19)
conr1[9]=1
conr1[10]=-1
conr2 <- rep(0,19)
conr2[18]=1
conr2[19]=-1
conr3 <- rep(0,19)
conr3[8]=1
conr3[17]=-2
conr=c(conr1,conr2,conr3)
cim <- dmm(nstates=3,itemtypes=2,fixed=fx,conrows=conr,stval=st,modname="CIM")

# define a mixture of the above models ...
mix <- mixdmm(dmm=list(allor,cim),modname="MixAllCim")

# ... and fit it on the combined data discrimination
fitmix <- fitdmm(discrimination,mix)
summary(fitmix)

} # end not run

}

\keyword{models} 
