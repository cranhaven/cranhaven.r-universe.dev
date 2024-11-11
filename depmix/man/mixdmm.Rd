
\name{mixdmm}

\alias{mixdmm}
\alias{summary.mixdmm}

\title{Mixture of dmm's specification}

\description{

	\code{mixdmm} creates an object of class \code{mixdmm},
	ie a mixture of \code{dmm}'s, given a list of component models of class
	\code{dmm}.

}
\usage{
	
	mixdmm(dmm, modname=NULL, mixprop=NULL, conrows=NULL)
	\method{summary}{mixdmm}(object, specs=FALSE, precision=3, se=NULL, ...)

}
\arguments{
	
	\item{dmm}{A list of \code{dmm} objects to form the mixture.}
	
	\item{modname}{A character string with the name of the model, good when 
	fitting many models. Components of mixture models keep their own names. 
	Names are printed in the summary. Boring default names are provided.}
		
	\item{conrows}{Argument \code{conrows} can be used to specify general
	constraints between parameters.}

	\item{mixprop}{Arugement \code{mixprop} can be used to set the initial
	values of the mixing proportions of a mixture of \code{dmm}'s.}
		
	\item{precision}{Precision sets the number of digits to be printed in the
	summary functions.}
		
	\item{object}{An object of class \code{mixdmm}.}

	\item{specs,...}{Internal use. Not functioning currently.}

	\item{se}{Vector with standard errors, these are passed on from the 
	summary.fit function if and when ses are available.}
}

\details{ 

	The function \code{mixdmm} can be used to define a mixture of
	\code{dmm}'s by providing a list of such objects as argument to this
	function. See the \code{dmm} helpfile on how to use the \code{conrows} 
	argument. Note that it has to be of length \code{npars}, ie including
	all parameters of the model and not just the mixing proportions. 

}

\value{
	
\code{mixdmm} returns an object of class \code{mixdmm} which has the same
fields as a \code{dmm} object.  In addition it has the following fields:
	
	\item{nrcomp}{The number of components of the mixture model.}
	
	\item{mod}{A list of the component models, that is a list of objects of
	class \code{dmm}.}

}

\author{Ingmar Visser \email{i.visser@uva.nl}} 

\seealso{\code{\link[depmix]{dmm}} on defining single component models, and
\code{\link[depmix]{mgdmm}} on defining multi group models.
See \code{\link[depmix]{generate}} for generating data.
}

\examples{

# define component 1
# all or none model with error prob in the learned state
fixed = c(0,0,0,1,1,1,1,0,0,0,0)
stv = c(1,1,0,0.07,0.93,0.9,0.1,0.5,0.5,0,1)
allor <- dmm(nstates=2,itemtypes=2,fixed=fixed,stval=stv,modname="All-or-none")

# define component 2
# Concept identification model: learning only after an error
st=c(1,1,0,0,0,0.5,0.5,0.5,0.25,0.25,0.8,0.2,1,0,0,1,0.25,0.375,0.375)
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

# define a mixture of the above component models
mix <- mixdmm(dmm=list(allor,cim),modname="MixAllCim")
summary(mix)

}

\keyword{models}


