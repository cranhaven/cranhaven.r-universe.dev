
\name{mgdmm}

\alias{mgdmm}
\alias{summary.mgd}

\title{Multi group model specification}

\description{

	\code{mgdmm} creates an object of class \code{mgd}, a
	multi-group model, from a given model of either class \code{dmm} or
	class \code{mixdmm} or lists of these.

}
\usage{

	mgdmm(dmm,ng=1,modname=NULL,trans=FALSE,obser=FALSE,init=FALSE,conpat=NULL)
	\method{summary}{mgd}(object, specs=FALSE, precision=3, se=NULL, ...)
	
}
\arguments{
	
	\item{modname}{A character string with the name of the model, good when
	fitting many models.  Components of mixture models keep their own
	names.  Names are printed in the summary.  Boring default names are
	provided.}
			
	\item{dmm}{Object (or list of objects) of class \code{dmm}; 
	see details below.}
		
	\item{ng}{Number of groups for a multigroup model.}

	\item{trans,obser,init}{Logical arguments specify whether
	\code{trans}ition parameters, \code{obser}vation parameters and
	\code{init}ial state parameters should be estimated equal across
	groups.}
	
	\item{conpat}{Can be used to specify general linear constraints. 
	See \code{\link[depmix]{dmm}} for details.}
	
	\item{precision}{Precision sets the number of digits to be printed in
	the summary functions.}
	
	\item{se}{Vector with standard errors, these are passed on from the 
	summary.fit function if and when ses are available.}
	
	\item{specs,...}{Internal use.}
	
	\item{object}{An object of class \code{mgd}.}
}

\details{ 

	The function \code{mgdmm} can be used to define an \code{mgd}-model or
	multi group \code{dmm}.  Its default behavior is to create \code{ng}
	copies of the \code{dmm} argument, thereby providing identical starting
	values for each group's model.  If the \code{dmm} argument is a list of
	models of length \code{ng}, the starting values of those models will be
	used instead.  This may save quite some cpu time when fitting large
	models by providing the parameter values of separately fitted models as
	starting values.  Currently, \code{depmix} does not automatically
	generate starting values for multi group models.
	
}
\value{

\code{mgdmm} returns an object of class \code{mgd} which contains all the
fields of an object of class \code{dmm} and the following extra:

	\item{ng}{\code{ng} is the number of groups in the multigroup model.}

	\item{mixmod}{\code{mixmod} is a list of length \code{ng} of \code{mixdmm} 
	models for each group.}
	
	\item{itemtypes}{See above.}
	
	\item{npars,freepars,pars,fixed,A,bl,bu}{The same as above but now for
	the combined model, here npars equals the sum of npars of the component
	models plus the mixing proportions.}

}

\author{Ingmar Visser \email{i.visser@uva.nl}} 

\seealso{\code{\link[depmix]{dmm}} on defining single component models, and
\code{\link[depmix]{mixdmm}} for defining mixtures of \code{dmm}'s.}

\examples{

# create a 2 state model with one continuous and one binary response
# with start values provided in st
st <- c(1,0.9,0.1,0.2,0.8,2,1,0.7,0.3,5,2,0.2,0.8,0.5,0.5)
mod <- dmm(nsta=2,itemt=c(1,2), stval=st)

# define 3-group model with equal transition parameters, and no 
# equalities between the obser parameters
mgr <- mgdmm(dmm=mod,ng=3,trans=TRUE,obser=FALSE)
summary(mgr)

}

\keyword{models}


