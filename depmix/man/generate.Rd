
\name{generate}

\alias{generate}

\title{Generate data from a dependent mixture model}

\description{
	\code{generate} generates a dataset according to a
	given \code{dmm}.
}

\usage{	
	generate(ntimes,dmm,nreal=1) 

}
\arguments{
	
	\item{ntimes}{The number of repeated measurements, ie the length of the 
	time series (this may be a vector containing the lengths of independent 
	realiazations).}
		
	\item{dmm}{Object of class \code{dmm} or \code{mixdmm}.}
	
	\item{nreal}{The number of independent realizations that is to generated. 
	Each of them will have the dimension of \code{ntimes}; all this does is
	replace ntimes by rep(ntimes,nreal).}

}

\details{ 
	
	\code{generate} generates a date set of the specified dimensions
	\code{ntimes} and \code{nreal} using the parameter values in
	\code{dmm}, which should be an object of class \code{dmm} or
	\code{mixdmm}.  \code{generate} does not handle multi group models,
	which can be run separately.

	This function is used in the \code{bootstrap}'ping routine to compute
	standard errors based on parametric bootstraps.

}
\value{

Generate returns an object of class \code{\link[depmix]{markovdata}}.  The
return object has an attribute called instates, a vector with the starting
states of each realization.  When the model is a mixture the return has
another attribute \code{incomp} containing the components of each realization.

}

\author{Ingmar Visser \email{i.visser@uva.nl}} 

\seealso{\code{\link[depmix]{dmm}}, \code{\link[depmix]{markovdata}}}

\examples{

# create a 2 state model with one continuous and one binary response
# with start values provided in st
st <- c(1,0.9,0.1,0.2,0.8,2,1,0.7,0.3,5,2,0.2,0.8,0.5,0.5)
mod <- dmm(nsta=2,itemt=c(1,2), stval=st)

# generate two series of lengths 100 and 50 respectively using above model
gen<-generate(c(100,50),mod)

summary(gen)
plot(gen)

}

\keyword{datagen} 


