
\name{markovdata}
\alias{markovdata}
\alias{summary.md}
\alias{plot.md}
\alias{print.md}
\alias{plot.ts2}
\alias{dname}
\alias{ntimes}
\alias{itemtypes}
\alias{replicates}
\alias{ncov}
\alias{inames}
\alias{nitems}
\alias{ind}

\title{Specifying Markov data objects}

\description{
	Markovdata creates an object of class \code{md}, to be used 
	by \code{\link[depmix]{fitdmm}}.
}
\usage{

	markovdata(dat, itemtypes, nitems = length(itemtypes), ntimes =
                 length(as.matrix(dat))/nitems, replicates = rep(1,
                 length(ntimes)), inames = NULL, dname = NULL, xm =
                 NA)
				 
	\method{summary}{md}(object, \dots)
	\method{plot}{md}(x, nitems = 1:(min(5, dim(x)[2])), 
			nind = 1:(min(5,length(attributes(x)$ntimes))),\dots)
	\method{print}{md}(x, \dots) 
	
	dname(object)
	ntimes(object)
	itemtypes(object)
	replicates(object)
	
	ncov(object)
	inames(object)
	nitems(object)
	ind(object)
	
}
\arguments{
	\item{dat}{An R object to be coerced to markovdata, a data frame or
	matrix.}

	\item{itemtypes}{A vector providing the types of measurement with
	possible values \sQuote{continuous}, \sQuote{categorical}, and
	\sQuote{covariate}.  This is mainly only used to rearrange the data
	when there are covariates in such a way that the covariate is in the
	last column.  Only one covariate is supported in estimation of models.}
	
	\item{ntimes}{The number of repeated measurements, ie the length of the
	time series (this may be a vector containing the lengths of independent
	realiazations).  It defaults the number of rows of the data frame or
	data matrix.}
	
	\item{replicates}{Using this argument case weights can be provided.
	This is particularly usefull in eg latent class analysis with
	categorical variables when there usually are huge numbers of
	replicates, ie identical response patterns.  \code{depmix} computes the
	raw data log likelihood for each case separately.  Thus, when there are
	many replicates of a case a lot of computation time is saved by
	specifying case weights instead of providing the full data set.}
	
	\item{inames}{The names of items. These default to the column names of 
	matrices or dataframes.}
	
	\item{dname}{The name of the dataset, used in summary, print and plot
	functions.}
  
	\item{xm}{\code{xm} is the missing data code.  It can be any value but
	zero. Missing data are recoded into \code{NA}.}
	
	\item{object,x}{An object of class \code{md}.}
	
	\item{...}{Further arguments passed on to plot and summary.}
	
	\item{nitems,nind}{In the plot function, these arguments control which
	data are to be plotted, ie nitems indicates a range of items, and nind
	a range of realizations, respectively.}
	
} 

\details{ 

The function \code{markovdata} coerces a given data frame or matrix to be
an object of class \code{md} such that it can be used in
\code{\link[depmix]{fitdmm}}.  The \code{md} object has its own summary,
print and plot methods.

The functions dname, itemtypes, ntimes, and replicates retrieve the
respective attributes with these names; similarly \code{ncov, nitems,
inames}, and \code{ind} retrieve the number of covariates, the number of
items (the number of columns of the data), the column names and the number
of \code{ind}ependent realizations respectively.

}
	
\value{ An \code{md}-object is a
		matrix of dimensions sum(ntimes) by nitems, containing the
		measured variables and covariates rearranged such that the
		covariate appears in the last column. The column names are
		\code{inames} and the matrix has three further attributes: 


	\item{dname}{The name of the data set.}	
	
	\item{itemtypes}{See above.}
	
	\item{ntimes}{See above. This will be a vector computed as 
	ntimes=rep(ntimes,nreal).}
	
	\item{replicates}{The number of replications of each case, used as
	weigths in computing the log likelihood.}

}

\author{Ingmar Visser \email{i.visser@uva.nl}} 

\seealso{\code{\link[depmix]{dmm}}, \code{\link[depmix]{depmix}}}

\examples{

x=rnorm(100,10,2)
y=ifelse(runif(100)<0.5,0,1)
z=matrix(c(x,y),100,2)
md=markovdata(z,itemtypes=c("cont","cat"))
summary(md)

data(speed)
summary(speed)
plot(speed,nind=2)

# split the data into three data sets 
# (to perform multi group analysis) 
r1=markovdata(dat=speed[1:168,],item=itemtypes(speed))
r2=markovdata(dat=speed[169:302,],item=itemtypes(speed))
r3=markovdata(dat=speed[303:439,],item=itemtypes(speed))
summary(r2)

}

\keyword{data}
