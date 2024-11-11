\name{depmix-internal}

\alias{checkSetRecode}
\alias{recode}
\alias{fblo}
\alias{fbuo}
\alias{ppar}
\alias{recitt}
\alias{pp}
\alias{np}
\alias{pa2conr}
\alias{paridx}
\alias{fresp}
\alias{bdiag}
\alias{cl2st}
\alias{cl2stob}
\alias{kmstart}
\alias{poststart}
\alias{tr2stin}

\title{Depmix utility functions}
\description{
  	
	These functions are used internally by depmix functions. They should not be called
	directly unless you know what you're doing.
}
\usage{
	
	checkSetRecode(dat,dmm,tdcov,printlevel=1)
	recode(dat,xm)
	fblo(x,i,bigB)
	fbuo(x,i,bigB)
	ppar(x,z)
	recitt(itemtypes)
	pp(x)
	np(x)
	pa2conr(x)
	paridx(nstates,itemtypes,mat,idx1=0,idx2=0,it=0,comp=1,group=1)
	fresp(x,pars)
	bdiag(x)
	cl2st(cluster,dat,dmm)
	cl2stob(cluster,dat,dmm)
	kmstart(dat,dmm)
	poststart(dat,dmm)
	tr2stin(sttr)
}
\arguments{
  
	\item{dat,xm}{See markovdata help.}
	\item{dmm,itemtypes,nstates}{See dmm help.}
	\item{cluster}{Some clustering of the data, eg from kmeans or posterior
	estimates that can be used to arrive at starting values for parameters.}
	\item{sttr}{transition matrix starting values.}
	\item{printlevel,tdcov}{See depmix help.}
	\item{x,mat,idx1,idx2,it,comp,group}{A vector/matrix(name)/indices et cetera.}
	\item{i,z,bigB,pars}{More internal stuff.}
	
}
\details{
	Function bdiag takes as argument a list of matrices and returns the 
	blockdiagonal matrix formed from these, and the other entries padded with 
	zeroes. This function is from package assist by Chunlei Ke and Yuedong Wang.
} 

\value{
	Most of these functions are used for their side-effect, ie sending stuff to C-routines,
	or returning recoded stuff (data, itemtypes) et cetera.
}

\author{Ingmar Visser \email{i.visser@uva.nl}} 

\keyword{models} 
