\name{par.dist.default}
\alias{par.dist.default}

\title{
Create default parameters for a distribution
}

\description{
A distribution for knowledge about the values under H1 is assumed.  The function \emph{par.dist.default} creates default values for the parameters of the distribution. 
}
\usage{
par.dist.default(dist, p.0)
}

\arguments{
  \item{dist}{The name of the distribution. Possible values are \dQuote{triangular} (triangular distribution) and \dQuote{tr.normal} (truncated normal distribution).
  
The value \dQuote{usersupplied} generates a parameter structure that the caller must first fill in with his own list of p.1s (see the examples below).   
}
  \item{p.0}{The PD values under H0. The default parameter values are derived from p.0
}
}
\details{
The structure of the parameters depends on the distribution.  The function \emph{par.dist.default} creates default values for the parameters.  These defaults can be changed but a call to the function is required  such that the structure of the parameters is known. 

The return value is to be used in a call to \code{\link{sample.knowledge.H1}} (also for the \dQuote{usersupplied}) distribution. 
}
\value{ The function returns a list with elements
\item{dist }{the distribution choosen}
\item{p.0 }{the value of p.0 that was used}
\item{param }{ 

For a triangular distribution these are a (The lower value of the base of the triangle), b (The heigher value of the base of the triangle) and mode (The mode of the triangle).
There is a value for a, b and mode for each rating class. 
  
For a truncated normal distribution the lowest value for the truncation (for each class), the highest value for truncation (for each class), the mean and the standard deviations (both for each class). 

For a \dQuote{usersupplied} distribution only the parameter structure is created, param is left NA.  The user should fill in an array of H1 values in param. 
}
}

\seealso{
\code{\link{sample.knowledge.H1}}
}

\examples{
par.trian<-par.dist.default(dist="triangular", p.0=c(0.001, 0.004))

par.trnormal<-par.dist.default(dist="tr.normal", p.0=c(0.001, 0.004))

par.ownsample<-par.dist.default(dist="usersupplied", p.0=c(0.001, 0.004))
#first fill in your own list of p1.s
par.ownsample$param<-rbind(c(0.001, 0.01), c(0.004, 0.004), c(0.004, 0.01))
# see also \code{simulate.scenario}
}
