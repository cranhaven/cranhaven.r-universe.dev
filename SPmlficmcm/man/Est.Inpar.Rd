\name{Est.Inpar}
\alias{Est.Inpar}
\title{
Computes the initial values 
}
\description{
Computes initial values of the model parameters.
}
\usage{
Est.Inpar(fl, N, gnma, gnch, tab1, typ, p = NULL)
}
\arguments{
 \item{fl}{Model formula.
}
  \item{N}{Numeric vector containing the number of eligible cases( N1 ) and controls ( N0) in the population N = ( N0, N1 ). 
}
  \item{gnma}{Name of mother genotype variable.
}
  \item{gnch}{Name of offspring genotype variable.
}
  \item{tab1}{\code{data.frame} of the database.
}
  \item{typ}{Argument that specifies whether the data are complete (1) or there are missing offspring genotypes (2). 
}
 \item{p}{Disease prevalence. 
}
 }
\details{
The function use logistic regression to evaluate the initial values of the equation parameters given by the formula and uses empirical estimation to compute the initial values of the nonlinear system. For details, see \emph{Nguile-Makao et al., (2015)}.
}
\value{
A list containing components
\item{parms}{Initial values of the model parameters given by the formula and the allelic frequency parameter.}
\item{ma.u}{Initial values of nonlinear system.}
}
\references{
Moliere Nguile-Makao, Alexandre Bureau (2015).Semi-Parametric Maximum Likelihood Method for Interaction in Case-Mother Control-Mother Designs: Package SPmlficmcm. \emph{Journal of Statistical Software}, 68(10), 1-17. doi:10.18637/jss.v068.i10
}
\keyword{ Initial values }
