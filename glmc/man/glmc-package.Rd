\name{glmc-package}
\alias{glmc-package}
%\alias{glmc}
\docType{package}
\title{
Fitting Generalized Linear Models Subject to Constraints
}
\description{
\code{\link{glmc}} is a collection of functions to 
fit generalized linear models where the parameters are subject to linear
constraints. The model is specified by giving a symbolic description of the
linear predictor, a description of the error distribution, and a matrix of
constraints on the parameters.

For a complete list of the functions, use \code{library(help="glmc")} 
or read the rest of the manual. For a simple demonstration,
use \code{demo(packages="glmc")}.

When publishing results obtained using this package the original authors
are to be cited as:

Mark S. Handcock, Sanjay Chaudhuri, and Michael S. Rendall. 2004 
\emph{glmc: An R package for generalized linear models subject to constraints}.
\cr
All programs derived from this package must cite it.
For complete citation information, use\cr
\code{citation(package="glmc")}.
}
\details{
In many situations information from a sample of individuals
can be supplemented by population level information
on the relationship between a dependent variable and explanatory
variables.
Inclusion of the population level information can reduce bias
and increase the efficiency of the parameter estimates.

Population level information can be incorporated via 
constraints on functions of the model parameters.
In general the constraints are nonlinear making the task of
maximum likelihood estimation harder.
In this package we provide an alternative approach exploiting the
notion of an empirical likelihood.  
Within the framework of generalised linear models, the
population level information corresponds to linear constraints,
which are comparatively easy to handle.  We provide a
two-step algorithm that produces parameter estimates
using only unconstrained estimation.
We also provide computable expressions for the standard errors.

%For detailed information on how to download and install the software,
%go to the \code{\link{glmc}} website:
%\url{http://http://www.stat.washington.edu/handcock/combining}.
%A tutorial, support newsgroup, references and links to further resources are provided there.
}
\author{
Mark S. Handcock \email{handcock@stat.ucla.edu},\cr
Sanjay Chaudhuri \email{sanjay@stat.nus.edu.sg}, and\cr
Michael S. Rendall \email{mrendall@umd.edu}

Maintainer: Mark S. Handcock \email{handcock@stat.washington.edu}
}
\references{
Sanjay Chaudhuri, Mark S. Handcock, and Michael S. Rendall. 2004 
Generalised Linear Models Incorporating Population Level Information: An
Empirical Likelihood Based Approach,
Working Paper, Center for Statistics and the Social Sciences, University of
Washington.
}
\keyword{ package }
\keyword{ models }
