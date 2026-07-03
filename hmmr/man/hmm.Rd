\name{hmm}
\alias{hmm}
\alias{lca}

\title{

Fit hidden Markov and latent class models.

}

\description{

\code{hmm} fits a hidden Markov model to its first argument.  
\code{lca} fits a latent class model or mixture model to its first
argument.  

Both functions provide an easy user-interface to the functions
provided in \pkg{depmixS4} by automagically setting some argument values.

}

\usage{

hmm(data, nstates, fit = TRUE, ntimes = NULL, family = NULL, verbose=FALSE, ...)
lca(data, nclasses, fit = TRUE, family = NULL, verbose=FALSE, ...)

}

\arguments{
  
	\item{data}{(columns of) a \code{data.frame} or \code{matrix} like
	object.}
  
	\item{nstates}{the required number of states of the hidden Markov
	model.}
	
	\item{nclasses}{the required number of classes of the mixture or latent
	class model.}

	\item{fit}{\code{logical} indicating whether the model needs to be
	fitted or returned unfitted; the latter is necessary if one wants to
	set constraints on the parameters, which then requires using the
	\code{fit} function from \pkg{depmixS4}.}
  
	\item{ntimes}{the lengths of time series in argument \code{data}; it
	defaults to assuming a single time series of length \code{nrow(data)}.}
  
	\item{family}{(a list of) name(s) of the distribution(s) to be used in
	fitting; if provided, it should have length of the number of the number
	of columns in \code{data}, see Details.}
	
	\item{verbose}{\code{logical}; when \code{TRUE} iteration information
	of the fitting process is printed.}
	
	\item{\dots}{not currently used.}

}

\details{

The distributions used in fitting models are the \code{multinomial} for
\code{factor} data columns and \code{gaussian} for \code{numeric} data
columns.  Data columns are treated as conditionally independent variables.
Use \code{makeDepmix} in the \pkg{depmixS4} package to specify multivariate
distributions.

}

\value{

\code{hmm} returns a \code{depmix} or \code{depmix.fitted} object depending
on the value of the \code{fit} argument; \code{lca} similarly returns
either a \code{mix} or \code{mix.fitted} object.

All these can be \code{print}'ed and \code{summary}'zed.
}

\references{

Visser, I., & Speekenbrink, M. (2010).  depmixS4: an R-package for hidden
Markov models.  Journal of Statistical Software, 36(7), 1-21.

}

\author{
Ingmar Visser
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{

data(conservation)

set.seed(1)
m2 <- lca(conservation$"r1", nclasses=2)
m2
summary(m2)

data(speed1)

set.seed(1)
hm2 <- hmm(speed1$"RT", nstates=2)
hm2
summary(hm2)

}

\keyword{model}
\keyword{statistics}% __ONLY ONE__ keyword per line
