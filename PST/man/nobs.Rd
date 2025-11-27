\name{nobs}
\docType{methods}
\alias{lnobs}
\alias{nobs,PSTf-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Extract the number of observations to which a VLMC model is fitted
}
\description{
	The number of observations to which a VLMC model is fitted is notably used for computing the Bayesian information criterion \code{BIC} or the Akaike information criterion with correction for finite sample sizes \code{AICc}.
}
\usage{
\S4method{nobs}{PSTf}(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	A PST, that is an object of class \code{PSTf} as returned by the \code{\link{pstree}} or \code{\link{prune}} method.
}
}
\details{
This is the method for the generic \code{nobs} function provided by the \code{stats4} package. The number of observations to which a VLMC model is fitted is the total number of symbols in the learning sample. If the learning sample contains missing values and the model is learned without including missing values (see \code{\link{pstree}}), the total number of symbols is the number of non-missing states in the sequence(s). This information is used to compute the Bayesian information criterion of a fitted VLMC model. The \code{BIC} generic function calls the \code{\link{logLik}} and \code{nobs} methods for class \code{PSTf}. For more details, see \cite{Gabadinho 2016}.
}
\value{
	An integer containing the number of symbols in the learning sample.
}
\references{
Gabadinho, A. & Ritschard, G. (2016). Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), pp. 1-39.
}
\author{
	Alexis Gabadinho
}

\seealso{
	\code{\link[=stats4]{BIC}}
}
\examples{
data(s1)
s1.seq <- seqdef(s1)
S1 <- pstree(s1.seq, L=3)
nobs(S1)

## Self rated health sequences
## Loading the 'SRH' data frame and 'SRH.seq' sequence object
data(SRH)

## model without considering missing states
## model with max. order 2 to reduce computing time
## nobs is the same whatever L and nmin
m1 <- pstree(SRH.seq, L=2, nmin=30, ymin=0.001)
nobs(m1)

## considering missing states, hence nobs is higher
m2 <- pstree(SRH.seq, L=2, nmin=30, ymin=0.001, with.missing=TRUE)
nobs(m2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
