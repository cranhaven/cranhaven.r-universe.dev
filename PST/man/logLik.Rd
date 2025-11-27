\name{logLik}
\docType{methods}
\alias{logLik}
\alias{logLik,PSTf-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Log-Likelihood of a variable length Markov chain model
}
\description{
	Retrieve the log-likelihood of a fitted VLMC. This is the \code{\link{logLik}} method for objects of class \code{PSTf} returned by the \code{\link{pstree}} and \code{\link{prune}} functions.  
}
\usage{
\S4method{logLik}{PSTf}(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
a probabilistic suffix tree, i.e., an object of class \code{"\linkS4class{PSTf}"} as returned by the \code{\link{pstree}}, \code{\link{prune}} or \code{\link{tune}} function.
}
}
\details{
The likelihood of a learning sample containing \eqn{n} sequences, given a model \eqn{S} fitted to it, is
\deqn{
L(S)=\prod_{i=1}^{n} P^{S}(x^{i})
}
where \eqn{P^{S}(x^{i})} is the probability of the \eqn{i}th observed sequence predicted by \eqn{S}.
Note that the log-likelihood of a VLMC model is not used in the estimation of the model's parameters (see \code{\link{pstree}}). It is obtained once the model is estimated by calling the \code{\link{predict}} function. The value is stored in the \code{logLik} slot of the probabilistic suffix tree representing the model (a \code{PSTf} object returned by the \code{pstree} or \code{prune} function). 
	 The \code{AIC} and \code{BIC} values can also be obtained with the corresponding generic functions, which call \code{logLik} and use its result. For more details, see \cite{Gabadinho 2016}.
}
\value{
	An object of class \code{logLik}, a negative numeric value with the \code{df} (degrees of freedom) attribute containing the number of free parameters of the model.
}
\references{
Gabadinho, A. & Ritschard, G. (2016). Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), pp. 1-39.
}
\author{
	Alexis Gabadinho
}

\seealso{
	\code{\link[=stats4]{AIC}}, \code{\link[=stats4]{BIC}}
}

\examples{
## activity calendar for year 2000
## from the Swiss Household Panel
## see ?actcal
data(actcal)

## selecting individuals aged 20 to 59
actcal <- actcal[actcal$age00>=20 & actcal$age00 <60,]

## defining a sequence object
actcal.lab <- c("> 37 hours", "19-36 hours", "1-18 hours", "no work")
actcal.seq <- seqdef(actcal,13:24,labels=actcal.lab)

## building a PST
actcal.pst <- pstree(actcal.seq, nmin=2, ymin=0.001)
logLik(actcal.pst)

## Cut-offs for 5% and 1% (see ?prune)
C99 <- qchisq(0.99,4-1)/2

## pruning
actcal.pst.C99 <- prune(actcal.pst, gain="G2", C=C99)

## Comparing AIC
AIC(actcal.pst, actcal.pst.C99)
}

\keyword{models}

