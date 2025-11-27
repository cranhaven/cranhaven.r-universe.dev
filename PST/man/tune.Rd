\name{tune}
\docType{methods}
\alias{tune}
\alias{tune,PSTf-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	AIC, AICc or BIC based model selection 
}
\description{
	Prune a probabilistic suffix tree with a series of cut-offs and select the model having the lowest value of the selected information criterion. Available information criterion are Akaike information criterion (AIC), AIC with a correction for finite sample sizes (AICc) and Bayesian information criterion (BIC).
}
\usage{
\S4method{tune}{PSTf}(object, gain="G2", C, criterion = "AIC", output = "PST")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	a probabilistic suffix tree, i.e., an object of class \code{"\linkS4class{PSTf}"} as returned by the \code{\link{pstree}}, \code{\link{prune}} or \code{\link{tune}} function.
}
  \item{gain}{ 
	character. The gain function used for pruning decisions. See \code{\link{prune}} for details.
}

  \item{C}{ 
	numeric. A vector of cutoff values. See \code{\link{prune}} for details.
}
  \item{criterion}{
	The criterion used to select the model, either AIC, AICc or BIC. AICc should be used when the ratio between the number of observations and the number of estimated parameters is low, which is often the case with VLMC models. \cite{Burnham et al 2004} suggest to use AICc instead of AIC when the ratio is lower than 40.
}
  \item{output}{
	If \code{output='PST'} the PST (an object of class \code{"\linkS4class{PSTr}"}) having the lowest AIC, AICc or BIC value. If \code{output='stats'}, a table with the statistics for each model obtained by pruning \code{object} with the cut-offs in \code{C}. 
}
}
\details{
The \code{tune} function selects among a series of PST pruned with different values of the \eqn{C} cutoff the model having the lowest \eqn{AIC} or \eqn{AIC_{c}} value. The function can return either the selected PST or a data frame containing the statistics for each model. For more details, see \cite{Gabadinho 2016}.
}
\value{
	If \code{output="PST"} a PST that is an object of class \code{PSTf}. If \code{output="stats"} a matrix with the results of the tuning procedure.\cr 
The selected model is tagged with \code{***}, while models with \eqn{IC < min(IC)+2} are tagged with \code{**}, and models with \eqn{IC < min(IC)+10} are tagged with \code{**}.
}
\references{
Burnham, K. P. & Anderson, D. R. (2004). Multimodel Inference \emph{Sociological Methods & Research}, 33, pp. 261-304.

Gabadinho, A. & Ritschard, G. (2016). Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), pp. 1-39.
}
\author{
Alexis Gabadinho
}

\seealso{
\code{\link{prune}}
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

## Cut-offs for 5% and 1% (see ?prune)
C95 <- qchisq(0.95,4-1)/2
C99 <- qchisq(0.99,4-1)/2

## selecting the optimal PST using AIC criterion
actcal.pst.opt <- tune(actcal.pst, gain="G2", C=c(C95,C99))

## plotting the tree
plot(actcal.pst.opt)
}
\keyword{models}

