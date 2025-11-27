\name{pmine}
\docType{methods}
\alias{pmine}
%% \alias{pmine-methods}
\alias{pmine,PSTf,stslist-method}
\title{ PST based pattern mining }
\description{
Mine for (sub)sequences satisfying user defined criteria in a state sequence object 
}

\usage{
\S4method{pmine}{PSTf,stslist}(object, data, l, pmin=0, pmax=1, prefix, lag, average=FALSE,
output="sequences", with.prefix=TRUE, sorted=TRUE, decreasing=TRUE, score.norm=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	A fitted PST, that is an object of class PSTf as returned by the \code{\link{pstree}} or \code{\link{prune}} method.
}

  \item{data}{
	A sequence object of class 'stslist' as defined with the \code{seqdef} function of the \code{\link{TraMineR}} library.
}
  \item{l}{
	integer. Length of the subsequence to search for.
}
  \item{pmin}{
	numeric. (Sub)-sequences having average or per state probability greater or equal than \code{pmin} are selected. Default to 1, meaning no lower threshold for the probability.  
}
  \item{pmax}{
	numeric. (Sub)-sequences having average or per state probability less or equal than \code{pmax} are selected. Default to 1, meaning no upper threshold for the probability.  
}
  \item{prefix}{
	character. Subsequences are searched in sequences starting with \code{'prefix'}, where \code{'prefix'} is a string representing a subsequence with states separated by \code{'-'}. This option can be used to search for -most- likely patterns in sequences starting with 'prefix'. 
}
  \item{lag}{
	integer. The \code{lag} first states in the sequence are omitted. If \code{prefix} is 
}
  \item{average}{
	logical. If \code{TRUE}, the \code{pmin} or \code{pmax} probability is supposed to be the average state probability in the (sub)sequence. If \code{FALSE} (sub)sequences having every state probability less than \code{pmax} or greater than \code{pmin} are selected.
}
  \item{output}{
	character. If \code{output='sequences'} the whole sequence(s) where the user defined criteria is satisfied are returned. If \code{output='patterns'} only the (sub)sequences satisfying the user defined criteria are returned.
}
  \item{with.prefix}{
	logical. If \code{'output=patterns'}, should the patterns in the output be preceeded by their prefix, that is by the whole sub-sequence preceding the pattern. 
}
  \item{sorted}{
	logical. If \code{'sorted=TRUE'}, selected patterns or sequences are sorted according to their score, i.e., their average probability.
}
  \item{decreasing}{
	logical. If \code{'sorted=TRUE'}, should sort order be decreasing or increasing ?
}
	\item{score.norm}{
		logical. If \code{TRUE}, the score attached to each selected pattern or (sub)-sequence (the weights in the returned sequence object) is the average per state probability, and is thus normalized by the length of the pattern. If \code{FALSE}, the score is the whole (sub)-sequence probability.
}
}
\details{
The likelihood \eqn{P^{S}(x)} of a whole sequence \eqn{x} is computed from the state probabilities at each position in the sequence. However, the likelihood of the first states is usually lower than at higher position due to a reduced memory available for prediction. A sequence may not appear as very likely if its first state has a low relative frequency, even if the model predicts high probabilities for the states at higher positions. 

The \code{pmine} function allows for advanced pattern mining with user defined parameters. It is controlled by the \code{lag} and \code{pmin} arguments. For example, by setting \code{lag=2} and \code{pmin=0.40} (example 1), we select all sequences with average (the geometric mean is used) state probability from position \eqn{lag+1, \ldots, \ell} above \code{pmin}. Instead of considering the average state probability at positions \eqn{lag+1, \ldots, \ell}, it is also possible to select frequent patterns that do not contain any state with probability below the threshold. This prevents from selecting sequences having many states with high probability but one ore several states with a low probability. 

It is also possible to mine the sequence data for frequent patterns of length \eqn{\ell_{j} < \ell}, regardless of the position in the sequence where they occur. By using the \code{output="patterns"} argument, the \code{pmine} function returns the patterns (as a sequence object) instead of the whole set of distinct sequences containing the patterns. Since the probability of a pattern can be different depending on the context (previous states) the returned subsequences also contain the context preceding the pattern. For more details, see \cite{Gabadinho 2016}.
}

\value{
	A state sequence object, that is an object of class \code{stslist}, where weights are the probability score of (sub)sequences. 
}
\references{
Gabadinho, A. & Ritschard, G. (2016). Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), pp. 1-39.
}
\author{
Alexis Gabadinho
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{cmine}} for context mining
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

## pruning
## Cut-offs for 5% and 1% (see ?prune)
C99 <- qchisq(0.99,4-1)/2
actcal.pst.C99 <- prune(actcal.pst, gain="G2", C=C99)

## example 1
pmine(actcal.pst.C99, actcal.seq, pmin=0.4, lag=2)

## example 2: patterns of length 6 having p>=0.6
pmine(actcal.pst.C99, actcal.seq, pmin=0.6, l=6)
}
\keyword{ misc }
