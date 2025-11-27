\name{cmine}
\docType{methods}
\alias{cmine}
\alias{cmine,PSTf-method}
\alias{[,cprobd.list-method}
\alias{[,cprobd.list,ANY,ANY,ANY-method}
\alias{plot,cprobd.list,ANY-method}

\title{ Mining contexts }
\description{
 Extracting contexts in a PST satisfying user defined criterion
}
\usage{
	\S4method{cmine}{PSTf}(object, l, pmin, pmax, state, as.tree=FALSE, delete=TRUE)
}

\arguments{
  \item{object}{
	A probabilistic suffix tree, i.e., an object of class \code{"\linkS4class{PSTf}"} as returned by the \code{\link{pstree}}, \code{\link{prune}} or \code{\link{tune}} function.
}

  \item{l}{
	length of the context to search for.
}
  \item{pmin}{
	numeric. Minimal probability for selecting the (sub)sequence.
}
  \item{pmax}{
	numeric. Maximal probability for selecting the (sub)sequence.
}

  \item{state}{
	character. One or several states of the alphabet for which the (cumulated) probability is greater than \code{pmin} or less than \code{pmax}.
}
	\item{as.tree}{
		logical. If \code{TRUE} the \code{cmine} method returns a subtree of the PST given as input with selected contexts (including their parent nodes, even if these don't statistify the defined criterion). If \code{FALSE} the output is the list of selected contexts. See value.
}
  \item{delete}{
	Logical. If \code{as.tree=TRUE} and \code{delete=FALSE}, the pruned nodes are not removed from the tree but tagged as pruned=FALSE, so that when plotting the pruned tree these nodes wil appear surrounded with red (can be set to another color) lines.
}

}


\section{details}{
The \code{cmine} function searches in the tree for nodes fulfilling certain characteristics, for example contexts that are highly likely to be followed by a given state (see example 1). One can also mine for contexts corresponding to a minimum or maximum probability for several states together (see example 2). For more details, see \cite{Gabadinho 2016}.
}


\examples{
## Loading the SRH.seq sequence object
data(SRH)

## Learning the model
SRH.pst <- pstree(SRH.seq, nmin=30, ymin=0.001)

## Example 1: searching for all contexts yielding a probability of the 
## state G1 (very good health) of at least pmin=0.5
cm1 <- cmine(SRH.pst, pmin=0.5, state="G1")
cm1[1:10]

## Example 2: contexts associated with a high probability of 
## medium or lower self rated health 
cm2 <- cmine(SRH.pst, pmin=0.5, state=c("B1", "B2", "M"))
plot(cm2, tlim=0, main="(a) p(B1,B2,M)>0.5")
}

\value{
	If \code{as.tree=TRUE} a PST, that is an object of class \code{PSTf} which can be printed and plotted; if \code{as.tree=FALSE} a list of contexts with their associated next symbol probability distribution, that is an object of class \code{cprobd.list} for which a \code{plot} method is available. Subscripts can be used to select subsets of the contexts, see examples. 
}
\references{
Gabadinho, A. & Ritschard, G. (2016). Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), pp. 1-39.
}
\author{
Alexis Gabadinho
}

\keyword{methods}
%\keyword{data mining}

