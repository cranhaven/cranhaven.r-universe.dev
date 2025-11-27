\name{summary-methods}
\docType{methods}
\alias{summary,PSTf-method}
\alias{summary,PSTr-method}
\title{ Summary of variable length Markov chain model }
\description{
Summary of a variable length Markov chain model stored in a probabilistic suffix tree.
}

\usage{
\S4method{summary}{PSTf}(object, max.level)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	A PST, that is an object of class \code{PSTf} as returned by the \code{\link{pstree}} or \code{\link{prune}} method.
}
\item{max.level}{ integer. If specified, the summary is computed for the \code{max.level} levels of the tree only. }
}

\value{
	An object of class \code{PST.summary} with following attributes:
  \describe{
	\item{alphabet}{ list of symbols in the alphabet } 
	\item{labels}{ long labels for symbols in the alphabet }
	\item{cpal}{ color palette used to represent each state of the alphabet }
	\item{ns}{number of symbols in the data to which the model was fitted }
	\item{depth}{ maximum depth (order) of the tree }
	\item{nodes}{ number of internal nodes in the PST }	
	\item{leaves}{ number of leaves in the PST }
	\item{freepar}{ number of free parameters in the mode, i.e., (nodes+leaves)*(|A|-1) where |A| is the size of the alphabet }
  }
A \code{show} method is available for displaying objects of class \code{PST.summary}.
}

\author{
	Alexis Gabadinho
}

\examples{
data(s1)
s1.seq <- seqdef(s1)
S1 <- pstree(s1.seq, L=3)
summary(S1)
summary(S1, max.level=2)
}

\keyword{methods}

