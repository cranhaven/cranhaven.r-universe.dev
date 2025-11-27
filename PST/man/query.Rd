\name{query}
\docType{methods}
\alias{query}
%% \alias{query-methods}
\alias{query,PSTf-method}
\alias{round,cprobd-method}
\title{
	Retrieve counts or next symbol probability distribution
}
\description{
	Retrieve counts or next symbol probability distribution from a node of a probabilistic suffix tree
}
\usage{
\S4method{query}{PSTf}(object, context, state, output = "prob", exact = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	A probabilistic suffix tree, i.e an object of class \code{"\linkS4class{PSTf}"}) as returned by the \code{\link{pstree}}, \code{\link{prune}} or \code{\link{tune}} function.
}
  \item{context}{
	Character. The string labelling the node to retrieve. States must be separated by '-' as for example in 'a-a-b'. If the node labelled with this string does not exist in the tree, the node labelled with the longest suffix is searched for, and so on until an existing node is  found.
}
  \item{state}{
	character. If specified the probability of the specified state is returned instead of the whole distribution.
}
  \item{output}{
	character. If output="prob" the probability distribution (or a single symbol distribution if state is specified) is returned. If output="counts" the counts on which the probability distribution is calculated are returned. If output="all" the node itself is returned, that is an object of class PSTr.
}
  \item{exact}{
	logical. If TRUE, the information is returned only if the node labelled with context is present in the tree. That is, the longest suffix of context is not searched for if context is not in the tree.
}
}

\details{
The PST is searched for the node labelled with \code{context}. If \code{exact=FALSE}, when the node does not exist the PST is searched for the longest suffix of \code{context}, and so on until a node corresponding to a suffix of \code{context} is found or the root node is reached. For more details, see \cite{Gabadinho 2016}.
}

\value{
	An object of class \code{cprobd}, with available \code{round} method.
}
\references{
Gabadinho, A. & Ritschard, G. (2016). Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), pp. 1-39.
}
\author{
Alexis Gabadinho
}

\seealso{
 \code{\link{cplot}}, \code{\link{ppplot}}
}
\examples{
data(s1)
s1 <- seqdef(s1)
S1 <- pstree(s1, L=3)
## Retrieving from the node labelled 'a-a-a'
query(S1, "a-a-a")

## The node 'a-b-b-a' is not presetnin the tree, and the next symbol
## probability is retrieved from the node labelled 'b-b-a' (the longest
## suffix
query(S1, "a-b-b-a")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
