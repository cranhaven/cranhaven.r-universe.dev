\name{nodenames}
\docType{methods}
\alias{nodenames}
%% \alias{nodenames-methods}
\alias{nodenames,PSTf-method}
\title{
	Retrieve the node labels of a PST
}
\description{
	Retrieve the node labels of a PST 
}
\usage{
\S4method{nodenames}{PSTf}(object, L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	A PST, that is an object of class \code{PSTf} as returned by the \code{\link{pstree}} or \code{\link{prune}} method.
}
  \item{L}{
	integer. Depth of the tree for which the node names are retrieved. If missing the names of all the nodes in the tree are returned.
}
}

\value{
	A vector containing the node labels (i.e. contexts).
}

\author{
Alexis Gabadinho
}

\examples{
data(s1)
s1 <- seqdef(s1)
S1 <- pstree(s1, L=3)

nodenames(S1, L=3)
nodenames(S1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ misc }% __ONLY ONE__ keyword per line
