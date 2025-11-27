\name{ppplot}
\docType{methods}
\alias{ppplot}
\alias{ppplot,PSTf-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Plotting a branch of a probabilistic suffix tree
}
\description{
 The \code{ppplot} function displays the probability distributions of a node and all its parent nodes (suffixes) in the tree. IF the name of a gain function and a vector of pruning cutoffs are provided, the graphic will display the outcomes of the gain function, i.e., whether a node represents an information gain relative to its parent.
}
\usage{
\S4method{ppplot}{PSTf}(object, path, gain, C, cex.plot = 1, nsize = 0.3, nlab=TRUE,
	psize = nsize/2, pruned.col = "red", div.col = "green", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	a probabilistic suffix tree, i.e., an object of class \code{"\linkS4class{PSTf}"} as returned by the \code{\link{pstree}}, \code{\link{prune}} or \code{\link{tune}} function.
}
  \item{path}{ character. Either a character string representing the node label (i.e., the context) where symbols are separated by '-', or a vector where each element is a symbol. See example.
}
  \item{gain}{
	character or function. Gain function, see \code{\link{prune}}.
}
  \item{C}{
	numeric. Value of the cutoff used by the gain function, see \code{\link{prune}}.
}
  \item{cex.plot}{numeric. Expansion factor for setting the size of the font for the axis labels and names. The default value is 1. Values lesser than 1 will reduce the size of the font, values greater than 1 will increase the size.
}
  \item{nsize}{
	numeric. Size of the circles representing the nodes.
}
  \item{nlab}{
	logical. Should the node label be displayed inside the circle?
}
  \item{psize}{
	numeric. Size of the circles representing the outcome of the gain function.
}
  \item{pruned.col}{
character. Color used to represent a terminal node which provides no information gain relative to its parent.
}
  \item{div.col}{
character. Color used to represent an internal node which provides information gain relative to its parent.
}
  \item{\dots}{
additional parameters to be passed to the \code{plot} function.
}
}
\details{
For more details, see \cite{Gabadinho 2016}.
}

\references{
Gabadinho, A. & Ritschard, G. (2016). Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), pp. 1-39.
}
\author{
Alexis Gabadinho
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cplot}}, \code{\link{prune}}
}
\examples{
data(s1)
s1.seq <- seqdef(s1)
S1 <- pstree(s1.seq, L=5, ymin=0.001)
ppplot(S1, "a-a-b-b-a", gain="G1", C=c(1.1, 1.2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
