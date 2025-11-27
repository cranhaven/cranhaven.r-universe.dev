\name{cplot}
\alias{cplot}
\alias{cplot,PSTf-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Plot single nodes of a probabilistic suffix tree
}
\description{
Plot the next symbol probability distribution associated with a particular node in a PST
}
\usage{
	\S4method{cplot}{PSTf}(object, context, state, main=NULL, all=FALSE, x.by=1, y.by=0.2, by.state=FALSE, ...)
}
%% ============================================
\arguments{
  \item{object}{A probabilistic suffix tree, i.e., an object of class \code{"\linkS4class{PSTf}"} as returned by the \code{\link{pstree}}, \code{\link{prune}} or \code{\link{tune}} function.}
  \item{context}{character. Label of the node to plot, provided as a string where states are separated by '-', see examples.}
  \item{state}{logical. Under development.}
  \item{main}{character. Main title for the plot. By default, the title is the node label.}
  \item{all}{logical.}
  \item{x.by}{numeric. Interval for the ticks on the x axis (segments).}
  \item{y.by}{numeric. Interval for the ticks on the y axis (probability).}
  \item{by.state}{logical. If \code{TRUE}, the representation of the probability distribution is done separately for each state of the alphabet.}
  \item{\dots}{ arguments to be passed to the plot function or other graphical parameters. }
}
%%  ============================================
\details{
	The \code{cplot()} function displays a single node labelled with \code{context} of the tree where one or mode barplots (if \code{object} is a segmented PST) represent the probability distribution(s) stored in the node. For more details, see \cite{Gabadinho 2016}.
}
\references{
Gabadinho, A. & Ritschard, G. (2016). Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), pp. 1-39.
}
\author{
	Alexis Gabadinho
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ppplot}}
}
\examples{
data(s1)
s1 <- seqdef(s1)
S1 <- pstree(s1, L=3)

cplot(S1, "a-b")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ models }
