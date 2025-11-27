\name{plot-PSTr}
\alias{plot,PSTf,ANY-method}
\alias{plot,PSTr,ANY-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Plot a PST
}
\description{
	Plot a PST
}
\usage{
	\S4method{plot}{PSTf,ANY}(x, y=missing, max.level=NULL,
		nodePar = list(), edgePar = list(),
		axis=FALSE, xlab = NA, ylab = if (axis) { "L" } else {NA}, 
		horiz = FALSE, xlim, ylim, 
		withlegend=TRUE, ltext=NULL, cex.legend=1, 
		use.layout=withlegend!=FALSE, legend.prop=NA, ...)
}

\arguments{
  \item{x}{
	A PST, that is an object of class \code{PSTf} as returned by the \code{\link{pstree}} or \code{\link{prune}} method.
}
  \item{y}{
	not applicable
  }
  \item{max.level}{ 
	integer. The maximal depth for the display of the tree.
}
  \item{nodePar}{ 
	list. A list of parameters for tuning the node representation. Possible parameters are
	\itemize{
	\item node.size. numeric. The size of the node, in fraction of a unit of the x axis (or y axis if horiz=TRUE).
	\item gratio. The ratio between horizontal and vertical dimensions of the node. usefull if the horizontal and vertical dimensions of the plot are not equal. If not provided, it is estimated as a function of the number of leaves represented in the plot and the depth of the tree.
	}
}
  \item{edgePar}{ 
	list. A list of parameters for tuning the edges representation. Possible paramters are
		

}
  \item{axis}{ 
	logical. If TRUE the axes are displayed on the plot.
}
  \item{xlab}{ 
	character. Label for the x axis.
}
  \item{ylab}{ 
	character. Label for the y axis representing the tree depth.
}
  \item{horiz}{ 
	logical. If FALSE, the tree is represented vertically. The root node at depth L=0 is plotted on the top, and the nodes of maximal depth are plotted on the bottom of the plot. If TRUE, the tree is represented horizontally. The root node at depth L=0 is plotted on the right, and the nodes of maximal depth are plotted on the left of the plot.
}
  \item{xlim}{
	numeric. Vector of length 2 giving the x limits for the plot. By default the limits are 1 .. number of terminal nodes (at max.level if specified). This may be usefull to facilitate comparison if several trees are plotted on the same figure.
}
  \item{ylim}{
	numeric. Vector of length 2 giving the y limits for the plot. By default the limits are 0 .. max. depth of the tree (max.level if specified). This may be usefull to facilitate comparison if several trees are plotted on the same figure.
}
  \item{withlegend}{ 
	defines if and where the legend of the state colors is plotted. The default value \code{TRUE} sets the position of the legend automatically. Other possible value is \code{"right"}.
}
  \item{ltext}{
	optional description of the states to appear in the legend. Must be a vector of character strings with number of elements equal to the size of the alphabet. If unspecified, the \code{label} attribute of the \code{seqdata} sequence object is used (see \code{\link{seqdef}}).
}
	\item{cex.legend}{
	expansion factor for setting the size of the font for the labels in the legend. The default value is 1. Values lesser than 1 will reduce the size of the font, values greater than 1 will increase the size.
}
 \item{use.layout}{
	if \code{TRUE}, \code{\link[graphics]{layout}} is used to arrange plots when using the group option or plotting a legend. When layout is activated, the standard '\code{\link[graphics]{par}(mfrow=....)}' for arranging plots does not work. With \code{withlegend=FALSE} and \code{group=NULL}, layout is automatically deactivated and '\code{par(mfrow=....)}' can be used.
}
 \item{legend.prop}{
	sets the proportion of the graphic area used for plotting the legend when \code{use.layout=TRUE} and \code{withlegend=TRUE}. Default value is set according to the place (bottom or right of the graphic area) where the legend is plotted. Values from 0 to 1.
}
  \item{\dots}{
	arguments to be passed to the plot function or graphical parameters
}
}

\details{
	The function for graphical representation of a PST uses is recursive. The main argument of the function is a tree represented as a nested list (an object of class \code{PSTr}). See also \cite{Gabadinho 2016}.
}
\references{
Gabadinho, A. & Ritschard, G. (2016). Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), pp. 1-39.
}
\author{
	Alexis Gabadinho, based on code from \code{plot.dendrogram} 
}

\examples{
data(s1)
s1 <- seqdef(s1)
S1 <- pstree(s1, L=3)
plot(S1)
plot(S1, horiz=TRUE)
plot(S1, nodePar=list(node.type="path", lab.type="prob", lab.pos=1, lab.offset=2, lab.cex=0.7), 
	edgePar=list(type="triangle"), withlegend=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
\keyword{ hplot }
