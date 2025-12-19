\encoding{latin1}
\name{plot.spp}
\alias{plot.spp}
\title{Plot a Spatial Point Pattern object}
\description{
 Plot a Spatial Point Pattern object returned by function \code{\link{spp}}.
}
\usage{
\method{plot}{spp}(x, main, out=FALSE, use.marks=TRUE, cols, chars, cols.out, chars.out,
maxsize, scale=TRUE, add=FALSE, legend=TRUE, csize=1, ...)
}
\arguments{
  \item{x}{an object of class \code{"spp"} (see \code{\link{spp}}).}
  \item{main}{by default, the value of argument \code{x}, otherwise a text to be displayed as a title of the plot.\code{main=NULL} displays no title.}
  \item{out}{by default \code{out = FALSE}. If \code{TRUE} points of the pattern located outside the sampling window are plotted.}
  \item{use.marks}{by default \code{use.marks = TRUE}. If \code{FALSE} different symbols are not used for each mark of multivariate
   or marked point patterns, so that they are plotted as univariate (see \code{\link{spp}}).}
  \item{cols}{(optional) the colour(s) used to plot points located inside the sampling window (see Details).}
  \item{chars}{(optional) plotting character(s) used to plot points located inside the sampling window (see Details).}
  \item{cols.out}{(optional) if \code{out = TRUE}, the colour(s) used to plot points located outside the sampling window (see Details).}
  \item{chars.out}{(optional) if \code{out = TRUE}, plotting character(s) used to plot points located outside the sampling window (see Details).}
  \item{maxsize}{(optional) maximum size of plotting symbols. By default \code{maxsize} is automatically adjusted to plot size.}
  \item{csize}{scaling factor for font size so that actual font size is \code{par("cex")*csize}. By default \code{csize = 1}.}
  \item{scale}{If \code{scale = TRUE} (the default) graduations giving plot size are displayed.}
  \item{legend}{If \code{legend = TRUE} (the default) a legend for plot symbols is displayed (multivariate and marked types only).}
  \item{add}{by default \code{add = FALSE}. If \code{TRUE} a new window is not created and just the points are plotted over the existing plot.}
  \item{\dots}{extra arguments that will be passed to the plotting functions \code{\link{plot.default}}, \code{\link{points}} and/or \code{\link{symbols}}.}
}
\details{
The sampling window \code{x$window} is plotted first, through a call to function \code{\link{plot.swin}}.
Then the points themselves are plotted, in a fashion that depends on the type of spatial point pattern (see \code{\link{spp}}).
\itemize{
	\item
	\bold{univariate pattern:}
    if \code{x$type = c("univariate")}, i.e. the point pattern does not have marks, or if \code{use.marks = FALSE}, then the locations of all 
	points is plotted using a single plot character.
	\item
	\bold{multivariate pattern:}
    if \code{x$type = c("multivariate")}, i.e. the marks are levels of a factor, then each level is represented by a different plot character.
	\item
	\bold{marked pattern:}
	if \code{x$type = c("marked")}, i.e. the marks are real numbers, then points are represented by circles (argument \code{chars = "circles"}, the default) or squares 
	(argument \code{chars = "squares"}) proportional to their marks' value (positive values are filled, while negative values are unfilled).
	}
	
	Arguments \code{cols} and \code{cols.out} (if \code{out = TRUE}) determine the colour(s) used to display the points located inside and outside the sampling window, respectively.
	Colours may be specified as codes or colour names (see \code{\link[graphics]{par}("col")}). For univariate and marked point patterns, \code{cols} and \code{cols.out} are single character strings, while 
	for multivariate point patterns they are character vectors of same length as \code{levels(x$marks)} and \code{levels(x$marksout)}, respectively.

	Arguments \code{chars} and \code{chars.out} (if \code{out = TRUE}) determine the symbol(s) used to display the points located inside and outside the sampling window, respectively.
	Symbols may be specified as codes or character strings (see \code{\link[graphics]{par}("pch")}). For univariate point patterns, \code{chars} and \code{chars.out} are single character strings, while 
	for multivariate point patterns they are character vectors of same length as \code{levels(x$marks)} and \code{levels(x$marksout)}, respectively. For marked point patterns, 
	\code{chars} and \code{chars.out} can only take the value \code{"circles"} or \code{"squares"}.	
}
\value{
  none.
}
\author{
 \email{Raphael.Pelissier@ird.fr}
}
\seealso{
  \code{\link{spp}},
  \code{\link{swin}},
  \code{\link{plot.swin}}.
}
\examples{
  data(BPoirier)
  BP<-BPoirier
  
  \dontrun{a univariate point pattern in a rectangle sampling window}
  plot(spp(BP$trees, win=BP$rect))
  
  \dontrun{a univariate point pattern in a circular sampling window}
 plot(spp(BP$trees, win=c(55,45,45)), out=TRUE, scale=TRUE)
 
 \dontrun{a univariate point pattern in a complex sampling window}
 \dontrun{(points outside the sampling window displayed in red colour)}
 plot(spp(BP$trees, win=BP$rect, tri=BP$tri1), out=TRUE)
  
 \dontrun{a multivariate point pattern in a rectangle sampling window}
 plot(spp(BP$trees, win=BP$rect, marks=BP$species))
 
 \dontrun{a multivariate point pattern in a circular sampling window}
 \dontrun{(points inside/outside the sampling window displayed in blue colour/as red crosses)}
 plot(spp(BP$trees, win=c(55,45,45), marks=BP$species), out=TRUE, cols=c("blue","blue","blue"), 
 chars.out=c("+","+","+"), cols.out=c("red","red","red"))
 
  \dontrun{a marked point pattern in a rectangle sampling window with circles in green colour}
  plot(spp(BP$trees, win=BP$rect, marks=BP$dbh), cols="green")

  \dontrun{a marked point pattern in a circular sampling window}
  \dontrun{(squares in red colour inside and circles in blue colour outside)}
  plot(spp(BP$trees, win=c(55,45,45), marks=BP$dbh), out=TRUE, chars="squares", 
  cols="red", cols.out="blue")
}
\keyword{spatial}
