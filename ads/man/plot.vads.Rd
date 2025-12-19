\encoding{latin1}
\name{plot.vads}
\alias{plot.vads}
\alias{plot.vads.dval}
\alias{plot.vads.k12val}
\alias{plot.vads.kval}
\title{Plot local density values}
\description{
  Plot local density estimates returned by functions \code{\link{dval},
  \link{kval} or \link{k12val}}.
}
\usage{
\method{plot}{vads}(x, main, opt, select, chars, cols, maxsize, char0, col0, legend, csize, \dots)
}
\arguments{
  \item{x}{an object of class \code{'vads'} (see Details).}
  \item{main}{by default, the value of argument x, otherwise a text to be displayed as a title of the plot. \code{main=NULL} displays no title.}
  \item{opt}{(optional) a character string to change the type of values to be plotted (see Details).}
  \item{select}{(optional) a vector of selected distances in \code{x$r}. By default, a multiple window displays all distances.}
  \item{chars}{one of \code{c("circles","squares")} plotting symbols with areas proportional to local density values. By default, circles are plotted.} 
  \item{cols}{(optional) the colour used for the plotting symbols. Black colour is the default.}
  \item{maxsize}{(optional) maximum size of the circles/squares plotted. By default, maxsize is automatically adjusted to plot size.}
  \item{char0}{(optional) the plotting symbol used to represent null values. By default, null values are not plotted.}
  \item{col0}{(optional) the colour used for the null values plotting symbol. By default, the same as argument \code{cols}.}
  \item{legend}{If \code{legend = TRUE} (the default) a legend for the plotting values is displayed.}
  \item{csize}{scaling factor for font size so that actual font size is \code{par("cex")*csize}. By default \code{csize = 1}.}
  \item{\dots}{extra arguments that will be passed to the plotting functions \code{\link{plot.swin}}, \cr
  	\code{\link{plot.default}}, \code{\link{symbols}} and/or \code{\link{points}}.}
}
\details{
Function \code{plot.vads} displays a map of first-order local density or second-order local neighbour density values as symbols with areas proportional 
to the values estimated at the plotted points. 
Positive values are represented by coloured symbols, while negative values are represented by open symbols. The plotted function values depend upon the 
type of \code{'vads'} object: 
	\itemize{
	\item
    if \code{class(x)=c("vads","dval")}, the plotted values are first-order local densities and argument \code{opt="dval"} by default, but
	is potentially one of \code{c("dval","cval")} returned by \code{\link{dval}}.\cr
	\item
    if \code{class(x)=c("vads","kval")} or \code{class(x)=c("vads","k12val")}, the plotted values are univariate or bivariate second-order 
	local neighbour densities. Argument \code{opt="lval"} by default, but is potentially one of \code{c("lval","kval","nval","gval")} 
	returned by \code{\link{kval}} and \code{\link{k12val}}.
	}
}
\value{
  none.
}
\author{\email{Raphael.Pelissier@ird.fr}}
\seealso{
  \code{\link{dval}},
  \code{\link{kval}},
  \code{\link{k12val}}.
}
\examples{
  data(BPoirier)
  BP <- BPoirier
  \dontrun{local density in a rectangle sampling window}
  dswr <- dval(spp(BP$trees, win=BP$rect), 25, 1, 11, 9)
  plot(dswr)
  \dontrun{display only distance r from 5 to 10 with null symbols as red crosses}
  plot(dswr, select=c(5:10), char0=3, col0="red")
  
  \dontrun{local L(r) values in a circular sampling window}
  lvswc <- kval(spp(BP$trees, win=c(55,45,45)), 25, 0.5)
  plot(lvswc)
  \dontrun{display square symbols in blue for selected values of r and remove title}
  plot(lvswc, chars="squares", cols="blue", select=c(5,7.5,10,12.5,15), main=NULL)
  
  \dontrun{local K12(r) values (1="beech", 2="oak") in a complex sampling window}
  k12swrt <- k12val(spp(BP$trees, win=BP$rect, tri=BP$tri1, marks=BP$species), 25, 1)
  plot(k12swrt, opt="kval")
}
\keyword{spatial}
