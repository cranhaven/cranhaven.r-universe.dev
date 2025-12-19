\encoding{latin1}
\name{plot.fads}
\alias{plot.fads}
\alias{plot.fads.kfun}
\alias{plot.fads.k12fun}
\alias{plot.fads.kpqfun}
\alias{plot.fads.kp.fun}
\alias{plot.fads.kmfun}
\alias{plot.fads.ksfun}
\alias{plot.fads.krfun}
\alias{plot.fads.kdfun}
\alias{plot.fads.mimetic}
\title{Plot second-order neighbourhood functions}
\description{
 Plot second-order neighbourhood function estimates returned by functions \code{\link{kfun}, \link{k12fun}, \link{kmfun}}, \cr
 	\code{ \link{kijfun} or \link{ki.fun}}.
}
\usage{
\method{plot}{fads}(x, opt, cols, lty, main, sub, legend, csize, \dots)
}
\arguments{
  \item{x}{an object of class \code{"fads"} (see Details).}
  \item{opt}{one of \code{c("all","L","K","n","g")} to display either all or one of the functions in a single window. By default \code{opt = "all"} for \code{fads} 
  objects of subclass \code{"kfun"}, \code{"k12fun"}, or \code{"kmfun"};  by default \code{opt = "L"} for \code{fads} objects of subclass \code{"kij"}, or \code{"ki."}.}
  \item{cols}{(optional) colours used for plotting functions.}
  \item{lty}{(optional) line types used for plotting functions.}
  \item{main}{by default, the value of argument x, otherwise a text to be displayed as a title of the plot. \code{main=NULL} displays no title.}
  \item{sub}{by default, the name of the function displayed, otherwise a text to be displayed as function subtitle. \code{sub=NULL} displays no subtitle.}
  \item{legend}{If \code{legend = TRUE} (the default) a legend for the plotting functions is displayed.}
  \item{csize}{scaling factor for font size so that actual font size is \code{par("cex")*csize}. By default \code{csize = 1}.}
  \item{\dots}{extra arguments that will be passed to the plotting functions \code{\link{plot.swin}}, \cr
  \code{\link{plot.default}}, \code{\link{symbols}} and/or \code{\link{points}}.}
}
\details{
 Function \code{plot.fads} displays second-order neighbourhood function estimates as a function of interpoint distance, with expected values 
 as well as confidence interval limits when computed. Argument \code{x} can be any \code{fads} object returned by functions \code{\link{kfun},
  \link{k12fun}, \link{kmfun}, \link{kijfun} or \link{ki.fun}}. 
}
\value{none.}
\author{\email{Raphael.Pelissier@ird.fr}}
\seealso{
  \code{\link{kfun}},
  \code{\link{k12fun}},
  \code{\link{kmfun}},
  \code{\link{kijfun}},
  \code{\link{ki.fun}}.}
\examples{
  data(BPoirier)
  BP <- BPoirier
  \dontrun{Ripley's function}
  swr <- spp(BP$trees, win=BP$rect)
  k.swr <- kfun(swr, 25, 1, 500)
  plot(k.swr)
  
  \dontrun{Intertype function}
  swrm <- spp(BP$trees, win=BP$rect, marks=BP$species)
  k12.swrm <- k12fun(swrm, 25, 1, 500, marks=c("beech","oak"))
  plot(k12.swrm, opt="L", cols=1)
  
  \dontrun{Mark correlation function}
  swrm <- spp(BP$trees, win=BP$rect, marks=BP$dbh)
  km.swrm <- kmfun(swrm, 25, 1, 500)
  plot(km.swrm, main="Example 1", sub=NULL, legend=FALSE)

}
\keyword{spatial}
