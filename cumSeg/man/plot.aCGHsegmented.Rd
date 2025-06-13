\name{plot.aCGHsegmented}
\alias{plot.aCGHsegmented}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot method for the class 'aCGHsegmented'
}
\description{
Plots fitted piecewise constant lines.
}
\usage{
\method{plot}{aCGHsegmented}(x, add = FALSE, y = TRUE, psi.lines = TRUE, typeL="l", 
    what=c("lines","criterion"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class \code{"aCGHsegmented"} returned by \code{jumpoints}.
}
  \item{add}{
logical; if \code{TRUE} the fitted piecewise constant lines are added 
  to an existing plot.
}
  \item{y}{
logical; if \code{TRUE} the observations are also plotted, otherwise only the fitted lines.
}
  \item{psi.lines}{
logical; if \code{TRUE} vertical lines corresponding to the estimated changepoints are added.
}
  \item{typeL}{
argument \code{type} for the fitted lines. Possible options are \verb{typeL="s"} to plot the horizontal and vertical lines of the step-function, and 
\verb{typeL="l"} to draw the horizontal lines only.  
}
  \item{what}{
If \code{'lines'} the fitted lines are plotted, otherwise the criterion values versus the number of change points, provided the fitted object \code{x} has been 
called with argument \code{output='2'} or \code{output='3'}.  
}
  \item{\dots}{
possible additional graphical arguments, such as \code{col}, \code{xlab}, and so on.  
}
}
\details{
This fuction takes a fitted object returned by \code{jumpoints} and plots 
the resulting fit, namely the estimated step-function and changepoints.
}
\value{
The function simply plots the fit returned by 'jumpoints'.
}
\author{
Vito Muggeo
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{jumpoints}}
}
\keyword{ regression }
