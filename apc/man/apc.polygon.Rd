\name{apc.polygon}
\alias{apc.polygon}
\title{Add connected line and standard deviation polygons to a plot}
\description{Draws a line for point forecasts and adds shaded region for forecast distribution around it. This is added to a plot in the same way as
	\code{\link{lines}} and \code{\link{polygon}} add lines and polygons to a plot.}
\usage{apc.polygon(m.forecast,x.origin=1,
	plot.se=TRUE,plot.se.proc=FALSE,plot.se.est=FALSE,
	unit=1,
	col.line=1,lty.line=1,lwd.line=1,
	q.se=c(2,2,2),
	angle.se=c(45,45,45),
	border.se=c(NA,NA,NA),
	col.se=gray(c(0.50,0.80,0.90)),
	density.se=c(NULL,NULL,NULL),
	lty.se=c(1,1,1))}
\arguments{
  \item{m.forecast}{Matrix.  Up to 4 columns.
  							Column 1: point forecasts.
							Column 2: forecast standard errors.
							Column 3: process standard errors.
							Column 4: estimation standard errors.}
  \item{x.origin}{\emph{Optional.} Numerical. x-coordinate for last observation. The first point forecast is made at \code{x.origin+unit}, where \code{unit} (with default 1) is defined in \code{\link{apc.data.list}}. Default: 1.}
  \item{plot.se}{\emph{Optional.} Logical. Should forecast   standard errors be plotted? Default: \code{TRUE}.}
  \item{plot.se.proc}{\emph{Optional.} Logical. Should process    standard errors be plotted? Default: \code{FALSE}.}
  \item{plot.se.est}{\emph{Optional.} Logical. Should estimation standard errors be plotted? Default: \code{FALSE}.}
  \item{unit}{\emph{Optional.} Numerical. step length for point forecasts. Default=1.}
  \item{col.line}{\emph{Optional.} Point forecasts: Colour of line. Same as \code{col} for \code{\link{lines}}. Default: 1.}
  \item{lty.line}{\emph{Optional.} Point forecasts: Type   of line. Same as \code{lty} for \code{\link{lines}}. Default: 1.}
  \item{lwd.line}{\emph{Optional.} Point forecasts: Width  of line. Same as \code{lwd} for \code{\link{lines}}. Default: 1.}
  \item{q.se}{\emph{Optional.} Vector of length 3. Multiplication factors for standard errors. Default: \code{c(2,2,2)}.}
  \item{angle.se}{\emph{Optional.} Standard error polygon: 3-vector: Angle  of shading.  Same as \code{angle} 	for \code{\link{polygon}}. Default: \code{=c(45,45,45)}.}
  \item{border.se}{\emph{Optional.} Standard error polygon: 3-vector: Border of polygon.  Same as \code{border} 	for \code{\link{polygon}}. Default: \code{=c(NA,NA,NA)}.}
  \item{col.se}{\emph{Optional.} Standard error polygon: 3-vector: Colour of polygon.  Same as \code{col} 	for \code{\link{polygon}}. Default: \code{gray(c(0.50,0.80,0.90))}.}
  \item{density.se}{\emph{Optional.} Standard error polygon: 3-vector: Density of shading. Same as \code{density} for \code{\link{polygon}}. Default: \code{=c(NULL,NULL,NULL)}.}
  \item{lty.se}{\emph{Optional.} Standard error polygon: 3-vector: Type of shading.    Same as \code{lty} 	for \code{\link{polygon}}. Default: \code{=c(1,1,1)}.}
}
\details{The empirical example of
	Martinez Miranda, Nielsen and Nielsen (2015)
	uses the data
	\code{\link{data.asbestos}}.
	The results of that paper are reproduced in 
	the vignette
	\href{../doc/ReproducingMMNN2015.pdf}{\code{ReproducingMMNN2015.pdf}},
	\href{../doc/ReproducingMMNN2015.R}{\code{ReproducingMMNN2015.R}}
	on
	\href{../doc/index.html}{\code{Vignettes}}.
	The function is used there.
}
\references{
Martinez Miranda, M.D., Nielsen, B. and Nielsen, J.P. (2015) Inference and forecasting in the age-period-cohort model with unknown exposure with an application to mesothelioma mortality. \emph{Journal of the Royal Statistical Society} A 178, 29-55. \emph{Download}: \href{https://doi.org/10.1111/rssa.12051}{Article}, \href{http://www.nuffield.ox.ac.uk/economics/papers/2013/Asbestos8mar13.pdf}{Nuffield DP}. 
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 6 Jan 2016}