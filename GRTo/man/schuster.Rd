\name{schuster}
\alias{schuster}
\title{ Plot of a phasor walkout for the Schuster's test }
\description{
     This function plots a phasor walkout for the Schuster's test.
}
\usage{
schuster(finame, title = "Schuster's diagram", color = c("black","red","orange",
"green","cyan","navy"), hd = FALSE, colidye = NULL, colidmo = NULL, colidda = NULL, 
colidho = 1, colidmi = 2, colidma = 3, colidz = NULL, utccor = 0, dayt1 = NULL, 
dayt2 = NULL, ysel1 = NULL, ysel2 = NULL, mosel1 = NULL, mosel2 = NULL, 
dasel1 = NULL, dasel2 = NULL, magsel1 = NULL, magsel2 = NULL, zsel1 = NULL, 
zsel2 = NULL, weekday1 = c("mo","tu","we","th","fr","sa","su"), 
weekday2 = c("mo","tu","we","th","fr","sa","su"))
}
\arguments{
  \item{finame}{ name of the file to be loaded }
  \item{title}{ main title that will appear in the phasor walkout plot }
  \item{color}{ color of the phasor walkout (black/red/orange/green/cyan/navyblue). The first letter is enough to discriminate the color name }
  \item{hd}{ logical. Whether \code{ finame } contains headers or not }
  \item{colidye}{ field number for the year values in the loaded file }
  \item{colidmo}{ field number for the month values in the loaded file }
  \item{colidda}{ field number for the day values in the loaded file }
  \item{colidho}{ field number for the hour values in the loaded file }
  \item{colidmi}{ field number for the minute values in the loaded file }
  \item{colidma}{ field number for the magnitude values in the loaded file }
  \item{colidz}{ field number for the depth values in the loaded file }
  \item{utccor}{ correction for local time. This value and values of the start and end hours of nighttime are used to
filter the daytime seismic noise from the walkout plot }
  \item{dayt1}{ the start hour of nighttime in local time }
  \item{dayt2}{ the end hour of nighttime in local time }
  \item{ysel1}{ minimum year value for selection (Default is NULL, e. g. no selection) }
  \item{ysel2}{ maximum year value for selection (Default is NULL, e. g. no selection) }
  \item{mosel1}{ minimum month value for selection (Default is NULL, e. g. no selection) }
  \item{mosel2}{ maximum month value for selection (Default is NULL, e. g. no selection) }
  \item{dasel1}{ minimum day value for selection (Default is NULL, e. g. no selection) }
  \item{dasel2}{ maximum day value for selection (Default is NULL, e. g. no selection) }
  \item{magsel1}{ minimum magnitude value for selection (Default is NULL, e.g. no selection) }
  \item{magsel2}{ maximum magnitude value for selection (Default is NULL, e.g. no selection) }
  \item{zsel1}{ minimum depth value for selection (Default is NULL, e. g. no selection) }
  \item{zsel2}{ maximum depth value for selection (Default is NULL, e. g. no selection) }
  \item{weekday1}{ first day in the week for selection ("mo" for "Monday", "tu" for "Tuesday", "we" for "Wednesday", "th" for "Thursday", "fr" for "Friday", "sa" for "Saturday", "su" for "Sunday") }
  \item{weekday2}{ last day in the week for selection ("mo" for "Monday", "tu" for "Tuesday", "we" for "Wednesday", "th" for "Thursday", "fr" for "Friday", "sa" for "Saturday", "su" for "Sunday") }
}
\details{
  This function reads earthquake times (hour and minutes values are required) in an input file and plots a phasor walkout into a file with name \code{file}\_schu.png (png format file).
Selections can be performed based on ranges in hour of the day, year, month, day, magnitude, depth and day of the week (in the same week). The correction for local time is such that GMT time + correction (e. g. \code{utccor}) = local time. 
}
\references{
Rydelek, P. A. and Hass, L. (1994) On Estimating the Amount of Blasts in Seismic Catalogs with Schuster's Method \emph{Bulletin of the Seismological Society of America}, Vol. 84, No. 4, pp. 1256-1259.

Zurn, W. and Rydelek, P. A. (1996) Revisiting the phasor-walkout method for detailed investigation of harmonic signals in time series \emph{Surveys in Geophysics}, Vol. 15, No. 4, pp. 409-431. 
}
\author{ Daniel Amorese \& Paul A. Rydelek }
%- \note{ } 
%\seealso{ \code{\link{read.table}}, \code{\link{save}} }
\examples{
schuster(finame=system.file("extdata","IDYLLdeep.data.txt",package="GRTo"), 
title = "Schuster's diagram", hd = FALSE, colidye = 1, color="n", colidmo = 2, 
colidda = 3, colidho = 4, colidmi = 5, colidma = 15, colidz = 9, utccor = -9, 
dayt1 = NULL, dayt2 = NULL, ysel1 = 1983, ysel2 = 1990, mosel1 = NULL, 
mosel2 = NULL, dasel1 = NULL, dasel2 = NULL, magsel1 = NULL, magsel2 = NULL, 
zsel1 = NULL, zsel2 = NULL, weekday1 = NULL, weekday2 = NULL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
