\name{DGobj-class}
\Rdversion{1.1}
\docType{class}
\alias{DGobj-class}
\alias{DG class}
\alias{DGobj}
\alias{DG}
\alias{DG object}
\alias{[,DGobj-method}
\alias{[<-,DGobj-method}
\alias{names,DGobj-method}
\alias{show,DGobj-method}
\alias{summary,DGobj-method}

\title{Class \code{"DGobj"}}
\description{
Class of objects containing demographic and genetic data and used as input of the function \link{ranking.strains} for ranking pathogen strains.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DGobj", ...)} and by calls of the constructors \link{DGobj.rawdata}, \link{DGobj.simul.mechanistic} and \link{DGobj.simul.regression}.
}
\section{Slots}{
  \describe{
    \item{\code{demographic}:}{Object of class \code{"matrix"}. The first two columns give the coordinates of sites where demographic data are available. The third column gives the values of the demographic growth at these sites. }
    \item{\code{genetic}:}{Object of class \code{"matrix"}. The first two columns give the coordinates of sites where genetic data are available. Each following column (3, 4, ...) gives the frequencies of a given strain at these sites. }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "DGobj")}: ... }
    \item{[<-}{\code{signature(x = "DGobj")}: ... }
    \item{names}{\code{signature(x = "DGobj")}: ... }
    \item{show}{\code{signature(object = "DGobj")}: ... }
    \item{summary}{\code{signature(object = "DGobj")}: ... }
	 }
}
\references{Soubeyrand S., Tollenaere C., Haon-Lasportes E. & Laine A.-L. (2014). Regression-based ranking of pathogen strains with respect to their contributions to natural epidemics. PLOS ONE 9(1): e86591.}
\author{
Soubeyrand, S., Tollenaere, C., Haon-Lasportes, E. and Laine, A.-L. 
}

\seealso{
	\link{DGobj.rawdata}, \link{DGobj.simul.mechanistic}, \link{DGobj.simul.regression}, \link{ranking.strains}
}
\examples{
showClass("DGobj")

## load powderymildew data
data(powderymildew)

## construct a DG object from raw data
DGdata=DGobj.rawdata(demographic.coord=powderymildew$demographic.coord,
 genetic.coord=powderymildew$genetic.coord,
 demographic.measures=powderymildew$demographic.measures,
 genetic.frequencies=powderymildew$genetic.frequencies)

## show
DGdata
## summary
summary(DGdata)
## show the demographic slot
DGdata["demographic"]
## show the genetic slot
DGdata["genetic"]
## modify the demographic slot
#DGdata["demographic"]=DGdata["demographic"][1:50,]
## names of slots
names(DGdata)
}
\keyword{classes}
