\name{BBootComp}
\alias{BBootComp}
\title{ Bootstrap test for the comparison of Gutenberg-Richter b-values }
\description{
     This function computes Gutenberg-Richter \emph{b}-values, as well as their uncertainties. The comparison of 2 \emph{b}-values are performed through a bootstrap test.
}
\usage{
BBootComp(finame1, finame2 = NULL, colid1 = 1, colid2 = 1, hd1 = FALSE, 
hd2 = FALSE, nrep = 5000, alter = c("two.sided", "less", "greater"), 
tm1 = NULL, tm2 = NULL, findtm1 = TRUE, findtm2 = TRUE, plot = FALSE, 
title = "BootComp",oplt="p")
}
\arguments{
  \item{finame1}{ a character string specifying the first file to be loaded }
  \item{finame2}{ an optional character string specifying the second file to be loaded. See 'Details' }
  \item{colid1}{ field number for the magnitude values in \code{finame1} }
  \item{colid2}{ an optional field number for the magnitude values in \code{finame2}. See 'Details' }
  \item{hd1}{ logical. Whether \code{ finame1 } contains headers or not }
  \item{hd2}{ logical. Whether \code{ finame2 } contains headers or not }
  \item{nrep}{ number of replicates for the bootstrap }
  \item{alter}{ a character string specifying the alternative hypothesis, must be one of '"two.sided"' (default), '"greater"' or '"less"'.  You can specify just the initial letter }
  \item{tm1}{ threshold magnitude value for the first data set. If \code{tm1} is NULL and \code{findtm1} is FALSE, the function considers that the data set has been already truncated after the threshold magnitude value (i. e. your data set shows only the linear part of the FMD) }
  \item{tm2}{ an optional threshold magnitude value for the second data set. If \code{tm2} is NULL and \code{findtm2} is FALSE, the function considers that the data set has been already truncated after the threshold magnitude value (i. e. your data set shows only the linear part of the FMD)}
  \item{findtm1}{ logical. Whether an automatic procedure is engaged to determine the threshold magnitude value \code{tm1} or not }
  \item{findtm2}{ logical. Whether an automatic procedure is engaged to determine the threshold magnitude value \code{tm2} or not }
  \item{plot}{ logical. If \code{TRUE}, a FMD plot is drawn. Default is \code{plot} equals \code{FALSE} } 
  \item{title}{ character. The title of the plot. The name for the PNG file that includes the plot begins with \code{title} } 
  \item{oplt}{ character. Option for the plot. If \code{oplt} is "p", the FMD displays points (open triangles), else it shows a staircase. This option applies only to the plot of a single FMD: When 2 FMDs are compared, they are always shown with point symbols } 
}
\details{
  if 'finame2' is not NULL, this function compares 2 sets of magnitude values that are contained
in 2 different files. Otherwise, only the FMD for data in 'finame1' is analyzed.
}
\value{
A list containing the following components:
  \item{val}{ object of class htest containing the results of the bootstrap test }
  \item{b1}{ \emph{b}-value for the first data set }
  \item{b2}{ \emph{b}-value for the second data set }
  \item{sd1}{ standard-error of the \emph{b}-value for the first data set }
  \item{sd2}{ standard-error of the \emph{b}-value for the second data set } 
  \item{m01}{ threshold magnitude for the first data set }
  \item{m02}{ threshold magnitude for the second data set }
  \item{a1}{ \emph{a}-value for the first data set. This value is not corrected for time, e. g. this is not the seismic productivity per year }
  \item{a2}{ \emph{a}-value for the second data set. This value is not corrected for time, e. g. this is not the seismic productivity per year }
}
\references{ 
D. Amorese, "Applying a change-point detection method on frequency-magnitude distributions",  \emph{Bull. seism. Soc. Am.} (2007) 97, doi:10.1785\/0120060181 

D. Amorese, J.-R. Grasso and P. A. Rydelek, "On varying \emph{b}-values with depth: results from computer-intensive tests for Southern California", \emph{Geophys. J. Int.} (2010) 180, 347-360 }
\author{ Daniel Amorese <amorese@ipgp.fr>, Paul A. Rydelek <prydelek@memphis.edu> and Jean-Robert Grasso <grasso@obs.ujf-grenoble.fr>

Maintainer: Daniel Amorese <amorese@ipgp.fr>
  }
%- \note{ } 
\seealso{ \code{\link{utcomp}} }
\examples{
    BBootComp(finame1=system.file("extdata","IDYLLshal.data.txt",package="GRTo"),
    finame2=NULL, colid1=15, colid2=NULL,nrep=200,alter=NULL,findtm1=TRUE,
    findtm2=NULL,plot=TRUE, title="IDYLLWILD", tm1=NULL,tm2=NULL)
 }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
