\name{bvmed}
\alias{bvmed}
\title{ Repeated medians regression for the determination of the Gutenberg-Richter b-value }
\description{
     This function determines the Gutenberg-Richter \emph{b}-value from a set of magnitude values, using the repeated medians regression method.
}
\usage{
bvmed(file, lis, hd = FALSE, colid = 1, nrep = 200, tm = NULL,
findtm = TRUE, title = "bvmed")
}
\arguments{
  \item{file}{ file to be loaded }
  \item{lis}{ list to be loaded }
  \item{hd}{ whether \code{file} contains header or not }
  \item{colid}{ field number for the magnitude values in \code{file} }
  \item{nrep}{ number of replicates for the bootstrap (calculation of the standard-error for the \emph{b}-value) }
  \item{tm}{ threshold magnitude value }
  \item{findtm}{ logical. Whether an automatic procedure is engaged to determine the threshold magnitude value \code{tm} or not }
  \item{title}{ character. The title of the plot. The name for the PNG file that includes the plot begins with \code{title} }
}
\details{
 This function reads magnitude values in the field which number is indicated by \code{colid} in \code{file}. Magnitude values are read in the
list \code{lis} if the file does not exist.
 This function produces a plot showing the FMD and the linear model line. The plot is stored into a file with name \code{file}_bvmed.png (png format file).
 It includes the mblm function from the version 0.11 (2007) of the mblm library by Lukasz Komsta (known e-mail address : luke@novum.am.lublin.pl).
}
\value{
A list containing the following components:
  \item{quantm}{ the 5\%, 50\% and 95\% quantiles of the bootstrap replicates for the threshold magnitude value }
  \item{mmed}{ the median of the bootstrap replicates for the threshold magnitude value }
  \item{quantb}{ the 5\%, 50\% and 95\% quantiles of the bootstrap replicates for the \emph{b}-value }
  \item{valid}{ the number of valid replicates }
  \item{brm}{ the \emph{b}-value }
  \item{bse}{ the bootstrap standard-error value for the \emph{b}-value } 
  \item{bme}{ the bootstrap margin of errors value for the \emph{b}-value }
}
\references{
D. Amorese, J.-R. Grasso and P. A. Rydelek, "On varying \emph{b}-values with depth: results from computer-intensive tests for Southern California", \emph{Geophys. J. Int.} (2010) 180, 347-360 

Siegel, A.F., "Robust regression using repeated medians", \emph{Biometrika} (1982) 69, 242-244.
}
\author{ Daniel Amorese <amorese@ipgp.fr>, Paul A. Rydelek <prydelek@memphis.edu> and Jean-Robert Grasso <grasso@obs.ujf-grenoble.fr>

Maintainer: Daniel Amorese <amorese@ipgp.fr>
}
\note{
Thanks to Paul Friberg for telling us about the bug in the graphical part of bvmed.R. Thanks to Scott Kostyshak for telling us about the extra bootstrap package dependency. GRTo version 1.2 fixes these issues. Thanks to Andrew Barbour (USGS) for detecting and fixing minor problems of bvmed.R of version 1.2. In version 1.3, an option coded by Andrew Barbour allows the processing of data from a list. Willy Aspinall (University of Bristol) highlights bugs in the graphical parts of bvmed: An inappropriate magnitude shift in the FMD plot is definitely fixed in version 1.3. Many thanks to Willy!
}
\examples{
bvmed(file=system.file("extdata","IDYLLdeep.data.txt",package="GRTo"), colid = 15, nrep = 150, 
tm = NULL, findtm = TRUE, title = "IDYLLWILD")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ robust }
