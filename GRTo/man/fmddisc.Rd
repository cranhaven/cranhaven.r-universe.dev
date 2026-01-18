\name{fmddisc}
\alias{fmddisc}
\title{ Analysis of Discontinuities in Frequency Magnitude Distributions }
\description{
     This function finds significant discontinuities in Frequency Magnitude Distributions (FMD).
}
\usage{
fmddisc(file, header = FALSE, colid = 1, nrep = 200, title = "fmddisc")
}
\arguments{
  \item{file}{ filename of the file to be loaded }
  \item{header}{ whether \code{file} contains headers or not }
  \item{colid}{ field number for the magnitude values in \code{file} }
  \item{nrep}{ number of replicates for the bootstrap }
  \item{title}{ main title for the plot }
}
\details{
  This function reads magnitude values in the field which number is indicated by \code{colid} in \code{file}.
  The function returns values of significant magnitude discontinuities (e. g. deviations from linearity) in the FMD. A bootstrap procedure is used to obtain the 90\% confidence interval for magnitude 
discontinuities. We assumed the distribution of the \code{nrep} bootstrap replicates is not skewed. Therefore, the 90\% confidence interval is simply formed by taking 5\% and 95\% quantiles of the bootstrap replicates as the lower and upper bound of the interval respectively. These values and the 50\% quantile (median) are returned in a list by the function.
  The function also returns bootstrap mean and bootstrap standard-error (standard deviation of bootstrap replicate estimates). The bootstrap margin of errors at the 90\% normal confidence level is returned as the result of 1.645 \* bootstrap standard-error. 
  The function produces a plot showing the FMD and histograms of magnitude values for the significant 
discontinuities in the FMD. The plot is stored into a file with name \code{file}\_disc.png (png format file).
Values of counts in histograms are controlled by the number of replicates \code{nrep} that are
used. 
}
\value{
  This function returns a list containing the following components: 
  \item{quant1 }{the 5\%, 50\% and 95\% quantiles of the bootstrap replicates for the main discontinuity}
  \item{valid }{the numbers of valid replicates}
  \item{bmean }{the bootstrap mean values}
  \item{bse }{the bootstrap standard-error values}
  \item{bme }{the bootstrap margin of errors values}
  \item{quant2 }{the 5\%, 50\% and 95\% quantiles of the bootstrap replicates for the auxiliary discontinuity}
}
\references{ 
D. Amorese, "Applying a change-point detection method on frequency-magnitude distributions", \emph{Bull. seism. Soc. Am.} (2007) 97, doi:10.1785\/0120060181
}
\author{ Daniel Amorese }
\note{
Thanks to Scott Kostyshak for telling us about the extra bootstrap package dependency. GRTo version 1.2 fixes this issue.
}
\examples{
fmddisc(file=system.file("extdata","IDYLLdeep.data.txt",package="GRTo"),
header=FALSE,colid=15,nrep=200,"FMD mag discontinuities")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ htest }
