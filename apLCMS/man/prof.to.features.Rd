\encoding{latin1}
\name{prof.to.features}
\alias{prof.to.features}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generate feature table from noise-removed LC/MS profile }
\description{
  Each LC/MS profile is first processed by the function proc.cdf() to remove noise and reduce data size. A matrix containing m/z value, retention time, intensity, and group number is output from proc.cdf(). This matrix is then fed to the function prof.to.features() to generate a feature list. Every detected feature is summarized into a single row in the output matrix from this function. 
}
\usage{
prof.to.features(a, bandwidth=0.5, min.bw=NA, max.bw=NA, sd.cut=c(0.1, 100), 
sigma.ratio.lim=c(0.1, 10), shape.model="bi-Gaussian", estim.method="moment",
do.plot=TRUE, power=1, component.eliminate=0.01, BIC.factor=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{ The matrix output from proc.cdf(). It contains columns of m/z value, retention time, intensity and group number. }
  \item{bandwidth}{ A value between zero and one. Multiplying this value to the length of the signal along the time axis helps determine the bandwidth in the kernel smoother used for peak identification. See the details section. }
  \item{min.bw}{The minimum bandwidth to use in the kernel smoother. See the details section.}
  \item{max.bw}{The maximum bandwidth to use in the kernel smoother. See the details section.}
  \item{sd.cut}{A vector of two. Features with standard deviation outside the range defined by the two numbers are eliminated.}
  \item{sigma.ratio.lim}{A vector of two. It enforces the belief of the range of the ratio between the left-standard deviation and the righ-standard deviation of the bi-Gaussian fuction used to fit the data.}
  \item{shape.model}{ The mathematical model for the shape of a peak. There are two choices - "bi-Gaussian" and "Gaussian". When the peaks are asymmetric, the bi-Gaussian is better. The default is "bi-Gaussian".}
  \item{estim.method}{The estimation method for the bi-Gaussian peak model. Two possible values: moment and EM.}
  \item{do.plot}{Whether to generate diagnostic plots.}
  \item{component.eliminate}{In fitting mixture of bi-Gaussian (or Gaussian) model of an EIC, when a component accounts for a proportion of intensities less than this value, the component will be ignored.}
  \item{power}{The power parameter for data transformation when fitting the bi-Gaussian or Gaussian mixture model in an EIC.}
  \item{BIC.factor}{the factor that is multiplied on the number of parameters to modify the BIC criterion. If larger than 1, models with more peaks are penalized more.}
}
\details{
  This function generates the feature table from the noise-removed profile. The m/z values are already grouped by the function proc.cdf() to generate EICs. The task of this function is to look at every EIC and determine (1) how many peaks there are, and (2) the location, spread and area of each peak.
  For the first task, when a series of signals is found at an m/z group, kernel smoother is fit along the time axis to determine whether there is one single peak or multiple peaks. The bandwidth of the kernel smoother is determined as follows: multiply the length of the signals by the bandwidth parameter. If the resulting value is between the parameters min.bw and max.bw, use that value; else if the value is below min.bw, use min.bw; else if the value is above max.bw, use max.bw. The default values of min.bw and max.bw are NA, in which case min.bw is set to be 1/30 of the retention time range, and max.bw is set to be 1/15 of the retention time range. 
  A modified EM algorithm which allows missing completely at random at certain time points is used for the evaluation of the peak location and area. If a single peak is detected by the kernel smoother, the maximum likelihood normal curve is fitted. If multiple peaks are detected, the EM algorithm finds the normal mixture that best explain the data. 
}
\value{
A matrix is returned. The columns are: m/z value, retention time, spread (standard deviation of the estimated normal curve), and estimated total signal strength (total area of the estimated normal curve).
}
\author{ Tianwei Yu <tyu8@sph.emory.edu> }
\seealso{ proc.cdf }
\examples{
\donttest{
data(prof)
this.feature<-prof.to.features(prof[[1]])
this.feature[1:5,]
}
}
\keyword{ models }

