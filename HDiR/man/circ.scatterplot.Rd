\encoding{utf8}
\name{circ.scatterplot}
\alias{circ.scatterplot}
\title{Circular scatterplot for plug-in HDRs}
\description{
This function produces a circular scatterplot with points coloured according to the HDRs in which they fall.
}
\usage{
circ.scatterplot(sample,tau=c(0.25,0.5,.75),
bw=bw.CV(circular(sample),upper=100),tau.method="quantile",
plot.density=TRUE,col=NULL,shrink=NULL,cex=NULL,lty=NULL)
}
\arguments{
  \item{sample}{Numeric vector of angles in radians.}
  \item{tau}{
Numeric vector of probabilities. According to Saavedra-Nieves and Crujeiras (2021), \code{1-tau} represents the probability coverages required for HDRs.}
  \item{bw}{
Smoothing parameter to be used. Following Oliveira et al. (2014), the value of the smoothing parameter can be chosen by using the functions \code{bw.rt}, \code{bw.CV}, \code{bw.pi} or \code{bw.boot}. It could be also selecting by considering \code{circ.boot.bw}, the new smoothing parameter proposed in Saavedra-Nieves and Crujeiras (2021). Default \code{bw=bw.CV} providing a cross-validation bandwidth.
}
  \item{tau.method}{
Character value selecting the rule to estimate the threshold of the HDR. This must be one of \code{"quantile"} or \code{"trapezoidal"}. The first option estimates the threshold using the quantile method proposed in Hyndman(1996); the second one, using the trapezoidal rule for numerical integration. Default \code{tau.method=} \code{"quantile"}.}
  \item{plot.density}{
Logical string. If \code{TRUE}, the kernel density estimator is added to the scatterplot. Default \code{plot.density=TRUE}. }
  \item{col}{
Vector containing the color numbers for plotting the scatterplot. If \code{NULL},
a default color palette is used.}
  \item{shrink}{
Parameter that controls the size of the plotted circle. Default is 2. Larger values shrink the circle, while smaller values enlarge the circle.}
  \item{cex}{
Point character size for representing the data on the scatterplot. Default is 0.5.}
  \item{lty}{
A numeric vector indicating the line types to represent the thresholds of HDRs. Line type can be specified as an integer (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash). Default \code{lty=2}.}
}
\details{
A detailed definition of directional HDRs and of their plug-in estimators is given in Saavedra-Nieves and Crujeiras (2021).\cr Package \code{NPCirc} is used to estimate the circular density using the classical kernel density estimator. See Oliveira et al. (2014) for more details.\cr Moreover, the density quantile algorithm proposed in Hyndman (1996) or the trapezoidal rule can be used to compute the threshold of HDR.\cr The scatterplot is created colouring the sample points according to which HDR they fall.
}
\value{
A scatterplot showing the points coloured according to which HDR they fall. Futhermore, a list where the number of components is equal to the number HDR estimated or, equivalently, to the length of \code{tau} vector. Each component contains the sample points in each HDR from the smallest value of \code{tau} to the biggest one.
}
\references{
Hyndman, R.J. (1996). Computing and graphing highest density regions, The American Statistician, 50, 120-126.\cr
Oliveira, M., Crujeiras R.M. and Rodríguez-Casal, A. (2014). NPCirc: an R package for nonparametric circular methods. Journal of Statistical Software, 61(9), 1-26. \url{https://www.jstatsoft.org/v61/i09/}.\cr
Saavedra-Nieves, P. and Crujeiras, R. M. (2021). Nonparametric estimation of directional highest density regions. Advances in Data Analysis and Classification, 1-36.

}
\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}

\examples{
# Scatterplot for orientations of females for saltator specie
data(sandhoppers)
attach(sandhoppers)
saltatorF<-angle[(species=="salt")&(sex=="F")]
circ.scatterplot(saltatorF)
# Scatterplot for sample of size 100 of model 14 in NPCirc
library(NPCirc)
set.seed(1)
sample<- rcircmix(100, model=14)
circ.scatterplot(sample,tau=c(0.2,0.5,0.8))
}

