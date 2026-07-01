\encoding{utf8}
\name{sphere.scatterplot}
\alias{sphere.scatterplot}
 \title{
Spherical scatterplot for plug-in HDRs
}
\description{
This function produces a spherical scatterplot with points coloured according to the HDRs in which they fall.}
\usage{
sphere.scatterplot(sample,tau=c(0.25,0.5,.75),bw="none",
                   ngrid=500,nborder=1000,tol=0.1, col=NULL)
}
 \arguments{
  \item{sample}{
A matrix whose rows represent points on the unit sphere in Cartesian coordinates. If a row norm is different from one, a message appears indicating that they must be standardized.}
  \item{tau}{
Numeric vector of probabilities. According to Saavedra-Nieves and Crujeiras (2021), \code{1-tau} represents the probability coverages required for HDRs.}
  \item{bw}{
Smoothing parameter to be used. According to \code{Directional} package, this can be either \code{"none"} for cross validation or \code{"rot"} for the rule of thumb suggested by García-Portugués (2013). It could be also selecting by considering \code{sphere.boot.bw}, the new smoothing parameter proposed in Saavedra-Nieves and Crujeiras (2021). Default \code{bw="none"}.}
  \item{ngrid}{
Sets the resolution of the density calculation. Default \code{ngrid=500}.}
  \item{nborder}{
Maximum number of HDRs boundary points to be represented. Default \code{nborder=} \code{1000}.}
  \item{tol}{
Tolerance parameter to determinate the boundary of HDRs. Default \code{tol=0.1}.
}
  \item{col}{
Vector containing the color numbers for plotting the scatterplot. If \code{NULL},
a default color palette is used.}
}
\details{
A detailed definition of directional HDRs and of their plug-in estimators is given in Saavedra-Nieves and Crujeiras (2021).\cr Package \code{Directional} is used to compute tha von Mises-Fisher kernel density estimate.\cr The density quantile algorithm proposed in Hyndman (1996) is used to calculate the threshold of HDR.\cr The scatterplot is created colouring the sample points according to which HDR they fall.}
\value{
A scatterplot showing the points coloured according to which HDR they fall. Futhermore, a list where the number of components is equal to the number HDR estimated or, equivalently, to the length of tau vector. Each component contains the sample points in each HDR from the smallest value of tau to the biggest one.%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
García-Portugués, E. (2013). Exact risk improvement of bandwidth selectors for kernel density
estimation with directional data. Electronic Journal of Statistics, 7, 1655-1685.\cr
Tsagris, M., Athineou, G., Sajib, A., Tsagris, M. M. and Imports, M. A. S. S. (2016). Package \code{Directional}. \url{https://cran.r-project.org/package=Directional}.\cr
Saavedra-Nieves, P. and Crujeiras, R. M. (2021). Nonparametric estimation of directional highest density regions. Advances in Data Analysis and Classification, 1-36.

}
\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}




\examples{
\donttest{
# Scatterplot of model 4 in library HDiR
set.seed(1)
sample=rspheremix(1000,model=4)
sphere.scatterplot(sample,tau=c(.2,.5,.8))
#Scatterplot of model 9 in library HDiR
set.seed(1)
sample=rspheremix(1000,model=9)
sphere.scatterplot(sample)
}
}
