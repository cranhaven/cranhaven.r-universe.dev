\name{ranking.strains}
\alias{ranking.strains}

\title{
Method for ranking pathogen strains
}
\description{
Ranking pathogen strains based on demographic and genetic data collected during an epidemic.
}
\usage{
ranking.strains(DGobject, bw, nb.mcsimul, plots = FALSE, kernel.type = "Quadratic")
}

\arguments{
  \item{DGobject}{
Object of the \link{DG class}.
}
  \item{bw}{
[Positive numeric] Smoothing bandwidth of the kernel used to estimate strain proportions.
}
  \item{nb.mcsimul}{
[Positive integer] Number of permutations to assess the significance of the ranking.
}
  \item{plots}{
[Logical] If TRUE, plots are produced. The plots show the growth variable in space, the sampling sites, the estimated values of the fitness coefficients and the corresponding permutation-based distributions obtained under the null hypothesis of coefficient equality.
}
  \item{kernel.type}{
[Character string] Type of kernel. Default: Quadratic kernel \eqn{K(u)=(1-u^2)I(0\le u\le1)}, where \eqn{I} is the indicator function. Other possible kernel types: Linear \eqn{K(u)=(1-u)I(0\le u\le1)}, Power3  \eqn{K(u)=(1-u^3)I(0\le u\le1)}, and Power4 \eqn{K(u)=(1-u^4)I(0\le u\le1)}.
}
}


\value{
\item{permutation.estimates}{
Estimates of the fitness coefficients obtained for the permutations (one row for each permutation).
}
\item{estimates}{
Estimates of the fitness coefficients obtained for the raw data.
}
\item{p.values}{
p.values of pairwise permutation tests of equality of the coefficients.
}
}
\references{
Soubeyrand S., Tollenaere C., Haon-Lasportes E. & Laine A.-L. (2014). Regression-based ranking of pathogen strains with respect to their contributions to natural epidemics. PLOS ONE 9(1): e86591.
}
\author{
Soubeyrand, S., Tollenaere, C., Haon-Lasportes, E. and Laine, A.-L. 
}

\seealso{
	\link{DGobj-class}, \link{DGobj.rawdata}, \link{DGobj.simul.mechanistic}, \link{DGobj.simul.regression}
}
\examples{
## Application of the ranking method to a real data set
data(powderymildew)
DGdata=DGobj.rawdata(demographic.coord=powderymildew$demographic.coord,
 genetic.coord=powderymildew$genetic.coord,
 demographic.measures=powderymildew$demographic.measures,
 genetic.frequencies=powderymildew$genetic.frequencies)
ranking.strains(DGobject=DGdata, bw=sqrt(2), nb.mcsimul=10^3, plots=TRUE,
	kernel.type="Power4")

## Application of the ranking method to a data set simulated under the 
## mechanistic model
DGmech=DGobj.simul.mechanistic(sqrtn=10, size1=30, size2=10, theta=c(1.5,2,3), 
	beta=c(5,5), M=7, delta=0.2)
ranking.strains(DGobject=DGmech, bw=sqrt(2), nb.mcsimul=10^3, plots=TRUE,
	kernel.type="Power4")
	
## Application of the ranking method to a data set simulated under the 
## regression model
DGreg=DGobj.simul.regression(sqrtn=10, size1=30, size2=10, theta=c(1.5,2,3), 
 alpha.function=generation.alpha.3strains, sigma=0.1)
ranking.strains(DGobject=DGreg, bw=sqrt(2), nb.mcsimul=10^3, plots=TRUE,
	kernel.type="Power4")
}
\keyword{ misc }

