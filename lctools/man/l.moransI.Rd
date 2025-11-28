\name{l.moransI}
\alias{l.moransI}

\title{Local Moran's I classic statistic for assessing spatial autocorrelation}

\description{
The local Moran's I proposed by Anselin (1995). The formula to calculate the local \eqn{I_i} which is now used in most textbooks and software is: \deqn{I_i = ((x_i - mean(x))/m_2)*(\Sigma w_{ij}*z_j)}  where \eqn{n} is number of observations, \eqn{w_{ij}} are the weights, \eqn{z_j = x_j - mean(x)}, \eqn{x} being the value of the variable at location \eqn{i} and \eqn{mean(x)} being the mean value of the variable in question, and \eqn{m_2 = (\Sigma (x_i - mean(x))^2) / n}. This function calculates the local Moran's I values for each observation along with goodness of fit statistics, it classifies the observations into five classes (High-High, Low-Low, Low-High, High-Low, and Not Significant) and optionally plots a Moran's I Scatter Plot.}

\usage{l.moransI(Coords, Bandwidth, x, WType='Binary', scatter.plot = TRUE, family = "adaptive")
}
\arguments{
  \item{Coords}{a numeric matrix or vector or data frame of two columns giving the X,Y coordinates of the observations (data points or geometric / population weighted centroids)}
  \item{Bandwidth}{a positive integer that defines the number of nearest neighbours for the calculation of the weights}
  \item{x}{a numeric vector of a variable}
  \item{WType}{string giving the weighting scheme used to compute the weights matrix. 
  Options are: "Binary" and "Bi-square". Default is "Binary".

  Binary: weight = 1 for distances less than or equal to the distance of the furthest neighbour (H), 0 otherwise;

  Bi-square: \eqn{weight = (1-(ndist/H)^2)^2} for distances less than or equal to H, 0 otherwise.}
  \item{scatter.plot}{a logical value that controls if the Moran's I Scatter Plot will be displayed (TRUE) or not. Default is TRUE.}
  \item{family}{a string giving the weighting scheme used to compute the weights matrix. 
  Options are: "adaptive" and "fixed". The default value is "adaptive".
  
  adaptive: the number of nearest neighbours (integer). 
  
  fixed: a fixed distance around each observation's location (in meters).
  }
}
\details{The interpretation of the local \eqn{I_i} is similar to that of the global Moran's I.}

\value{Returns the calculated local Moran's I and a list of statistics for the latter's inference: the expected Ei, the variance Vi, the Xi scores and the p-values for the randomization null hypotheses. It also returns the standardized value and the standardized lagged value of the variable to allow creating the Moran's I scatter plot and the classified values for creating the cluster map similar to those available in GeoDa (Anselin et al., 2006).

  \item{ID}{Numeric index from 1 to n}
  \item{Ii}{Classic lobal Moran's I_i statistic}
  \item{Ei}{The expected local Moran's I_i}
  \item{Vi}{The variance of I_i}
  \item{Zi}{The z score calculated for the randomization null hypotheses test}
  \item{p.value}{The p-value (two-tailed) calculated for the randomization null hypotheses test}
  \item{Xi}{The standardised value of the variable x}
  \item{wXj}{The standardised value of the lagged x (weighted some of nearest neighbours)}
  \item{Cluster}{The class each observation belongs based on the sign of Xi and wXj as well as the non-significant local Moran's I values}
}

\references{
Anselin, L.,1995, Local Indicators of Spatial Association-LISA. Geographical Analysis, 27, 93-115. 

Anselin, L., Syabri, I. and Kho., Y., 2006, GeoDa: An Introduction to Spatial Data Analysis. Geographical Analysis 38(1), 5-22.

Kalogirou, S. (2015) Spatial Analysis: Methodology and Applications with R. [ebook] Athens: Hellenic Academic Libraries Link. ISBN: 978-960-603-285-1 (in Greek). \url{https://repository.kallipos.gr/handle/11419/5029?locale=en}
}
\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\note{Please note that the weights are row standardised.}

\examples{
data(GR.Municipalities)
l.moran<-l.moransI(cbind(GR.Municipalities$X, GR.Municipalities$Y),6,GR.Municipalities$Income01)
}
\keyword{local Moran's I}
\keyword{Moran's I Clusters}
\keyword{LISA}
