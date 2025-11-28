\name{mc.lcorrel}
\alias{mc.lcorrel}
\title{Monte Carlo simulation for the significance of the local correlation coefficients}
\description{In order to assess if the spatial variation of the local correlation coefficients is statistically significant this function computes original and simulated statistics. LPCCs and GWPCCs can be calculated for a fixed bandwidth for the original locations of the observations as well as for a user-defined number of geographical reallocations of the observations. The latter is a simple Monte Carlo simulation proposed by Hope (1968) and adopted by Fotheringham et al. (2002) who assess if local parameter estimates in a Geographically Weighted Regression model exhibit spatial non-stationarity. 
First, the variances of LPCCs and GWPCCs, respectively, are computed for observed and simulated local correlation coefficients. Then, a pseudo p-value is calculated as p=(1+C)/(1+M) where C is the number of cases in which the variance of the simulated LPCCs and GWPCCs is equal to or higher than the variance of the observed LPCCs and GWPCCs, respectively of each test, and M is the number of permutations. If p<=0.05 it can be argued that the spatial variation of the local correlation coefficients is statistically significant. For this approach, a minimum of 19 permutations is required.}

\usage{
mc.lcorrel(Nsim=99,bwSIM,CorVars,Coord.X,Coord.Y) 
}

\arguments{
    \item{Nsim}{a positive integer that defines the number of the simulation's iterations}
    \item{bwSIM}{A positive value between 0 and 1 to define the proportion of the total observations for the local sample for which each time the local correlation coefficients will be calculated for.}
    \item{CorVars}{A data frame of two variables for which observed and simulated local correlation coefficients (LPCCs and GWPCCs) will be calculated for.}
    \item{Coord.X}{a numeric vector giving the X coordinates of the observations (data points or geometric centroids)}
    \item{Coord.Y}{a numeric vector giving the Y coordinates of the observations (data points or geometric centroids)}
}

\details{For 0.05 level of significance in social sciences, a minimum number of 19 simulations (Nsim>=19) is required. We recommend at least 99 and at best 999 iterations}

\value{Returns a list of summary statistics for the simulated values of LPCCs and GWPCCs, the observed LPCCs and GWPCCs and the pseudo p-value of significance for the spatial variation of the LPCCs and GWPCCs, respectivelly
  \item{SIM}{a dataframe with simulated values: SIM.ID is the simulation ID, SIM.gwGini is the simulated Gini of neighbours, SIM.nsGini is the simulated Gini of non-neighbours, SIM.SG is the simulated share of the overall Gini that is associated with non-neighbour pairs of locations, SIM.Extr = 1 if the simulated SG is greater than or equal to the observed SG}
  \item{LC.Obs}{list of 7 Data Frames as in lcorrel}
  \item{pseudo.p.lpcc}{pseudo p-value for the significance of the spatial variation of the LPCCs: if this is lower than or equal to 0.05 it can be argued that the the spatial variation of the LPCCs is statistically significant.}
  \item{pseudo.p.gwpcc}{pseudo p-value for the significance of the spatial variation of the GWPCCs: if this is lower than or equal to 0.05 it can be argued that the the spatial variation of the GWPCCs is statistically significant.}
}
\references{
Hope, A.C.A. (1968) A Simplified Monte Carlo Significance Test Procedure, Journal of the Royal Statistical Society. Series B (Methodological), 30 (3), pp. 582 - 598.

Fotheringham, A.S, Brunsdon, C., Charlton, M. (2002) Geographically Weighted Regression: the analysis of spatially varying relationships, Chichester: John Wiley and Sons.}

\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\examples{
X<-rep(11:14, 4)
Y<-rev(rep(1:4, each=4))
var1<-c(1,1,1,1,1,1,2,2,2,2,3,3,3,4,4,5)
var2<-rev(var1)
Nsim= 19
bwSIM<-0.5

SIM20<-mc.lcorrel(Nsim,bwSIM, cbind(var1,var2),X,Y)

SIM20$pseudo.p.lpcc
SIM20$pseudo.p.gwpcc
}

\keyword{Monte Carlo Simulation}
\keyword{LPCCs inference}
\keyword{GWPCCs inference}
\keyword{local multi-collinearity}
