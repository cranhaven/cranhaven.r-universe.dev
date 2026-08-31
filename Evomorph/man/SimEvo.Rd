\name{SimEvo}
\alias{SimEvo}
\title{Simulation of Shape Variation}
\description{\code{SimEvo} performs a simulation of shape variation using a modification of Lande's evolutionary model Polly 2004}
\usage{
SimEvo(vari, consensusvec, resids, ngen, fsamp)
}
\arguments{
  \item{vari}{Variation coefficient}
  \item{consensusvec}{Consensus shape (vectorized form)}
  \item{resids}{GPA residuals matrix}
  \item{ngen}{Number of generations of simulations (Default: 1000000 steps).}
  \item{fsamp}{Frequency of samples (Default: 1000000 steps).}
  }
\details{ 
Lande's evolutionary model defines mean morphological variation over generations \eqn{ \Delta Z } as:
	\deqn{ \Delta Z = \beta G }{ Delta Z = beta*G }
where \eqn{ G } is the additive genetic covariance matrix, and \eqn{ \beta } are selection coefficients applied to the morphological structure.
Polly 2004 proposes a modification of this equation in order to use it with morphological data instead of genetic data:
	\deqn{ \Delta Z = \beta P H }{ Delta Z = beta*H*P }
where \eqn{ P } is the phenotypic covariance matrix, and \eqn{ H } is an heritability matrix (See Polly 2004 for more information).
\code{ resids } will be used as the phenotypic covariance matrix, and \code{ vari } will be used to simulate \eqn{ \beta H } term. After \code{ ngen } simulations steps the new shape will be
reconstructed from the starting shape \code{ consensusvec }.
The number of plots representing the new shapes can be modify using \code{ fsamp }.
}

\value{
It returns a list of ngen/fsamp shapes (landmarks coordinates)
}
\references{Polly, P. D. (2004). On the simulation of the evolution of morphological shape: multivariate shape under selection and drift. Palaeontologia Electronica, 7(2), 1-28.}
\author{Cabrera Juan Manuel} 
\examples{ 
data("aegla_landmarks")

#Use GpaResiduals function to obtain GPA residual matrix and consensus
# coordinates from landmark configuration

a_data=GpaResiduals(aegla_landmarks)

#Simulate morphological evolution with a variation rate "vari"
# trough "ngen" generations and retrieve the last generation shape coordinates

simshape = SimEvo(vari = 2, consensusvec = a_data$cvectorized, 
resids = a_data$resid, ngen = 10000, fsamp = 10000)

#Plot consensus shape and the simulated shape

par(mfrow=c(1, 2))
plot(a_data$consens,type = "p",main = "Reference", xlab = "", ylab = "")
plot(simshape[[1]],type = "p",col = "red",main = "Target", xlab = "", ylab = "")

#Or you can use PlotVariations to see the difference more clearly

PlotVariations(simshape,a_data$consens)

}