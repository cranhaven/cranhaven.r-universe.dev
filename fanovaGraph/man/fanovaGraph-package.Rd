\name{fanovaGraph-package}
\alias{fanovaGraph}
\docType{package}
\title{Building Kriging models with FANOVA graphs
}
\description{
Estimates and plots the FANOVA graph of a function to identify its interaction structure and fits a kriging model modified by the identified structure
}
\details{
Important functions:
       \tabular{ll}{
         \code{\link{estimateGraph}} \tab Estimate indices for the graph, create graph structure\cr
         \code{\link{threshold}} \tab Set indices below a threshold to zero \cr
         \code{\link{plot.graphlist}} \tab Plot a given graph structure \cr
         \code{\link{plotDeltaJumps}} \tab Provide plots for the choice of the threshold \cr
         \code{\link{kmAdditive}} \tab Kriging model estimation with block-additive kernel \cr
         \code{\link{predictAdditive}} \tab Prediction function from Kriging model with block-additive kernel \cr
         \code{\link{simAdditive}} \tab Simulation from Kriging model with block-additive kernel
       }
}
\author{
J. Fruth, T. Muehlenstaedt, O. Roustant, M. Jastrow
}
\references{
Fruth, J.; Roustant, O.; Kuhnt, S. (2013+) Total interaction index: A variance-based sensitivity index for second-order interaction screening.

Janon, A.; Klein, T.; Lagnoux-Renaudie, A.; Nodet, M.; Prieur, C. (2012+) Asymptotic normality and efficiency of two Sobol index estimators.

Liu, R.; Owen, A.B. (2006) Estimating mean dimensionality of analysis of variance decompositions, \emph{Journal of the American Statistical Association}, \bold{101} 474, 712-721.

Mara, T.A (2009) Extension of the RBD-FAST method to the computation of global sensitivity indices, \emph{Reliability Engineering & System Safety}, \bold{94} no. 8, 1274-1281.

Muehlenstaedt, T.; Roustant, O.; Carraro, L.; Kuhnt, S. (2011) Data-driven Kriging models based on FANOVA-decomposition, \emph{Statistics and Computing}.

Sobol', I. M. (1993) Sensitivity estimates for nonlinear mathematical models, \emph{Mathematical Modeling and Computational Experiment}, \bold{1}, 407-414.
}
\keyword{package}
\seealso{
\code{\link[DiceKriging:DiceKriging-package]{DiceKriging}}, 
\code{\link[sensitivity:sensitivity-package]{sensitivity}},
\code{\link[igraph:igraph-package]{igraph}}
}
\examples{
#demo(ExampleIshigami)
#demo(Example6D)
#demo(Estimation)
#demo(Threshold)
}