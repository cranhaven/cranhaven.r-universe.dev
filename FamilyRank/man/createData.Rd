\name{createData}
\alias{createData}
\title{Simulate Data}
\description{Simulate data sets meant to emulate gene expression data in oncology.}
\usage{
createData(n.case, n.control, mean.upper = 13, mean.lower = 5, 
sd.upper = 1, sd.lower = 1, n.features = 10000, 
subtype1.feats = 1:5, subtype2.feats = 6:10, subtype3.feats = 11:15)
}
\arguments{
  \item{n.case}{Number of cases to simulate.}
  \item{n.control}{Number of controls to simulate}
  \item{mean.upper}{Mean of upper component of bimodal Gaussian distribution from which features are simulated.}
  \item{mean.lower}{Mean of lower component of bimodal Gaussian distribution from which features are simulated.}
  \item{sd.upper}{Standard deviation of upper component of bimodal Gaussian distribution from which features are simulated.}
  \item{sd.lower}{Standard deviation of lower component of bimodal Gaussian distribution from which features are simulated.}
  \item{n.features}{Number of features to simulate}
  \item{subtype1.feats}{Index of features used to define subtype 1.}
  \item{subtype2.feats}{Index of features used to define subtype 2.}
  \item{subtype3.feats}{Index of features used to define subtype 3.}
}
\details{Simulates case/control data as described in \code{\link{createCase}} and \code{\link{createControl}}, and graphical domain knowledge as described in \code{\link{createGraph}}.}
\value{Returns a named list with a simulated feature matrix (\code{x}), simulated binary response vector  (\code{y}), vector of subtype labels (\code{subtype}), and simulated domain knowledge graph (\code{graph}).}
\references{ADD REFERENCE}
\author{Michelle Saul}
\seealso{\code{\link{createCase}}, \code{\link{createControl}}, \code{\link{createGraph}}}

\examples{
## Toy Example
# Simulate data set
# 10 samples
# 20 features
# Features 1 through 15 perfectly define response
# All other features are random noise.
data <- createData(n.case = 5, n.control = 5, mean.upper=13, mean.lower=5,
                   sd.upper=1, sd.lower=1, n.features = 20,
                   subtype1.feats = 1:5, subtype2.feats = 6:10,
                   subtype3.feats = 11:15)
x <- data$x
y <- data$y
graph <- data$graph
}