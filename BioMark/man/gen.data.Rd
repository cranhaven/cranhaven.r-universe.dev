\name{gen.data}
\alias{gen.data}
\alias{gen.data2}
\title{Simulate data sets}
\description{
The functions \code{gen.data} and \code{gen.data2} generate one or more
two-class data matrices where the first \code{nbiom} variables are changed
in the treatment class. The aim is to provide an easy means to evaluate the
performance of biomarker identification methods. Function
\code{gen.data} samples from a multivariate normal distribution;
\code{gen.data2} generates spiked data either by adding differences to
the first columns, or by multiplying with factors given by the
user. Note that whereas \code{gen.data} will provide completely new
simulated data, both for the control and treatment classes,
\code{gen.data2} essentially only changes the biomarker part of the
treated class.}
\usage{
gen.data(ncontrol, ntreated = ncontrol, nvar, nbiom = 5, group.diff = 0.5,
         nsimul = 100, means = rep(0, nvar), cormat = diag(nvar))
gen.data2(X, ncontrol, nbiom, spikeI,
          type = c("multiplicative", "additive"),
          nsimul = 100, stddev = .05)
}
\arguments{
  \item{ncontrol, ntreated}{Numbers of objects in the two classes. If only
    ncontrol is given, the two classes are assumed to be of equal size,
    or, in the case of \code{gen.data2}, the remainder of the samples are
    taken to be the treatment samples.}
  \item{nvar}{Number of variables.}
  \item{nbiom}{Number of biomarkers, i.e. the number of variables to be
    changed in the treatment class compared to the control class. The
    variables that are changed are always the first variables in the
    data matrix.}
  \item{group.diff}{group difference; the average difference between
    values of the biomarkers in the two classes.}
  \item{nsimul}{Number of data sets to simulate.}
  \item{means}{Mean values of all variables, a vector.}
  \item{cormat}{Correlation matrix to be used in the simulation. Default
    is the identity matrix.}
  \item{X}{Experimental data matrix, without group differences.}
  \item{spikeI}{A vector of at least three different numbers, used to
    generate new values for the biomarker variables in the treated class.}
  \item{type}{Whether to use multiplication (useful when simulating
    cases where things like "twofold differences" are relevant), or
    addition (in the case of absolute differences in the treatment and
    control groups).}
  \item{stddev}{Additional noise: in every simulation, normally
    distributed noise with a standard deviation of
    \code{stddev * mean(spikeI)} will be added to \code{spikeI} before
    generating the actual simulated data.}
}
\value{A list with the following elements:
  \item{X}{An array of dimension \code{nobj1 + nobj2} times \code{nvar}
    times \code{nsimul}.}
  \item{Y}{The class vector.}
  \item{n.biomarkers}{The number of biomarkers.}

  Note that the biomarkers are always in the first \code{nbiom} columns
  of the data matrix.
}
\details{The \code{spikeI} argument in function \code{gen.data2}
  provides the numbers that will be used to artificially "spike" the
  biomarker variables, either by multiplication (the default) or by
  addition. To obtain approximate two-fold differences, for example, one
  could use \code{spikeI = c(1.8, 2.0, 2.2)}. At least three different
  values should be given since in most cases more than one set will be
  simulated and we require different values in the biomarker
  variables.
  }
\author{Ron Wehrens}
\examples{
\dontrun{
X <- gen.data(10, nvar = 200)
names(X)
dim(X$X)

set.seed(7)
simdat <- gen.data(10, nvar = 1200, nbiom = 22, nsimul = 1,
                   group.diff = 2)
simdat.stab <- get.biom(simdat$X[,,1], simdat$Y, fmethod = "all",
                        type = "stab", ncomp = 3, scale.p = "auto")
## show LASSO success
traceplot(simdat.stab, lty = 1, col = rep(2:1, c(22, 1610)))

data(SpikePos)
real.markers <- which(SpikePos$annotation$found.in.standards > 0)
X.no.diff <- SpikePos$data[1:20, -real.markers]

set.seed(7)
simdat2 <- gen.data2(X.no.diff, ncontrol = 10, nbiom = 22,
                     spikeI = c(1.2, 1.4, 2), nsimul = 1)
simdat2.stab <- get.biom(simdat2$X[,,1], simdat$Y,
                         fmethod = "all", type = "stab", ncomp = 3,
                         scale.p = "auto")
## show LASSO success
traceplot(simdat2.stab, lty = 1, col = rep(2:1, c(22, 1610)))
}
}
\keyword{manip}
\keyword{multivariate}
