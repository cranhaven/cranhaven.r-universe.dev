\name{em.control}
\alias{em.control}
\title{
Control Parameters for the EM algorithm
}
\description{
Input parameters for the EM algorithm.
}
\usage{
em.control(pi.initial = NULL, max.iter = 10000, tol = 1e-12,
           nr.threads = 0, verbose = TRUE)
}
\arguments{
  \item{pi.initial}{
Initial guess for the probabilities of the vectors of associations status. If \code{NULL} then 0.9 is assigned for the \code{c(0,\ldots,0)} configuration and 0.1 is distributed uniformly for all other configurations.
}
\item{max.iter}{
Maximum number of EM iterations.
}
  \item{tol}{
Tolerance (in maximum absolute difference between two EM iterations in estimated probabilities) before declaring convergence and stopping.
}
  \item{nr.threads}{
  Number of processing threads to use. If zero (the default), will automatically detect the number of compute cores available and spawn one thread per core.
  }
  \item{verbose}{
An indicator of whether to report progress (running iteration number) during computation.
  }
}
\details{
The function is used inside the \code{control} argument in \code{\link{repfdr}} and \code{\link{piem}}.
}
\value{
A list with the input values.
}
\seealso{
\code{\link{repfdr}} \code{\link{piem}}
}
\examples{
\dontrun{
download.file('http://www.math.tau.ac.il/~ruheller/repfdr_RData/binned_zmat.RData',
  destfile = "binned_zmat.RData")
load(file = "binned_zmat.RData")
out <- repfdr(pbz,bz,"replication",
              control = em.control(pi.initial = c(0.48,rep(0.02,26)),
              verbose = TRUE, nr.threads = 1))
# iterations are printed; run bit slower (1 thread)
}
}
