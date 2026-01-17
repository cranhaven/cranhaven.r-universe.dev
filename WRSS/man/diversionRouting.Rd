\name{diversionRouting}
\alias{diversionRouting}
\title{
  base function for diversion simulation
}
\description{
  Given a sort of demand(s), \code{diversionRouting} function enable us to simulate the performance and effect of a diversion dam  under a givn recharge time series, \code{inflow}, on the drainage network.
}
\usage{
  diversionRouting(demand=NA, priority = NA,
                   capacity, inflow, simulation)
}
\arguments{
  \item{demand}{A matrix: is column-wise matrix of demands, at which the rows presents demands for each time step and columns are for different individual demand sites (MCM).}
  \item{priority}{A vector: is a vector of priorities associated to \code{demand}}
  \item{capacity}{The maximum capacity of diversion dam (CMS).}
  \item{inflow}{A vector : a vector of water flowing into the diversion (MCM)}
  \item{simulation}{A list: \code{simulation} is a list of three vectors: \code{start}, \code{end}, and \code{interval}. the \code{start} and \code{end} components must be in \code{'YYYY-MM-DD'} format. the \code{interval} component can takes either of \code{'day','week'}, or \code{'month'}.}
}
\value{
  the \code{diversionRouting} function returns a list of features given as below:
      \itemize{
        \item  \code{release} : a matrix of release(s) equivalant to each \code{demand} (MCM) 
        \item  \code{diverted}: a vector of diverted volumes (MCM), release(s) are included 
        \item  \code{overflow}: a vector of overflow passing through the diversion (MCM) 
      }
}
\author{
  Rezgar Arabzadeh
}

\seealso{\code{\link[WRSS]{aquiferRouting}}}

\examples{
  demand         <-matrix(rnorm(480,10,3),120)
  priority       <-sample(1:3,4,replace=TRUE)
  capacity       <-12
  inflow         <-rlnorm(120,log(50),log(4))
  simulation     <-list(start='2000-01-01',end='2009-12-29',interval='month')
  res<-diversionRouting(demand=demand,
                        priority=priority,
                        capacity=capacity,
                        inflow=inflow,
                        simulation=simulation)
  plot(ecdf(res$diverted$diverted),xlab='cms',ylab='exceedance probability')
}
\keyword{ list }
\keyword{ matrix }