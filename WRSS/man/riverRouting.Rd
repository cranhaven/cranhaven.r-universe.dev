\name{riverRouting}
\alias{riverRouting}
\title{
  base function for rivers and reachs simulation
}
\description{
  Given a sort of demand(s), \code{riverRouting} function enable us to simulate rivers and channels under givn a hydrologic time series, \code{inflow}, and optional \code{demand}(s).
}
\usage{
  riverRouting(demand=NA, priority = NA, discharge, seepageFraction=NA, simulation)
}
\arguments{
  \item{demand}{(optional) A matrix: is column-wise matrix of demands, at which the rows presents demands for each time step and columns are for different individual demand sites (MCM).}
  \item{priority}{(optional) A vector: is a vector of priorities associated to \code{demand}}
  \item{discharge}{(optional) A vector : a vector of water flowing into the diversion (MCM)}
  \item{seepageFraction}{(optional) The seepage coeffcient of river discharge flow. The seepage is computed as the product of seepageFraction and river discharge. It is in \code{[0, 1]} interval}
  \item{simulation}{A list: \code{simulation} is a list of three vectors: \code{start}, \code{end}, and \code{interval}. the \code{start} and \code{end} components must be in \code{'YYYY-MM-DD'} format. the \code{interval} component can takes either of \code{'day','week'}, or \code{'month'}.
  }
}
  \value{
    the \code{riverRouting} returns a matrix of release(s) corresponding to each demand(s).
  }
  \author{
    Rezgar Arabzadeh
  }
  
  \seealso{\code{\link[WRSS]{diversionRouting}}}
  
  \examples{
    demand         <-matrix(rnorm(480,15,3),120)
    priority       <-sample(1:3,4,replace=TRUE)
    discharge      <-rlnorm(120,log(50),log(4))
    simulation     <-list(start='2000-01-01',end='2000-04-29',interval='day')
    
    riverRouting(demand    = demand   ,
                 priority  = priority ,
                 discharge = discharge,
                 simulation= simulation)
    
  }
  \keyword{ matrix }
  