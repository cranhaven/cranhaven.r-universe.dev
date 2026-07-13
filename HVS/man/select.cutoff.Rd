\name{select.cutoff}
\alias{select.cutoff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Select the cut-off using HVS
}
\description{
Selects a p-value cut-off for hypergraph variable selection on a graph. 
}
\usage{
select.cutoff(G,batchsize=100,nbatch=100,level=0.05,threshold=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{G}{
    The graph, represented as a square matrix. The values in the matrix
    represent the p-values of hypothesis tests corresponding to the
    edges of the graph (with diagonal elements corresponding to loops).
  }
  \item{batchsize}{
    The size of each batch of points.
  }
  \item{nbatch}{
    The number of batches
  }
  \item{level}{
    The desired FDR control level. If threshold is set to a numeric
    value, then this value is overridden.
  }
  \item{threshold}{
    Either a numeric value or null or "PRDS". If the value is null or
    "PRDS", then the value is recalculated to control the gFDR at the
    specified level, either under no dependence assumptions, or under
    the PRDS assumption.
  }  
}
\details{
  This selects the largest cut-off c such that the number of vertices
  minus the log to base 2 of the number of independent sets of the graph
  with edges having values below this cut-off, is more than or equal to
  c times the parameter "threshold". 
}
\value{
  An object of class c("HVS","GLSUP") which includes the following components:
  \describe{
    \item{level}{The specified gFDR control level.}
    \item{pv}{The p-values in increasing order.}
    \item{sizing}{The sizing function applied to the hyperedges with
      smallest p-values.}
    \item{rejected}{Logical vector indicating which p-values are less
      than the cut-off.}
    \item{cut.off.slope}{The specified or calculated threshold.}
    \item{pv.cut.off}{The p-value cut-off.}
    \item{sizing.function}{The estimated sizing function from isotonic
      regression. This can be studied to determine the uncertainty in
      counting the number of sets.}
  }
}
\references{
  Hypergraph Variable Selection with False Discovery Rate Control

Sarah Organ, Toby Kenney, Hong Gu

http://arxiv.org/abs/2606.20514
}
\author{
Toby Kenney,  Sarah Organ
}
\examples{
set.seed(1)
G<-matrix(runif(400)/10,20,20)
G<-pmin(G,t(G)) # make G symmetric 

cut.off<-select.cutoff(G,threshold="PRDS")
}
