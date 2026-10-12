\name{DataSimulH0}
\alias{DataSimulH0}
\title{
  Simulation of paired comparison data
}
\description{
  Returns paired comparison data according a given configuration
}
\usage{
DataSimulH0(Data, ResH0)
}
\arguments{
  \item{Data}{
    Object of class \code{DataPairComp}
}
  \item{ResH0}{
    Object of class \code{BradleyEstim}.
}
}
\details{
The paired comparison data are simulated according the products configuration, the weight of the different classes for the different criteria (stored in the object \code{ResH0} of class \code{BradleyEstim}) obtained on the basis of the results of \code{EstimBradley} function for the paired comparison data contained in the objet \code{Data} of class \code{DataPairComp}
}
\value{
Object of class \code{DataPairComp} with the following components:
  
  
 \code{Cons} : corresponding to the label of consummers 
 
 \code{Crit} : names of the different criteria 
 
 \code{Prod} : names of the different products 
 
 \code{Paircomp} : \code{list} of number of criteria elements each corresponding to the results of simulated paired comparisons performed by the consummers according their belonging to the different classes.
}