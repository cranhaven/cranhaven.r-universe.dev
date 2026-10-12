\name{ClassDataPairComp}
\alias{ClassDataPairComp}
\title{
Create an object of class DataPairComp}
\description{
return an object of \code{DataPairComp} class}
\usage{
ClassDataPairComp(Mat, labelprod = NULL, labelcons = NULL, labelcrit = NULL)
}
\arguments{
  \item{Mat}{
Paired comparison matrix with a number of rows equal to nsubject*nitems and nitems columns.
}
  \item{labelprod}{
 names of the different items (default \code{labelprod=NULL})
}
  \item{labelcons}{
names of the different subjects (default \code{labelcons=NULL})
}
  \item{labelcrit}{
name of the criterium (default \code{labelcrit=NULL})
}
}

\value{
Object of class \code{DataPairComp} with the following elements:
  
 \code{Cons} : corresponding to the label of consummers (default : Number of consummers)
 
 \code{Crit} : name of the different criteria contained
 
 \code{Prod} : names of the different products (default : number of the product)
 
 \code{Paircomp} : \code{list} of number of criteria elements each corresponding to the results of paired comparisons performed by the consummers.
}
