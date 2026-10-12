\name{Cocktail}
\alias{Cocktail}
\title{
  Beverages paired comparison
}
\description{
    Paired comparison of 7 beverages by 112 subjects according their preferences
}
\usage{data(Cocktail)
}
\format{
  A \code{DataPairComp} class object with the following elements:

 \code{Cons} : corresponding to the label of consummers (default : Number of consummers)
 
 \code{Crit} : name of the different criteria contained
 
 \code{Prod} : names of the different products (default : number of the product)
 
 \code{Paircomp} : \code{list} of number of criteria elements each corresponding to the results of paired comparisons performed by the consummers.
}

\examples{
  data(Cocktail)
  show(Cocktail)
}