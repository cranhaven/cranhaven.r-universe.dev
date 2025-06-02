\name{batch}

\alias{batch}
\alias{batch,Rcpp_QueftsModel-method}


\title{
Batch QUEFTS model predictions
}

\description{
Make many predictions with a QUEFTS model. 
}


\usage{
\S4method{batch}{Rcpp_QueftsModel}(x, supply, fert, yatt, leaf_ratio, stem_ratio, var="yield")
}


\arguments{
  \item{x}{QUEFTSModel}
  \item{supply}{matrix or data.frame with soil nutrient supply data for N, P, and K}  
  \item{fert}{matrix or data.frame with fertilizer nutrient supply data for N, P, and K}  
  \item{yatt}{numeric. Attainable yield}  
  \item{leaf_ratio}{positive numeric (typically between 0 and 1) indicating the leaf weight relative to the storage organ weight. For example: 0.46 for maize, 0.17 for potato, and 0.18 for rice}  
  \item{stem_ratio}{positive numeric (typically between 0 and 1) indicating the stem weight relative to the storage organ weight, For example: 0.56 for maize, 0.14 for potato, and 0.67 for rice}  
  \item{var}{character. Output variable name. Either "yield" or "gap"}  
}


\value{
numeric or matrix (if \code{var="gap"})
}

\examples{
potato <- quefts_crop("potato")
q <- quefts(crop=potato)
fert=cbind(c(0,100), c(0,200), c(0,30))
supply=cbind(50,50,25)
yatt <- 10000
batch(q, supply, fert, yatt, 0.45, 0.4)

batch(q, supply, fert, yatt, 0.45, 0.4, var="gap")
}

