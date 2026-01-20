\name{LMO}
\alias{LMO}
\title{
Leave Many Out
}
\description{
Calculate the robustness of the equation using the leave many out method.
}
\usage{
LMO(mydata, cv, n)
}

\arguments{
  \item{mydata}{
Dataframe containing names and values of response and descriptors
}
  \item{cv}{
Numbers of fold
}
  \item{n}{
Numbers of selected descriptors of the regression (determined using Select_MLR)
}
}

\value{
return Q2, the coefficient that measure the robstness
}


\references{
1. Gramatica, P. Principles of QSAR Models Validation: Internal and External. Qsar &
Combinatorial Science 2007, 26, 694-701.  \cr
2. Golbraikh, A.; Tropsha, A. Beware of Q(2)! Journal of Molecular Graphics & Modelling 2002,
20, 269-276.
}


\examples{
# First run Select_MLR to define n

#LMO(mydata,5,dim(MLR)[2])
#LMO(mydata,10,dim(MLR)[2])
}
