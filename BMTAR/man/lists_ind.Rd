\name{lists_ind}
\alias{lists_ind}
\title{Create indicator vector for the regimen of each observation}
\usage{
lists_ind(r,Zt,l,...)
}
\arguments{
\item{r}{value for the threshold variable}
\item{Zt}{threshold processes univariate}
\item{l}{number of regimes}
\item{...}{other arguments passed to specific methods}
}
\value{
Vector of length N indicating the number of the regime of each observation
}
\description{
From a threshold variable and the corresponding processes calculate a vector indicating the number of the regime of each observation.
}
