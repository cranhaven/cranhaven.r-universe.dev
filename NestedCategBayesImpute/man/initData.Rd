\name{initData}
\alias{initData}
\title{
Initialize the input data structure.
}
\description{
Initialize the input data structure.
}
\usage{
initData(md)
}
\arguments{
  \item{md}{A list holds all the input data with optional missing data info.}
}
%\details{
%Curently the variables and the level of each variable are harded coded. This needs to be changed later on once it is generalized.
%}
\value{
A list object including all the necessary data variables needed by the sampler.
\item{origdata}{Original data.}
\item{n_i}{Vector containing the number of individuals in each household in the data.}
\item{n}{Number of households in the data}
\item{HHdataorigT}{The transposed household level data -- each column now represents each household.}
\item{HHserial}{Vector containing the household index for each individual in the data.}
\item{n_individuals}{The total number of individuals N across all n households in the input data.}
\item{n_individuals_real}{The real total number of individuals N across all n households. The is the same as n_individuals if the household head hasn't been moved to the household level and different otherwise.}
\item{p}{Number of individual-level variables.}
\item{d}{Vector containing the number of levels for each of the \verb{p} variables.}
\item{dataT}{The transposed individual level data -- each column now represents each individual.}
\item{maxd}{The max value in \verb{d}}
\item{n_star_h}{Vector containing the number of observed households for the different household sizes in the original data.}
}

\author{
Quanli Wang
}

