\name{MISSING_INFO}
\alias{MISSING_INFO}
\title{Missing value statistics}
\description{Frequencies and proportions of missing values}
\usage{MISSING_INFO(data, verbose)}  
\arguments{
  \item{data}{
  \code{}An all-numeric dataframe where the rows are cases & the columns are the variables.}

  \item{verbose}{
  \code{}(optional) Should detailed results be displayed in console? TRUE (default) or FALSE}
}
\details{
Provides the number of cases with each of N missing values (NA values), along with the proportions,
cumulative proportions, and the cumulative Ns.
}
\value{A matrix with the following columns:
 \item{N_cases}{The number of cases}
 \item{N_missing}{The number of missing values}
 \item{Proportion}{The proportion of missing values}
 \item{Cum_Proportion}{The cumulative proportion of missing values}
 \item{Cum_N}{The cumulative number of cases}
}
\author{Brian P. O'Connor }
\examples{
MISSING_INFO(airquality)

# add NA values to the Rosenberg Self-Esteem scale items, for illustration
data_RSE_missing <- data_RSE
data_RSE_missing[matrix(rbinom(prod(dim(data_RSE_missing)), size=1, prob=.3)==1, 
                 nrow=dim(data_RSE_missing)[1])] <- NA

MISSING_INFO(data_RSE_missing)
}
