
\name{crime}
\alias{crime}
\docType{data}
\encoding{UTF-8}
\title{Crime data}
\description{Data collected by FBI's Uniform Crime on the violent and property crimes of 236 cities.
}
\usage{data(crime)}
\format{ 
  A list of 3 objects: Y, department and state. Y represents the crime rate data array from 236 cities. Department is the police department names and state represents the states where each city is located at. Y is of dimensionality 10 x 13 x 236 with 236 crime rates on the following 10 variables from year 2000 through 2012. 
  \describe{
   \item{Population}{Population of each city;}
   \item{Violent Crime rate}{Total number of violent crimes;}
   \item{Murder and non-negligent manslaughter rate}{Number of murders;}
   \item{Forcible rape rate}{Number of rape crimes;}
   \item{Robbery rate}{Number of robberies;}
   \item{Aggravated assault rate}{Number of assaults;}
   \item{Property crime rate}{Total number of property crimes;}
   \item{Burglary rate}{Number of burglary crimes;}
   \item{Larceny-theft rate}{Number of theft crimes;}
   \item{Motor vehicle theft rate}{Number of vehicle theft crimes;}
  }
}
\details{The data have been made publicly available by FBI's Uniform Crime Reports.}
\examples{

data(crime)

}

\keyword{datasets}
