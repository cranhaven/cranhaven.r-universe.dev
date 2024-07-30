\name{affluence}
\alias{affluence}
\docType{data}
\title{
Equivalised income}
\description{The database contains information about equivalised income of households.}
\usage{data("affluence")}

\format{
  A data frame with 2000 observations on the following 4 variables.
  \describe{
    \item{\code{income}}{a numeric vector (equivalised income of households; equivalisation using modified OECD scale)}
    \item{\code{education}}{a numeric vector (education of the household's head: 1=tertiary, 2=secondary, 3=basic vocational, 4=low)}
    \item{\code{age}}{a numeric vector (age of the household's head: 1=less than 35, 2=35-44, 3=45-59, 4=60 and more)}
    \item{\code{sex}}{a numeric vector (sex of the household's head: 0=male, 1=female)}
	\item{\code{hs_size}}{vector of weights}
  }
}

\source{
Based on Council for Social Monitoring (2016). Integrated database.http://www.diagnoza.com [11.09.2016]. 
}
\examples{
data(affluence)
names(affluence)
}
\keyword{datasets}
