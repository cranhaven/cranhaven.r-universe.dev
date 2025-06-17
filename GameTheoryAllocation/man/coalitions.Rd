\name{coalitions}
\alias{coalitions}
\title{
coalitions
}
\description{
This function gives all the coalitions in a binary mode and usual way.
}
\usage{
coalitions(n)
}
\arguments{
  \item{n}{
Number of the involved players
}
}

\value{
A list with the following components:
\item{Binary}{A matrix where each row indicates a binary representation of the coalition in a binary mode. The second one, the usual way.}
\item{Classic}{A vector with the associated representation of the coalitions.}
}
\author{
A. Saavedra-Nieves
}
\examples{
coalitions(3)
#$Binary
#     [,1] [,2] [,3]
#[1,]    0    0    0
#[2,]    1    0    0
#[3,]    0    1    0
#[4,]    0    0    1
#[5,]    1    1    0
#[6,]    1    0    1
#[7,]    0    1    1
#[8,]    1    1    1
#
#$Classic
#[1] "0"           "'{ 1 }'"     "'{ 2 }'"     "'{ 3 }'"     "'{ 1,2 }'"  
#[6] "'{ 1,3 }'"   "'{ 2,3 }'"   "'{ 1,2,3 }'
}
\keyword{coalition}% use one of  RShowDoc("KEYWORDS")
\keyword{coa}% __ONLY ONE__ keyword per line
