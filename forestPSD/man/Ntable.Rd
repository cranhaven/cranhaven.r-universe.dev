\name{Ntable}
\alias{Ntable}
\title{
Organize the data into a data format suitable for population structure analysis.
}
\description{
Organize the data into a data format suitable for population structure analysis.
}
\usage{
Ntable(ax)
}
\arguments{
  \item{ax}{
Population number of within different age class.
}
}
\details{
Organize the data into a data format suitable for population structure analysis.
}
\value{
Result returns the data for forest population number of within different age class, the data format id the data.frame.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
data(Npop)
Npop

##Generate the Npop data##
number=c(8283,5238,1921,1425,926,659,479,228,57,24,10)
Ntable(ax=number)
}
