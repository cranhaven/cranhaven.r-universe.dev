\name{nutrient}
\alias{nutrient}
\docType{data}
\title{
Sample data of decision ranking table for mapping soil nutrient condition
}
\description{
This is an 11-factor table of decision ranking of soil nutrient indicators
}
\usage{data("nutrient")}
\format{
  A dataframe with 11 factors for pairwise decision ranking of soil nutrient indicators.
   }
\details{
The ranks are the recipricals of Saaty's scale of relative importance which are between 1 and 9
}
\source{
Hypothetical data of rank between soil nutrient indicators
}
\examples{
data(nutrient)
str(nutrient)
plot(nutrient)
}
\keyword{datasets}
