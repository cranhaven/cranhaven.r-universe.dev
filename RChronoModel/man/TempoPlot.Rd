\name{TempoPlot}
\alias{TempoPlot}
\title{Plot of the occurence of events}
\usage{
TempoPlot(data, position, level=0.95,  count = TRUE, Gauss=FALSE, title = "Tempo plot")
}
\arguments{
   \item{data}{dataframe containing the output of the MCMC algorithm }
  \item{position}{numeric vector containing the position of the column corresponding to the MCMC chains of interest}
  \item{level}{probability corresponding to the level of confidence used for the credible interval}
  \item{count}{if TRUE the counting process is given as a number, otherwise it is a probability}
  \item{Gauss}{if TRUE, the Gaussian approximation of the CI is used}
  \item{title}{title of the graph}
}
\value{

It calculates the cumulative frequency of specified events by calculating how many events took place before each date in a specified range of dates. 

}
\description{
A statistical graphic designed for the archaeological study of rhythms of the long term that embodies a theory of archaeological evidence for the occurrence of events.
}
\author{
  Anne Philippe <Anne.Philippe@univ-nantes.fr>, Thomas S. Dye  <TSD@tsdye.com> and

  Marie-Anne Vibet <Marie-Anne.Vibet@univ-nantes.fr>
}
\references{
Dye, T.S. (2016) Long-term rhythms in the development of Hawaiian social stratification. Journal of Archaeological Science, 71, 1--9.

}
\examples{
  data(Events); 
  TempoPlot(Events[1:1000,], c(2:5))
  TempoPlot(Events[1:1000,], c(2:5), count = TRUE)
}
\keyword{ tempo plot}
\keyword{ credible interval }
\keyword{ mean }

