\name{TempoActivityPlot}
\alias{TempoActivityPlot}
\title{Plot of the activity of events}
\usage{
TempoActivityPlot(data, position, level=0.95,  count = TRUE, 
title = "Activity plot")
}
\arguments{
   \item{data}{dataframe containing the output of the MCMC algorithm }
  \item{position}{numeric vector containing the position of the column corresponding to the MCMC chains of interest}
  \item{level}{probability corresponding to the level of confidence used for the credible interval}
  \item{count}{if TRUE the counting process is given as a number, otherwise it is a probability}
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
  TempoActivityPlot(Events[1:1000,], c(2:5))
  TempoActivityPlot(Events[1:1000,], c(2:5), count = TRUE)
}
\keyword{ tempo activity plot}
\keyword{ mean }

