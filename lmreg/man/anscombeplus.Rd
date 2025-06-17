\name{anscombeplus}
\alias{anscombeplus}
\docType{data}
\title{
Six data sets with similar regression summary
}
\description{
Six synthetic data sets with similar regression summary, for illustrating the importance of regression diagnostics.
}
\details{
This data set is presented by Sengupta and Jammalamadaka (2019), after expanding on the ideas of Anscombe (1973)
}
\usage{data(anscombeplus)}
\format{
  A data frame with 20 observations on 8 synthetic real-valued variables, labelled as \code{x1}, \code{y1}, \code{y2}, \code{y3}, \code{y4}, \code{y5}, \code{x2}, \code{y6}.
  \describe{
    \item{\code{x1}}{Explanatory variable of first five data sets}
    \item{\code{y1}}{Response variable of first data set}
    \item{\code{y2}}{Response variable of second data set}
    \item{\code{y3}}{Response variable of third data set}
    \item{\code{y4}}{Response variable of fourth data set}
    \item{\code{y5}}{Response variable of fifth data set}
    \item{\code{x2}}{Explanatory variable of sixth data set}
    \item{\code{y6}}{Response variable of sixth data set}
  }
}
  
\source{
Anscombe, F.J. (1973), Graphs in statistical analysis, American Statistician,
vol.27, pp.17-21. 

Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach, World Scientific Publishing Co., Table 5.1.
}

\examples{
data(anscombeplus)
head(anscombeplus)
}
\keyword{datasets}
