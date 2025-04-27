\name{tot.rss}
\alias{tot.rss}

\title{
Total Residual Sum of Squares for Broken Stick Model
}
\description{
Calculates the total residual sum of squares for broken stick model (2 part)
}
\usage{
tot.rss(data, break.pt, xvar, yvar)
}

\arguments{
  \item{data}{
data frame for calculating total residual sum of squares.
}
  \item{break.pt}{
This is the data point at which the data are split for a broken stick model.
}
  \item{xvar}{
The x-variable in the data frame for broken stick model.
}
  \item{yvar}{
The y-variable in the data frame for broken stick model.
}
}
\value{
The residual sum of squares of a broken stick model with a specified break point.
}
\author{
Tyler L. Moulton
}
\seealso{
code{\link{sumsq}}
code{\link{get.pcrit}}
}
\examples{
## load data ##
data(fishMR)

## subset data to appropriate region ##

data<-fishMR[fishMR$DO.mgL < 4,] 
data$times <- data$times-min(data$times)
data<-data[data$times< 6800,]

## calculate total RSS for different breakpoints ##

a1 <- tot.rss(data, break.pt = 4000,
xvar = "times", yvar = "DO.mgL") 
a2 <- tot.rss(data, break.pt = 4250,
xvar = "times", yvar = "DO.mgL")
a3 <- tot.rss(data, break.pt = 4500,
xvar = "times", yvar = "DO.mgL")
a4 <- tot.rss(data, break.pt = 4750,
xvar = "times", yvar = "DO.mgL") 
a5 <- tot.rss(data, break.pt = 5000,
xvar = "times", yvar = "DO.mgL")
a6 <- tot.rss(data, break.pt = 5250,
xvar = "times", yvar = "DO.mgL")

# a5 represents the break point for the 
# best broken stick linear model of the
# above 6 options.

}
