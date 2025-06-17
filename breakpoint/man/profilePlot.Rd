\name{profilePlot}
\alias{profilePlot}
\title{
Mean profile plot}
\description{
Plotting function to obtain mean profile plot of the testing dataset based on the estimates of the break-points. An R object created from the CE.Normal, CE.NB ot CE.ZINB is required. User can alter the axis names. 
}
\usage{
profilePlot(obj, data, x.label = "Data Sequence", y.label = "Value")
}
\arguments{
  \item{obj}{
R object created from CE.Normal, CE.NB or CE.ZINB.
}
  \item{data}{
data to be analysed. A single column array or a dataframe.
}
  \item{x.label}{
x axis label. Default is "Data Sequence".
}
  \item{y.label}{
y axis label. Default is "Value".
}
}

\author{
Priyadarshana, W.J.R.M. <mjayawardana@swin.edu.au>
}

\seealso{
 \code{\link{CE.Normal.Mean}}, 
 
 \code{\link{CE.NB}}, 
 
 \code{\link{CE.ZINB}}.
}
\examples{
data(ch1.GM03563)
\dontrun{
## CE with four parameter beta distribution ##
obj1 <- CE.Normal.Mean(ch1.GM03563, distyp = 1, penalty = "mBIC", parallel =TRUE)
profilePlot(obj1)

## CE with truncated normal distribution ##
obj2 <- CE.Normal.Mean(ch1.GM03563, distyp = 2, penalty = "mBIC", parallel =TRUE)
profilePlot(obj2)
}}
