\name{unconditional.sort}
\alias{unconditional.sort}

\title{Unconditional Portfolio Sort

}
\description{Calculates out-of-sample mean sub-portfolio returns and the composition of each sub-portfolio using the unconditional portfolio sorting method.
}
\usage{unconditional.sort(Fa,Fb=NULL,Fc=NULL,R.Forward,dimA,dimB=NULL,dimC=NULL,type = 7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Fa}{xts-object containing data for the first dimension of sort
}
\item{Fb}{xts-object containing data for the second dimension of sort (optional)
}
\item{Fc}{xts-object containing data for the third dimension of sort (optional) 
}
\item{R.Forward}{xts-object containing forward returns 
}
\item{dimA}{vector of break points between 0 and 1  
}
\item{dimB}{vector of break points between 0 and 1 (optional)  
}
\item{dimC}{vector of break points between 0 and 1 (optional)
}
\item{type}{pass-through parameter to the \code{\link[stats]{quantile}} function
}
}
\details{
The unconditional sort function sorts assets based on each factor (Fa to Fc) from low to high independently at each time \emph{t} and forms sub-portfolios based on the intersection between them.  Based on the sorted assets in each sub-portfolio at time \emph{t}, mean out-of-sample sub-portfolio returns are computed for time \emph{t+1}.  The function outputs out-of-sample returns for each sub-portfolio in columns and a list of the sub-portfolio constituents at each rebalancing point.  
}
\value{
\item{returns}{Out-of-sample sub-portfolio returns}
\item{portfolio}{List of the sub-portfolio constituents over time}
}
\note{
The function implicitly handles NA/NaN or Inf values at each rebalancing point (at time \emph{t}) by excluding them from the \code{\link[stats]{quantile}} function.  Furthermore, if there are any NA, NaN or Inf values in the R.Forward object when computing out-of-sample returns, these are also excluded.  The function outputs returns in columns.  For example, if a double sort is conducted with both Fa and Fb including 3 breakpoints (a 3v3) sort, column 1 will contain out-of-sample returns for the 'Low-Low' sub-portfolio, column 4 will contain out-of-sample returns for the 'Mid-Low' sub-portfolio whilst column 9 will contain the 'High-High' sub-portfolio returns.      
}
\author{
Jonathan Spohnholtz and Alexander Dickerson
}

\examples{
# Load the included data
library(portsort)
data(Factors)

# Specifiy the sort dimension - in this case, a double sort on lagged returns and Bitcoin volumes
# with 4 breakpoints (a 4v4 sort)
dimA = c(0,0.25,0.5,0.75,1)
dimB = c(0,0.25,0.5,0.75,1)

# Specify the factors for the double sort
# Lagged returns, lagged volumes are stored in the Factors list

R.Forward = Factors[[1]]; R.Lag = Factors[[2]]; V.Lag = Factors[[3]]

# Subset the data from late 2017
R.Forward = R.Forward["2017-12-01/"]
R.Lag = R.Lag["2017-11-30/2018-09-05"]
V.Lag = V.Lag["2017-11-30/2018-09-05"]

Fa = R.Lag
Fb = V.Lag

# Conduct an unconditional sort
sort.output <- conditional.sort(Fa,Fb,Fc=NULL,R.Forward = R.Forward,dimA = dimA,dimB = dimB)

}
