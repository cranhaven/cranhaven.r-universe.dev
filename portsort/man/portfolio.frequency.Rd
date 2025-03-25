\name{portfolio.frequency}
\alias{portfolio.frequency}

\title{
Calculate Sub-Portfolio Concentration
}
\description{
Computes the frequency that an asset appears in each sub-portfolio based on its rank.
}
\usage{
portfolio.frequency(sort.output, rank)
}

\arguments{
  \item{sort.output}{
object returned from either the conditional.sort or unconditional.sort function. 
}
  \item{rank}{
input the rank of the security you would like to return the frequency for.
}
}
\details{
Returns the frequency that the security appears in each sub-portfolio based on the rank input.
}

\author{
Alexander Dickerson and Jonathan Spohnholtz
}
\examples{
# Load the included data
library(portsort)
data(Factors)

# Specifiy the sort dimension - in this case, a double-sort on lagged returns and Bitcoin volumes
dimA = 0:3/3
dimB = 0:3/3

# Specify the factors
# Lagged returns, lagged volumes are stored in the Factors list
R.Forward = Factors[[1]]; R.Lag = Factors[[2]]; V.Lag = Factors[[3]]

# Subset the data from late 2017
R.Forward = R.Forward["2017-12-01/"]
R.Lag = R.Lag["2017-11-30/2018-09-05"]
V.Lag = V.Lag["2017-11-30/2018-09-05"]

Fa = R.Lag
Fb = V.Lag



# Conduct an unconditional sort (in this case) or a conditional sort
sort.output = unconditional.sort(Fa = Fa, Fb = Fb , R.Forward = R.Forward, dimA = dimA, dimB = dimB)

# We want to see which security appeared the most in each sub-portfolio, 
# i.e the secruity with a rank of 1.

rank = 1
portfolio.frequency(sort.output,rank)
}