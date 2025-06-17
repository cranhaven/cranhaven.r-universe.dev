\name{EPQcoo}
\alias{EPQcoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
EPQcoo}
\description{
This function obtains the optimal orders and the associated cost when agents are cooperating in the EPQ model.}
\usage{
EPQcoo(n = NA, a = NA, d = NA, h = NA, m = NA, r = NA, b = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of agents in the inventory model.
}
  \item{a}{
The fixed cost per order.
}
  \item{d}{
Vector. Deterministic demands per time unit to each agent.
}
  \item{h}{
Vector. Holding costs to each agent.
}
  \item{m}{
Vector. Number of orders to each agent (optional).
}
  \item{r}{
Vector. Replacement rate to each agent. In general, r>d.
}
  \item{b}{
Vector. Cost of a shortage to each agent.
}
}

\value{
A list with the following components:
\itemize{
\item {\code{Optimal order} If \emph{n} is lesser than \emph{0}, a matrix with all possible coalitions in the first column. The next \emph{n} columns contain the associated cost to each agent in the coalition. Last column indicates the global cost of the optimal order.  Otherwise, this matriz contains the individual costs and the associated values for \emph{N}.}
\item {\code{Optimal shortages} A matrix, for each coalition (row), contains in the column \emph{i} the allowed optimal shortages.}
}
}
\examples{
EPQcoo(n=3,a=600,d=c(500,300,400),h=c(9.6,11,10),r=rep(600,3),b=c(100,150,200))

#EPQ model
#Cooperative case
#$`Optimal order
#        1        2        3     Costs
#   0.0000   0.0000   0.0000    0.0000
# 641.0928   0.0000   0.0000  935.9019
#   0.0000 265.0557   0.0000 1358.2049
#   0.0000   0.0000 388.8444 1234.4268
# 363.7611 218.2567   0.0000 1649.4341
# 387.3208   0.0000 309.8566 1549.1036
#   0.0000 196.1473 261.5297 1835.3556
# 291.2332 174.7399 232.9866 2060.2045
#
#$`Optimal shortages`
#        1        2        3
# 0.000000 0.000000 0.000000
# 9.359019 0.000000 0.000000
# 0.000000 9.054699 0.000000
# 0.000000 0.000000 6.172134
# 5.310381 7.455973 0.000000
# 5.654318 0.000000 4.918359
# 0.000000 6.700683 4.151265
# 4.251580 5.969377 3.698200
}
