\name{inventorygames}
\alias{inventorygames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inventorygames}
\description{
Generic function to show the associated cost game to a EOQ or EPQ model.}
\usage{
inventorygames(n = NA, a = NA, d = NA, h = NA, m = NA, r = NA, b = NA, 
model = c("EOQ", "EPQ"))
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
Vector. Replacement rate to each agent. In general, r>d.}

  \item{b}{
Vector. Cost of a shortage to each agent.
}
  \item{model}{
Model to be selected. \code{EOQ} and \code{EPQ}  models can be considered.
}
}

\value{
The characteristic function of the associated cost game is calculated to model \code{EOQ} or \code{EPQ}.
}

\examples{
inventorygames(n=3,a=600,d=c(500,300,400),h=c(9.6,11,10),model="EOQ")
#EOQ model
#Cooperative case
#Optimal order
#    Coalition        1        2        3 Coalitional costs
#1           0   0.0000   0.0000   0.0000             0.000
#2     '{ 1 }' 250.0000   0.0000   0.0000          2400.000
#3     '{ 2 }'   0.0000 180.9068   0.0000          1989.975
#4     '{ 3 }'   0.0000   0.0000 219.0890          2190.890
#5   '{ 1,2 }' 192.4501 115.4701   0.0000          3117.691
#6   '{ 1,3 }' 184.6372   0.0000 147.7098          3249.615
#7   '{ 2,3 }'   0.0000 121.6327 162.1770          2959.730
#8 '{ 1,2,3 }' 157.4592  94.4755 125.9673          3810.512
}
