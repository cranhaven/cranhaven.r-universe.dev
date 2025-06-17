\name{EPQ}
\alias{EPQ}
\title{EPQ}
\description{
This function obtains the optimal orders and the associated cost in the EPQ model.
}
\usage{
EPQ(n = NA, a = NA, d = NA, h = NA, m = NA, r = NA, b = NA)
}
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
}

\value{
This function calculates two vectors. The first one shows the optimal order for each agent. The second vector indicates the associated cost to these orders.
}
\examples{
EPQ(n=3,a=600,d=c(500,300,400),h=c(9.6,11,10),m=NA,r=rep(600,3),b=c(100,150,200))
#EPQ model
#$"Optimal order"
#[1] 641.0928 265.0557 388.8444
#
#$"Optimal shortages"
#[1] 9.359019 9.054699 6.172134
#
#$"Order costs"
#[1]  935.9019 1358.2049 1234.4268
}
