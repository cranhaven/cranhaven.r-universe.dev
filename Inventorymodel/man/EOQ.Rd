\name{EOQ}
\alias{EOQ}
\title{EOQ}
\description{
This function obtains the optimal orders and the associated cost in the EOQ model.}
\usage{
EOQ(n = NA, a = NA, d = NA, h = NA, m = NA)
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
}

\value{
This function calculates two vectors. The first shows the optimal order for each agent. The second vector indicates the associated cost to these orders.
}

\examples{
EOQ(n=3,a=600,d=c(500,300,400),h=c(9.6,11,10))

#EOQ model
#$"Optimal order"
#[1] 250.0000 180.9068 219.0890
#
#$"Order costs"
#[1] 2400.000 1989.975 2190.890
}
