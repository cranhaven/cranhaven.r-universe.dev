\name{STI}
\alias{STI}
\title{
STI}
\description{
This function obtains the optimal orders and the associated cost when agents are cooperating in the inventory transportation system.
}
\usage{
STI(n = NA, a = NA, av = NA, d = NA, h = NA, m = NA)
}
\arguments{
  \item{n}{
 Agents in the inventory situation.
}
  \item{a}{
The fixed cost per order.
}
  \item{av}{
Vector. The transportations cost per order to each agent.
}
  \item{d}{
Vector. Deterministic demands per time unit to each agent.
}
  \item{h}{
Vector. Holding cost per time unit to each agent.
}
  \item{m}{
Vector. Number of orders to each agent (optional).
}
}

\value{
This function calculates two vectors. The first one shows the optimal order for each agent. The second vector indicates the associated cost to these orders.
}
\examples{
STI(n=3,a=200,av=c(300,300,900),d=c(90,80,20),h=c(0.06,0.06,0.1),m=NA)
#STI model
#$"Optimal order"
#[1] 1224.745 1154.701  663.325
#
#$"Order cost"
#[1] 73.48469 69.28203 66.33250
}
