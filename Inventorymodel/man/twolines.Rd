\name{twolines}
\alias{twolines}
\title{
Two-lines rule
}
\description{
Two-lines rule for a basic EOQ system without holding costs and with transportation cost.
}
\usage{
twolines(n = NA, a = NA, av = NA, d = NA, K = NA)
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
  \item{K}{
Vector. Capacities of agents' warehouse to each agent.
}
}

\value{
The output is a vector who contains the allocation to each player.
}



\examples{
twolines(n=3,a=400,av=c(300,500,200),d=c(2,2,5),K=c(9,8,7))
#MCT model
#Cooperative case
#Two-lines rule
#[1]   0.0000 219.6429 423.2143
}
