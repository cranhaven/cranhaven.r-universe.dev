\name{linerule}
\alias{linerule}
\title{
Line rule
}
\description{
Line rule for an inventory transportation system when coalition \emph{N} is formed.
}
\usage{
linerule(n=NA,a=NA,av=NA,d=NA,h=NA,m=NA)
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
Allocation for each agent if \emph{N} was formed.
}
\examples{
linerule(n=3,a=200,av=c(300,300,900),d=c(90,80,20),h=c(0.06,0.06,0.1),m=NA)

#Line rule
#[1] 51.38935 46.10733 66.33250
}
