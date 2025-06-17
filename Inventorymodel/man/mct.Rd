\name{mct}
\alias{mct}
\title{
MCT}
\description{
This function obtains the associated costs in a basic EOQ system without holding costs and with transportation cost.
}
\usage{
mct(n = NA, a = NA, av = NA, d = NA, K = NA, cooperation = c(0, 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{n}{
 Agents in the inventory situation.
}
  \item{a}{
The fixed cost per order.
}
  \item{av}{
Vector. The transportations cost per order to each agent.}
  \item{d}{
Vector. Deterministic demands per time unit to each agent.
}
  \item{K}{
Vector. Warehouse's capacity to each agent.
}
  \item{cooperation}{
Option to indicate cooperation. If it exists \code{cooperation=1} else \code{cooperation=0}.}
}
\value{
If \code{cooperation=0}, a vector with the individual cost to each agent in a \code{MCT}. If \code{cooperation=1} and \emph{n} is lesser than \emph{0}, a matrix which contains the associated costs for each possible group. Otherwise, this matrix only contains the individual costs and the associated values for \emph{N}.
}
\examples{
mct(n=3,a=400,av=c(300,500,200),d=c(2,2,5),K=c(9,8,7),cooperation=0)

#MCT model
#Individual cost
#[1] 155.5556 225.0000 428.5714
}