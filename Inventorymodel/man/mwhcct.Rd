\name{mwhcct}
\alias{mwhcct}
\title{
MWHCCT}
\description{
This function obtains the associated costs in a basic EOQ system without holding costs and with a general transportation cost.
}
\usage{
mwhcct(n = NA, a = NA, av = NA, d = NA, K = NA, cooperation = c(0, 1),allocation = c(0,1))
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
Vector. The transportations cost per order to each possible group of agents.}
  \item{d}{
Vector. Deterministic demands per time unit to each agent.
}
  \item{K}{
Vector. Warehouse's capacity to each agent.
}
  \item{cooperation}{
Option to indicate cooperation. If it exists \code{cooperation=1} else \code{cooperation=0}.}
  \item{allocation}{
Option to indicate the allocation. If it is required \code{allocation=1} else \code{allocation=0}.
}
}
\value{
If \emph{n} is lesser than \emph{0}, a matrix with all possible coalitions in the first column. The next \emph{n} columns contain the associated cost to each agent in the coalition. Last column indicates the global cost of the optimal order. Otherwise, this matriz contains the individual costs and the associated values for \emph{N}.}
\examples{
mwhcct(n=3,a=10,av=c(0,10,10,10,20,20,20,30),d=c(1,0.95,0.9),K=c(1,1,1),
cooperation=1,allocation = 1)
# MWHC with Transportation Costs model
#Cooperative case
#[1] "Shapley Value"
#$`Optimal solution`
#  1 2 3   Coalition Cost
#1 0 0 0           0  0.0
#2 1 0 0     '{ 1 }' 20.0
#3 0 1 0     '{ 2 }' 19.0
#4 0 0 1     '{ 3 }' 18.0
#5 1 1 0   '{ 1,2 }' 30.0
#6 1 0 1   '{ 1,3 }' 30.0
#7 0 1 1   '{ 2,3 }' 28.5
#8 1 1 1 '{ 1,2,3 }' 40.0
#
#$`Allocation R rule`
#      1     2  3
#  13.75 13.25 13
}