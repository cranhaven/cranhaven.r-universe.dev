\name{inventorymodelfunction}
\alias{inventorymodelfunction}
\title{
inventorymodelfunction}
\description{
General function in Inventorymodel package for obtaining the optimal policy by selecting any model and its corresponding parameters.}
\usage{
inventorymodelfunction(model=c("EOQ","EPQ","STI","FOC","MCT","MWHC","MWHC2","MWHCCT"),
n=NA,a=NA,av=NA,d=NA,h=NA,m=NA,r=NA,K=NA,b=NA,c1=NA,c2=NA,
cooperation=c(0,1),allocation=c(0,1))
}
\arguments{
  \item{model}{
Model to be selected. \code{EOQ}, \code{EPQ}, \code{STI}, \code{mfoc}, \code{mct}, \code{mwhc}, \code{mwhc2c} or \code{mwhcct}  models can be considered.
}
  \item{n}{
Number of agents in the inventory model.
}
  \item{a}{
The fixed cost per order.
}
  \item{av}{
Vector. Transportation costs to each agent.}
  \item{d}{
Vector. Deterministic demands per time unit to each agent.
}
  \item{h}{
Vector. Holding costs to each agent.
}
  \item{m}{
Vector. Number of orders to each agent(optional).
}
  \item{r}{
Vector. Replacement rate to each agent. In general, r>d.
}
\item{K}{
Vector. Warehouse capacity to each agent.}
  \item{b}{
Vector. Shortage cost to each agent.}
  \item{c1}{
Value. Cost of a product from the first supplier.}
  \item{c2}{
Value. Cost of a product from the second supplier.}
  \item{cooperation}{
Option to indicate cooperation. If it exists \code{cooperation=1} else \code{cooperation=0}.}
  \item{allocation}{
Option to indicate the allocation. If it is required \code{allocation=1} else \code{allocation=0}.}
}


\seealso{
\code{\link{EOQ}}, \code{\link{EOQcoo}}, \code{\link{EPQ}}, \code{\link{EPQcoo},} \code{\link{STI}}, \code{\link{STIcoo}}, \code{\link{mct}}, \code{\link{mfoc}}, \code{\link{mwhc}}, \code{\link{mwhc2c}} and \code{\link{mwhcct}}.
}

\examples{
inventorymodelfunction(model="MCT",n=3,a=400,av=c(300,500,200),d=c(2,2,5),K=c(9,8,7),
cooperation=1,allocation=1)
#MCT model
#Cooperative case
#Two-lines rule
#$`Optimal solution`
#  1 2 3 Coalition     Cost
#  0 0 0         0   0.0000
#  1 0 0         1 155.5556
#  0 1 0         2 225.0000
#  0 0 1         3 428.5714
#  1 1 0        12 225.0000
#  1 0 1        13 500.0000
#  0 1 1        23 642.8571
#  1 1 1       123 642.8571
#
#$`Allocation two-lines rule`
#[1]   0.0000 219.6429 423.2143
}
