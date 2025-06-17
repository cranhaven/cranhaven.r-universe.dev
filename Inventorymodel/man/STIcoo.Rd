\name{STIcoo}
\alias{STIcoo}
\title{STIcoo}
\description{
This function obtains the optimal orders and the associated cost when agents are cooperating in the inventory transportation system when agents are cooperating.
}
\usage{
STIcoo(n = NA, a = NA, av = NA, d = NA, h = NA, m = NA)
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
A list with the following components:
\itemize{

\item{\code{Optimal order} If \emph{n} is lesser than \emph{0}, a matrix with all possible coalitions in the first column. The next \emph{n} columns contain the associated cost to each agent in the coalition. Last column indicates the global cost of the optimal order. Otherwise, this matriz contains the individual costs and the associated values for \emph{N}.} 
}
}
\examples{
STIcoo(n=3,a=200,av=c(300,300,900),d=c(90,80,20),h=c(0.06,0.06,0.1),m=NA)

#$`Optimal order`
#          1        2        3   Coalition Order cost
#1    0.0000    0.000   0.0000           0    0.00000
#2 1224.7449    0.000   0.0000     '{ 1 }'   73.48469
#3    0.0000 1154.701   0.0000     '{ 2 }'   69.28203
#4    0.0000    0.000 663.3250     '{ 3 }'   66.33250
#5  891.1328  792.118   0.0000   '{ 1,2 }'  100.99505
#6 1551.8080    0.000 344.8462   '{ 1,3 }'  127.59310
#7    0.0000 1438.954 359.7385   '{ 2,3 }'  122.31108
#8 1208.5759 1074.290 268.5724 '{ 1,2,3 }'  163.82918
}