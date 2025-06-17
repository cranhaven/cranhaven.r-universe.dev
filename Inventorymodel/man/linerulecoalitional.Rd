\name{linerulecoalitional}
\alias{linerulecoalitional}
\title{
Line rule (coalitional)
}
\description{
Line rule for an inventory transportation system for each possible coalition.
}
\usage{
linerulecoalitional(n = NA, a = NA, av = NA, d = NA, h = NA, m = NA)
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
Matrix with allocations proposed by Line rule to agents (columns) in each possible coalition \emph{S} (row).
}

\examples{
linerulecoalitional(n=3,a=200,av=c(300,300,900),d=c(90,80,20),
h=c(0.06,0.06,0.1),m=NA)

#    Coalition        1        2       3 Coalitional cost
#1           0  0.00000  0.00000  0.0000          0.00000
#2     '{ 1 }' 73.48469  0.00000  0.0000         73.48469
#3     '{ 2 }'  0.00000 69.28203  0.0000         69.28203
#4     '{ 3 }'  0.00000  0.00000 66.3325         66.33250
#5   '{ 1,2 }' 52.59885 48.39619  0.0000        100.99505
#6   '{ 1,3 }' 61.26061  0.00000 66.3325        127.59310
#7   '{ 2,3 }'  0.00000 55.97858 66.3325        122.31108
#8 '{ 1,2,3 }' 51.38935 46.10733 66.3325        163.82918
}
