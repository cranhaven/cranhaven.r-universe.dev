\name{EOQcoo}
\alias{EOQcoo}
\title{
EOQcoo}
\description{
This function obtains the optimal orders and the associated cost when agents are cooperating in the EOQ model.
}
\usage{
EOQcoo(n = NA, a = NA, d = NA, h = NA, m = NA)
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
A list with the following components:
\itemize{
\item {\code{Optimal order} If \emph{n} is lesser than \emph{0}, a matrix with all possible coalitions in the first column. The next \emph{n} columns contain the associated cost to each agent in the coalition. Last column indicates the global cost of the optimal order. Otherwise, this matriz contains the individual costs and the associated values for \emph{N}.
}}
}
\examples{
EOQcoo(n=3,a=600,d=c(500,300,400),h=c(9.6,11,10))

#EOQ model
#Cooperative case
#Optimal order
#    Coalition        1        2        3 Coalitional costs
#1           0   0.0000   0.0000   0.0000             0.000
#2     '{ 1 }' 250.0000   0.0000   0.0000          2400.000
#3     '{ 2 }'   0.0000 180.9068   0.0000          1989.975
#4     '{ 3 }'   0.0000   0.0000 219.0890          2190.890
#5   '{ 1,2 }' 192.4501 115.4701   0.0000          3117.691
#6   '{ 1,3 }' 184.6372   0.0000 147.7098          3249.615
#7   '{ 2,3 }'   0.0000 121.6327 162.1770          2959.730
#8 '{ 1,2,3 }' 157.4592  94.4755 125.9673          3810.512
}
