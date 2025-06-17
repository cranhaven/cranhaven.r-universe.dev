\name{SOC}
\alias{SOC}
\title{
SOC}
\description{
Generic function for showing the allocations proposed by SOC rule under an EOQ or EPQ model.}
\usage{
SOC(n = NA, a = NA, d = NA, h = NA, m = NA, r = NA, b = NA, 
model = c("EOQ", "EPQ"))
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
Vector. Number of orders to each agent(optional).
}
  \item{r}{
Vector. Replacement rate to each agent. In general, r>d.}

  \item{b}{
Vector. Cost of a shortage to each agent.
}
  \item{model}{
Model to select. \code{EOQ} and \code{EPQ}  models can be considered.
}
}

\value{
Matrix with number of rows equal to the number of coalitions and \emph{n} columns. For each coalition or row, the output shows the cost that SOC rule allocates to each player or column.}
\examples{
SOC(n=3,a=600,d=c(500,300,400),h=c(9.6,11,10),m=NA,r=NA,b=NA,model="EOQ")

#EOQ model
#$`Share the ordering costs rule (individually)`
#[1] 2400.000 1989.975 2190.890

SOC(n=3,a=600,d=c(500,300,400),h=c(9.6,11,10),m=NA,r=NA,b=NA,model="EOQ")

#EOQ model
#$`Share the ordering costs rule (individually)`
#         1        2        3
#     0.000    0.000    0.000
#  2400.000    0.000    0.000
#     0.000 1989.975    0.000
#     0.000    0.000 2190.890
#  1847.521 1270.171    0.000
#  1772.517    0.000 1477.098
#     0.000 1337.960 1621.770
#  1511.608 1039.230 1259.673
}