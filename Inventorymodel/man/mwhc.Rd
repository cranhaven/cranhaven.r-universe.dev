\name{mwhc}
\alias{mwhc}
\title{
MWHC}
\description{
This function obtains the associated costs in a model without holding costs. Demands 
and capacities must be introduced in the order indicated by the ratios \code{d/K}. In other case, 
agents change their position.
}
\usage{
mwhc(n = NA, a = NA, b = NA, d = NA, K = NA, cooperation = c(0, 1), 
allocation = c(0, 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{n}{
 Agents in the inventory situation.
}
  \item{a}{
The fixed cost per order.
}
  \item{b}{
Vector. Shortage cost per unit to each agent.
}
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
A list with the following components:
\itemize{

\item{ \code{"Optimal policies"} If \emph{n} is lesser than \emph{0}, a matrix with all possible coalitions in the first column. The second column contains the optimal order to each coalition. Last column indicates the global cost of this optimal order. Otherwise, this matriz contains the individual costs and the associated values for \emph{N}.}
\item{ \code{"R-rule"} A matrix, for each coalition (row), contains the coalition  \emph{i(S)} and allocations proposed by R-rule.}
}}



\examples{
mwhc(n=4,a=180,b=c(15,15,10,12),d=c(0.45,0.95,1.05,1.2),K=c(5,7.5,8,9),
cooperation=1,allocation=1)

#MWHC model
#Cooperative case
#$`Optimal policies`
#      Coalitions Optimal orders    Costs
#1              0     0.00000000  0.00000
#2        '{ 1 }'     0.07520921 14.74965
#3        '{ 2 }'     0.10684954 20.86510
#4        '{ 3 }'     0.10406757 20.89599
#5        '{ 4 }'     0.11094004 21.79985
#6      '{ 1,2 }'     0.10684954 20.86510
#          ..   
#
#$`R-rule`
#  Coalition_SxT        1         2         3        4
#              0  0.00000  0.000000  0.000000  0.00000
#              1 14.74965  0.000000  0.000000  0.00000
#              2  0.00000 20.865100  0.000000  0.00000
#              3  0.00000  0.000000 20.895986  0.00000
#              4  0.00000  0.000000  0.000000 21.79985
#              2  0.00000 20.865100  0.000000  0.00000
#              3  0.00000  0.000000 20.895986  0.00000
#             ..  
}
