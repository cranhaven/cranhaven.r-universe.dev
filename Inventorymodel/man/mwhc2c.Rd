\name{mwhc2c}
\alias{mwhc2c}
\title{
MWHC2C}
\description{
This function obtains the associated costs in a model without holding costs and with two differents cost of product. Demands 
and capacities must be introduced in the order indicated by the ratios \code{d/K}. In other case, 
agents change their position.
}
\usage{
mwhc2c(n=NA,a=NA,b=NA,d=NA,K=NA,c1=NA,c2=NA,cooperation=c(0,1),allocation=c(0,1))
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

  \item{c1}{
Value. Cost per unit of product from the first vendor.
}
  \item{c2}{
Value. Cost per unit of product from the second vendor.
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
\item {\code{"Optimal policies"} If \emph{n} is lesser than \emph{0}, a matrix with all possible coalitions in the first column. The second column contains the optimal order to each coalition. Last column indicates the global cost of this optimal order. Otherwise, this matriz contains the individual costs and the associated values for \emph{N}.}
\item {\code{"GR-rule"} A matrix, for each coalition (row), contains the coalition  \emph{i(S)} and allocations proposed by GR-rule.}}
}
\examples{
mwhc2c(n=3,a=1,b=c(10,10,10),d=c(30,45,46),K=c(30,45,46),c1=3.5,c2=3,cooperation=1,allocation=1)
#MWHC model
#$`Optimal policies`
#   Coalitions Optimal orders    Costs
#1           0      0.0000000   0.0000
#2     '{ 1 }'      0.9505864 105.5947
#3     '{ 2 }'      0.9515422 157.9165
#4     '{ 3 }'      0.9515838 161.4046
#5   '{ 1,2 }'      0.9523090 262.5595
#6   '{ 1,3 }'      0.9523241 266.0476
#7   '{ 2,3 }'      0.9525115 318.3690
#8 '{ 1,2,3 }'      0.9527470 423.0118
#
#$`GR-rule`
#  Coalition_SxT        1        2        3
#              0   0.0000   0.0000   0.0000
#              1 105.5947   0.0000   0.0000
#              2   0.0000 157.9165   0.0000
#              3   0.0000   0.0000 161.4046
#             12 105.0238 157.5357   0.0000
#             13 105.0188   0.0000 161.0288
#             23   0.0000 157.4352 160.9338
#            123 104.8790 157.3184 160.8144
}
