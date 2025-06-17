\name{shapley_mfoc}
\alias{shapley_mfoc}
\title{
SMFOC}
\description{
Function to calculate the Shapley value for the associated game to a fixed order cost model.}
\usage{
shapley_mfoc(n = NA, a = NA, d = NA, K = NA)
}
\arguments{
\item{n}{
 Agents in the inventory situation.
}
    \item{a}{
The fixed cost per order.
}
 \item{d}{
Vector. Deterministic demands per time unit to each agent.
}
  \item{K}{
Vector. Warehouse's capacity to each agent.
}
}

\value{
Shapley value for the associated game to the fixed order cost model.
}

\examples{
shapley_mfoc(n=5,a=200,d=c(1.2,1.4,1.2,1.3,0.4),K=c(6,10,8,8,4))

#Shapley-Value
#MFOC model
#[1]  4.000000  6.000000  6.666667  7.916667 15.416667
}
