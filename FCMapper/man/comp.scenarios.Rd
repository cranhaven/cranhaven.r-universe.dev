\name{comp.scenarios}
\alias{comp.scenarios}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scenario comparison
}
\description{
The equilibrium values of two scenarios are compared.
}
\usage{
comp.scenarios(scenario1, scenario2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scenario1}{
Dataframe output from nochanges.scenario or changes.scenario.
}
  \item{scenario2}{
Dataframe output from nochanges.scenario or changes.scenario.
}
}

\value{
The difference between the equilibrium values of the two scenarios are given in a data frame along with the concept names, the scenario 1 values, the scenario 2 values, and the percent difference. 
}

\author{
Shaun Turney
}
\note{
The scenarios must be scenarios of fuzzy cognitive maps with the same concepts, though not neccesarily the same edges. 
}


\seealso{
\code{\link{changes.scenario}}
\code{\link{nochanges.scenario}}
}
\examples{
matrix = matrix(nrow=7,ncol=7)
matrix[1,] = c(0,-0.5,0,0,1,0,1)
matrix[2,] = c(1,0,1,0.2,0,0,0.6)
matrix[3,] = c(0,1,0,0,0,0,0)
matrix[4,] = c(0.6,0,0,1,0,0,0.1)
matrix[5,] = c(0,0.5,0,0,1,0,-0.6) 
matrix[6,] = c(0,0,-1,0,0,0,0)
matrix[7,] = c(0,0,0,-0.5,0,0,1)
concept.names = c("A","B","C","D","E","F","G")

scenario1 = nochanges.scenario(matrix,iter=25,concept.names)

scenario2 = changes.scenario(matrix,concept.names,iter=25,set.concepts=c("B"),set.values=c(0.5))

comp.scenarios(scenario1,scenario2)
}

