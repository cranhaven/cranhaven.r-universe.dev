\name{Gini}

\alias{Gini}

\title{Compute the simple Gini coefficient}

\description{

Computes the simple Gini coefficient of unequality

}

\usage{

Gini(x)

}

\arguments{

\item{x}{a numeric vector with non-negative elements}

}

\details{

Gini coefficient is a common measure of inequality. Here it presents only
for the convenience to have this calculation "outside" of social science
R packages (where it commonly presents). Please read elsewhere of its
meaning and uses.

Code is based on the 'reldist' package from Mark S. Handcock but
simplified to revome the using of weights (as a sideway result, it should
be slightly faster).

}

\value{

The Gini coefficient (number between 0 and 1).

}

\references{

\emph{Relative Distribution Methods in the Social Sciences}, by Mark S.
Handcock and Martina Morris, Springer-Verlag, Inc., New York, 1999. ISBN
0387987789.

}

\author{Alexey Shipunov}

% \seealso{}

\examples{

salary <- c(21, 19, 27, 11, 102, 25, 21)
Gini(salary)

new.1000 <- sample((median(salary) - IQR(salary)) :
(median(salary) + IQR(salary)), 1000, replace=TRUE)
salary2 <- c(salary, new.1000)
Gini(salary2)

salary3 <- salary[-which.max(salary)]
salary3
Gini(salary3)

salary4 <- c(salary3, 1010)
salary4
Gini(salary4)

}

\keyword{univar}
