\name{binaries}
\alias{binaries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert categorical variable to several binary variables
}
\description{
Stacks up in columns the values of all the binary variables that can be associated with different levels of a categorical variable.
}
\usage{
binaries(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A categorical variable (either numeric or character).
}
}
\value{
A set of binary vectors, each having the value 1 for a unique level of x.
}
\details{
The name of each new variable is of the type v.x, where x is the level of the categorical variable for which this binary variable is equal to 1. 
}
\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}

\examples{
x <- c(1,2,2,3,1,1,2,3,3,2,1)
binaries(x)
binaries(as.factor(x))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ category }
