\name{hyptest}
\alias{hyptest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test of a linear hypothesis in a linear model
}
\description{
Carries out test of a single linear hypothesis in a linear model.
}
\usage{
hyptest(lmobj, p, xi = 0, type = "both")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lmobj}{
An object produced by lm fitting.
}
  \item{p}{
A numeric vector containing coefficients of the linear combination of model parameters.
}
  \item{xi}{
A numeric variable containing hypothesized value of the linear combination of model parameters (default = 0).
}
  \item{type}{
A character variable indicating the type of alternative: "upper" (one-sided), "lower" (one-sided) or "both" (default, two-sided).
}
}

\value{
Returns the estimated value of the linear combination of model parameters, its standard error, the t-statistic, the degrees of freedom and the p-value.
}
\details{
It is assumed that all the model parameters are estimable and the linear model is homoscedastic and normal.
}
\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}

\examples{
data(lifelength)
lmlife <- lm(Lifelength~factor(Category), data = lifelength)
p <- c(0,0,0,1,-1,0,0,0)
hyptest(lmlife, p, xi = 1, type = "upper")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ regression }
