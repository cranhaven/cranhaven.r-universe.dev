\name{hanova}
\alias{hanova}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ANOVA table for adequacy of a subset in a linear model)
}
\description{
Prepares the Analysis of Variance table for testing adequacy of a subset model within a linear model.
}
\usage{
hanova(lm1, lm2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lm1}{
An lm object describing full model.
}
  \item{lm2}{
An lm object describing subset model.
}
}

\details{
Normal distribution of response (given explanatory variables and/or factors) is assumed. The program simply reformats the
output of the \code{\link[stats]{anova}} function.
}

\value{
Returns analysis of variance table for testing adequacy of lm2 within lm1.
}
\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}

\examples{
data(birthwt)
lmbw <- lm(bwt ~ smoke+factor(race), data = birthwt)
lm1 <- lm(bwt ~ smoke, data = birthwt)
hanova(lm1,lmbw)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ model }
\keyword{ design }
\keyword{ regression }
