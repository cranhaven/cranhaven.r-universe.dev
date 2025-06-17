\name{ivif}
\alias{ivif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Intercept augmented variance inflation factors
}
\description{
Computes the intercept augmented variance inflation factors for a linear model.
}
\usage{
ivif(lmobj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lmobj}{
An object produced by lm fitting.
}
}

\value{
Returns the intercept augmented variance inflation factors for the model, with each VIF labelled either as (Intercept) or by the variable name.
}
\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}

\examples{
data(imf2015)
lmimf <- lm(UNMP~CAB+DEBT+EXP+GDP+INFL+INV, data = imf2015)
ivif(lmimf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ regression }
