\name{bc.model}
\alias{bc.model}
\title{One Dimensional Box-Cox Model}
\usage{
bc.model(mod, data = list(), range = seq(-2, 2, 0.1), details = FALSE)
}
\arguments{
\item{mod}{estimated linear model object or formula.}

\item{data}{if \code{mod} is a formula then the corresponding data frame has to be specified.}

\item{range}{range and step size of lambda values. Default is a range from -2 to 2 at a step size of 0.1.}

\item{details}{logical value indicating whether specific details about the test should be returned.}
}
\value{
A list object including:
\tabular{ll}{
\code{results} \tab regression results with minimal SSR.\cr
\code{lambda} \tab optimal lambda-values.\cr
\code{nregs} \tab no. of regressions performed.\cr
\code{idx.opt} \tab index of optimal regression.\cr
\code{val.opt} \tab minimal SSR value.\cr
}
}
\description{
Finds lambda-values for which the one dimensional Box-Cox model has lowest SSR.
}
\examples{
y <- c(4,1,3)
x <- c(1,2,4)
my.mod <- ols(y ~ x)
bc.model(my.mod)

}
\concept{Box-Cox-Model}
\concept{deformed logarithm}
