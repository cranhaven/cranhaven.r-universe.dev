\name{Bregbf}
\alias{Bregbf}
\title{Bayesian regression model comparison with Bayes factors.
}
\description{
  This function compares different linear models on the basis of their
  Bayes factors and by graphically comparing posterior model probabilities. 
}
\usage{
Bregbf(form.list, data, l=length(form.list))
}
\arguments{
  \item{form.list}{
a list of linear models, each expressed by a model formula, that should
be compared; the models must all be applicable to the same data frame
and use the same response variable
}
  \item{data}{
a data frame to be analyzed
}
  \item{l}{
the number of models to be compared; defaults to all models in the form.list
}
}
\details{
  Note that a list containing several appropriate models for the data
  frame should be prepared beforehand. See the example for how to do this.
}
\value{
A list with model parameter probabilities is silently returned.
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\examples{
\dontrun{
data(PlantGrowth)
frmlst <- list(
model0 = formula(weight ~ 1),
model1 = formula(weight ~ group) )
Bregbf(form.list=frmlst, data=PlantGrowth)
data(fev)
frmlst.fev <- list(
formula(FEV ~ Age),
formula(FEV ~ Smoke),
formula(FEV ~ Age + Smoke),
formula(FEV ~ Age * Smoke)
)
Bregbf(frmlst.fev, fev)
}
}
\keyword{models}
\keyword{htest}

