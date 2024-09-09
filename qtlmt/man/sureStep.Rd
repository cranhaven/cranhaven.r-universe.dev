\name{sureStep}
\alias{sureAdd1}
\alias{sureDrop1}
\alias{sureStep}
\alias{surStep}
\title{
SURE model selection
}
\description{
Select a SURE model via model selection.
}
\usage{
sureAdd1(object, y, x, range=NULL, iter=10000, tol=1e-12, ext=FALSE)

sureDrop1(object, y, x, range=NULL, iter=10000, tol=1e-12, ext=FALSE)

sureStep(object, y, x, cv, direction=c("both","backward","forward"),
   range=NULL, iter=10000, steps=1000, tol=1e-12, ext=FALSE)

surStep(y, x, v, lower, upper, k, direction=c("both","backward",
   "forward"), iter=10000, max.terms=200, steps=1000, tol=1e-12)
}
\arguments{
  \item{object}{
initial model in model search; can be an object of \code{\link{sureEst}}.
}
  \item{y}{
a n by p matrix, whose columns are dependent variables.
}
  \item{x}{
a n by m matrix, whose columns are predictor variables to select from.
}
  \item{range}{
a list; range[[j]] indicates which x's (all if \code{NULL}) correspond to which y[,j].
}
  \item{cv}{
critical value used in the likelihood ratio test to determine adding/dropping a term.
}
  \item{v}{
a list; v[[j]] indicates which x's to start with for y[,j].
}
  \item{lower}{
a list in the format of \code{v}; lower scope of the model.
}
  \item{upper}{
a list in the format of \code{v}; upper scope of the model.
}
  \item{k}{
penalty, 0 if missing or <0.
}
  \item{direction}{
forward selection, backward elimination or stepwise.
}
  \item{iter}{
maximum number of iterations in a numerical process to estimate model parameters.
}
  \item{max.terms}{
maximum number of terms in the final model.
}
  \item{steps}{
maximum number of search steps.
}
  \item{tol}{
convergence tolerance.
}
  \item{ext}{
whether to perform an extensive search for an "optimal" model with the same number of QTL per phenotype.
}
}
\value{
a list with the following components:
  \item{loglik}{log-likelihood of the model}
  \item{b}{estimates of model coefficients}
  \item{sigma}{estimates of residual variance-covariance}
  \item{v}{variables selected in the model}
}

\examples{
data(etrait)
x<- as.matrix(mdat-1/2)
y<- as.matrix(traits)[,1:3]
v<- list()
upper<- list()
for(k in 1:ncol(y)){
   v[[k]]<- numeric(0)
   upper[[k]]<- 1:ncol(x)
}
\dontrun{
o1<- surStep(y, x, v=v, upper=upper, k=19)
o2<- sureStep(o1, y, x, cv=50, ext=FALSE)

# search for optimal model of o1
o3<- sureStep(o1, y, x, cv=Inf, direction="forward", ext=TRUE)
}
}

