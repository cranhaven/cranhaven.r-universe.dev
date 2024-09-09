\name{sureEps}
\alias{sureEps}
\title{
Epistasis in a SURE model
}
\description{
Look for epistasis between existing QTL in a model via model selection.
}
\usage{
sureEps(y, x, v, k, direction=c("both","backward","forward"),
   iter=10000, max.terms=200, steps=1000, tol=1e-12)
}
\arguments{
  \item{y}{
a n by p matrix, whose columns are dependent variables.
}
  \item{x}{
a n by m matrix, whose columns are predictor variables
}
  \item{v}{
a list; v[[j]] indicates which x's in the model for y[,j], between which possible epistasis is looked for.
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
}

\value{
associated traits, epistatic markers and p-values
}

\seealso{
\code{\link{surStep}}
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
o<- surStep(y, x, v=v, upper=upper, k=19, direction="both",
   iter=250, max.terms=250, steps=2000, tol=1e-12)
neps<- 6 # suppose there are 6 possible epistatic effects
oeps<- sureEps(y, x, o$v, k=qchisq(1-0.05/neps,1), direction="backward",
  iter=250, max.terms=200, steps=1000, tol=1e-12)
}
}

