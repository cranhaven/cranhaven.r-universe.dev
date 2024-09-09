\name{mStep}
\alias{mStep}
\title{
Model selection in multivariate multiple regression
}
\description{
Select a multivariate multiple regression model via model selection.
}
\usage{
mStep(object, scope, direction=c("both","backward","forward"),
   trace=FALSE, keep=TRUE, steps=1000, k=2, ...)
}
\arguments{
  \item{object}{
initial model in model search.
}
  \item{scope}{
a single formula, which provides `upper', or a list containing components `upper' and `lower', both formulae; defines the lower and upper bound. See \code{\link{step}}.
}
  \item{direction}{
forward selection, backward elimination or stepwise.
}
  \item{trace}{
whether to track the process for monitoring purpose.
}
  \item{keep}{
whether to return the change of terms and related statistics.
}
  \item{steps}{
maximum number of search steps.
}
\item{k}{
penalty on a parameter. The selection criterion is the known "AIC" if \code{k = 2} and is "BIC" if \code{k = log(n)} where "n" is the sample size.
}
  \item{...}{
additional arguments to \code{\link{update}}.
}
}

\value{
a list with components of a \code{\link{lm}} object plus `keep' if required.
}

\seealso{
\code{\link{mAdd1}} and \code{\link{mDrop1}}
}

\examples{
data(etrait)
mdf<- data.frame(traits,markers)
\dontrun{
mlm<- lm(cbind(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16) ~
   m1 + m2 + m3 + m4 + m5, data=mdf)

lw<- formula(paste("~ ", paste("m",1:3,collapse=" + ",sep="")))
up<- formula(paste("~", paste("m",1:15,collapse=" + ",sep="")))

ob<- mStep(mlm, scope=list(lower=lw), k=99, direction="backward", data=mdf)
of<- mStep(mlm, scope=list(upper=up), k=5, direction="forward", data=mdf)
o1<- mStep(mlm, scope=list(upper=up), k=5, direction="both", data=mdf)
o2<- mStep(o1, scope=list(upper=up), k=2, direction="forward", data=mdf)
}
}

