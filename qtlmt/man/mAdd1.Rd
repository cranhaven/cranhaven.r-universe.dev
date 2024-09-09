\name{mAdd1}
\alias{mAdd1}
\alias{mDrop1}
\title{
Add or drop all possible terms
}
\description{
Add or drop all possible terms to or from a multivariate multiple regression model.
}
\usage{
mAdd1(object, scope, test = c("none","Chisq","F"), k = 0, ...)

mDrop1(object, scope, test = c("none","Chisq","F"), k = 0, ...)
}
\arguments{
  \item{object}{
initial model.
}
  \item{scope}{
a formula or a vector of variable names, giving lower/upper bound of the model.
}
  \item{test}{
if not "none", the test statistic and p-value will be included in the output.
}
\item{k}{
penalty on a parameter in AIC.
}
  \item{...}{
additional arguments to \code{\link{update}}.
}
}

\value{
An object summarizing the differences in fit between the models.
}

\seealso{
\code{\link{add1}} and \code{\link{mStep}}
}

\examples{
data(etrait)
mdf<- data.frame(traits,markers)
\dontrun{
mlm<- lm(cbind(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16) ~
   m1 + m2 + m3 + m4 + m5, data=mdf)

up<- formula(paste("~", paste("m",1:15,collapse=" + ",sep="")))
oa<- mAdd1(mlm, scope=up, test="F", k=5, data=mdf)

lw<- formula(paste("~ ", paste("m",1:3,collapse=" + ",sep="")))
od<- mDrop1(mlm, scope=lw, test="F", k=5, data=mdf)
}
}

