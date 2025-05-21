\name{predict.spm}
\alias{predict.spm}
\title{Semiparametric regression prediction.}
\description{Takes a fitted \code{spm} object produced by 
\code{spm()} and obtains predictions at new data values.}
\usage{
\method{predict}{spm}(object,newdata,se,...)
}
\arguments{ 
  
\item{object}{a fitted \code{spm} object as produced by \code{spm()}.}

\item{newdata}{a data frame containing the values of the predictors at
               which predictions are required. The columns should
               have the same name as the predictors.}

\item{se}{when this is TRUE standard error estimates are
          returned for each prediction. The default is FALSE.}

\item{...}{other arguments.}

}
\details{Takes a fitted \code{spm} object produced by 
\code{spm()} and obtains predictions at new data values
as specified by the `newdata' argument. If `se=TRUE' then
standard error estimates are also obtained.}

\value{If se=FALSE then a vector of predictions at `newdata' is returned.
       If se=TRUE then a list with components named `fit' and `se' is
       returned. The `fit' component contains the predictions. 
       The `se' component contains standard error estimates.}

\references{
     Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}


     Ganguli, B. and Wand, M.P. (2005)\cr
     \emph{SemiPar 1.0 Users' Manual}.\cr
     \url{http://matt-wand.utsacademics.info/SPmanu.pdf}
}
\author{M.P. Wand 
\email{mwand@uow.edu.au}
(other contributors listed in SemiPar Users' Manual).}

\seealso{\code{\link{spm}}
\code{\link{lines.spm}}
\code{\link{plot.spm}}
\code{\link{summary.spm}}
}


\examples{
library(SemiPar)
data(fossil)
attach(fossil)
fit <- spm(strontium.ratio~f(age))
newdata.age <- data.frame(age=c(90,100,110,120,130))
preds <-  predict(fit,newdata=newdata.age,se=TRUE)
print(preds)

plot(fit,xlim=c(90,130))
points(unlist(newdata.age),preds$fit,col="red")
points(unlist(newdata.age),preds$fit+2*preds$se,col="blue")
points(unlist(newdata.age),preds$fit-2*preds$se,col="green")
}
\keyword{models} \keyword{smooth} \keyword{regression} 


