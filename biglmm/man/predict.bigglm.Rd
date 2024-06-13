\name{predict.bigglm}
\alias{predict.bigglm}
\alias{predict.biglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Predictions from a biglm/bigglm }
\description{
Computes fitted means and standard errors at new data values after
fitting a model with \code{biglm} or \code{bigglm}.
}
\usage{
\method{predict}{bigglm}(object, newdata, type = c("link", "response"), 
se.fit = FALSE, make.function = FALSE, ...)
\method{predict}{biglm}(object, newdata=NULL,  se.fit = FALSE, make.function = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{fitted model }
  \item{newdata}{data frame with variables for new values }
  \item{type}{\code{link} is on the linear predictor scale,
    \code{response} is the response }
  \item{se.fit}{Compute standard errors?}
  \item{make.function}{If \code{TRUE} return a prediction function, see
    Details below}
  \item{\dots}{not used }
}
\details{
  When \code{make.function} is \code{TRUE}, the return value is either a
  single function that computes the fitted values or a list of two
  functions that compute the fitted values and standard errors.  The
  input to these functions is the design matrix, without the intercept
  column.  This allows the relatively time-consuming calls to
  \code{model.frame()} and \code{model.matrix()} to be avoided.
  
}
\value{
Either a vector of predicted values or a data frame with predicted
values and standard errors.
}
\references{ ~put references to the literature/web site here ~ }
\author{based on code by Christophe Dutang }

\seealso{\code{\link{predict.glm}},\code{\link{biglm}},\code{\link{bigglm}} }
\examples{
example(biglm)
predict(a,newdata=trees)
f<-predict(a,make.function=TRUE)
X<- with(trees, cbind(log(Girth),log(Height)))
f(X)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{regression}

