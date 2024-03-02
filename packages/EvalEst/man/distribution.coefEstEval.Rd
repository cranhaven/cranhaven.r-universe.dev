\name{distribution.coefEstEval}
\alias{distribution.coefEstEval}
\alias{distribution.rootsEstEval}
\title{Plot distribution of estimates}
\description{Plot distribution of estimates.}
\usage{
     \method{distribution}{coefEstEval}(obj, ...,  Sort=FALSE, bandwidth=0.2,
	graphs.per.page=5)
     \method{distribution}{rootsEstEval}(obj, ..., mod=TRUE, invert=FALSE, Sort=FALSE,
        bandwidth=0.2, select=NULL)
}
\arguments{
    \item{obj}{an object as returned by EstEval.} 
    \item{Sort}{if Sort is true then sort is applied. This helps (a bit)
       with estimation methods like black.box which may not return parameters
       of the same length or in the same order.}
    \item{bandwidth}{passed to \code{density} or \code{ksmooth}.}
    \item{graphs.per.page}{integer indicating number of graphs to place on a
       page.}
    \item{...}{other objects to be plotted (not working for some methods).}
    \item{invert}{logical indicating if the inverse of roots should be plotted}
    \item{mod}{logical indicating if the modulus of roots should be plotted}
    \item{select}{integer vector indicating roots to be plotted. If select 
        is not NULL then roots are sorted by magnitude and only the 
	indicated roots are plotted. For example, select=c(1,2)  will plot only 
	the two largest roots.}
}
\value{None}
\details{
\code{ksmooth} is applied if available to get a smoothed 
estimate of the distribution of the estimates. If \code{ksmooth} is not
available then \code{density} is applied if it is available.
}
\seealso{
\code{\link{EstEval}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- estVARXls(TSdata(output=outputData(eg1.DSE.data.diff)), max.lag=2)
# now use this as the true model
z <-  EstEval(model, 
    estimation="estVARXls", estimation.args=list(max.lag=2))
distribution(z) 
tfplot(z)
}
\concept{DSE}
\keyword{ts}


