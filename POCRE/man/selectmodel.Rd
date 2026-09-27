\name{selectmodel}
\alias{selectmodel}
\title{
Select the Optimal Model
}
\description{
Select the optimal model from those fitted by POCRE, on the basis of prespecified criterion, such as EBIC, BIC, AIC, and AICc.
}
\usage{
selectmodel(ppobj, msc=NULL)
}
\arguments{
  \item{ppobj}{
output from \link{pocrepath}.
}
  \item{msc}{
a value indicating the information criterion: 0 for BIC, (0,1] for EBIC (by default), 2 for AIC, 3 for AICc.
}
}
\value{
{output of \link{pocre} for the optimal model.}
}
\references{
Chen J and Chen Z (2008) Extended Bayesian information criteria for
model selection with large model spaces. \emph{Biometrika}, 95: 759-771.

Zhang D, Lin Y, and Zhang M (2009). Penalized orthogonal-components regression for large p small n data. \emph{Electronic Journal of Statistics}, 3: 781-796.
}
\author{Dabao Zhang, Zhongli Jiang, Zeyu Zhang, Department of Statistics, Purdue University}
\seealso{
\code{\link{pocrepath}}, \code{\link{plot.pocrepath}}.
}
\examples{
data(simdata)
xx <- scale(as.matrix(simdata[,-1]))
yy <- scale(as.matrix(simdata[,1]))

# ppres <- pocrepath(yy,xx,delta=0.01)
ppres <- pocrepath(yy,xx)
fres <- selectmodel(ppres)
}
