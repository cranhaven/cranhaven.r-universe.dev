\name{savemining}
\alias{savemining}
\alias{savemodel}
\alias{loadmining}
\alias{loadmodel}
\title{
Load/save into a file the result of a fit (model) or mining functions.
}
\description{
Load/save into a file the result of a \code{\link{fit}} (model) or \code{\link{mining}} functions.
}
\usage{savemining(mmm_mining, file, ascii = TRUE)}
\arguments{
  \item{mmm_mining}{the list object that is returned by the \code{\link{mining}} function.}
  \item{file}{filename that should include an extension}
  \item{ascii}{if \code{TRUE} then ascii format is used to store the file (larger file size), else a binary format is used.}
}
\details{
Very simple functions that do what their names say. Additional usages are:\cr
\code{loadmining(file)}\cr
\code{savemodel(MM_model,file,ascii=FALSE)}\cr
\code{loadmodel(file)}\cr
}
\value{
\code{loadmining} returns a \code{\link{mining}} mining list, while \code{loadmodel} returns a \code{model} object (from \code{\link{fit}}).
}
\references{
See \code{\link{fit}}.
}
\author{
Paulo Cortez \url{http://www3.dsi.uminho.pt/pcortez/}
}

\seealso{
\code{\link{fit}}, \code{\link{predict.fit}}, \code{\link{mining}}, \code{\link{mgraph}}, \code{\link{mmetric}}, \code{\link{savemining}}, \code{\link{Importance}}.
}
\examples{
### dontrun is used here to avoid the creation of a new file
### in the CRAN servers. The example should work fine:
\dontrun{
data(iris)
M=fit(Species~.,iris,model="rpart")
tempdirpath=tempdir()
filename=paste(tempdirpath,"/iris.model",sep="")
savemodel(M,filename) # saves to file
M=NULL # cleans M
M=loadmodel(filename) # load from file
print(M)
}
}
\keyword{file}
\keyword{classif}
\keyword{regression}
\keyword{neural}
\keyword{nonlinear}
