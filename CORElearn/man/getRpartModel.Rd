\name{getRpartModel}
\alias{getRpartModel}
\title{ Conversion of a CoreModel tree into a rpart.object }
\description{ The function converts a given CoreModel model (decision or regression tree) 
into a \code{rpart.object} prepared for visualization with \code{plot} function.
 }
\usage{
  getRpartModel(model, dataset)
}
\arguments{
  \item{model}{ A tree model produced by \code{\link{CoreModel}} }
  \item{dataset}{ A data set which was used in learning of the \code{model}.}
}
\details{
The conversion creates \code{rpart.object} and copies CORElearn internal structures contained in memory controlled by 
dynamic link library written in C++. 

An alternative visualization is accessible via function \code{\link{display}}, which outputs tree structure formatted 
for screen or in dot format.
}
\value{ Function returns a \code{\link[rpart]{rpart.object}}.}
\author{ Initial version by John Adeyanju Alao, improvements by Marko Robnik-Sikonja.}

\examples{
# plot a decision tree directly
dataset <- CO2
md<-CoreModel(Plant ~ ., dataset, model="tree")
plot(md, dataset)

# or indirectly 
rpm <- getRpartModel(md, dataset)
# set angle to tan(0.5)=45 (degrees) and length of branches at least 5 
plot(rpm, branch=0.5, minbranch=5, compress=TRUE)
# pretty=0 prints full names of attributes, 
# numbers to 3 decimals, try to make a dendrogram more compact
text(rpm, pretty=0, digits=3)
destroyModels(md) # clean up

# an alternative is to use fancier rpart.plot package
# rpart.plot(rpm) # rpart.plot has many parameters controlling the output
# but it cannot plot models in tree leaves 
}

\seealso{\code{\link{CoreModel}}, \code{\link{plot.CoreModel}},\code{\link[rpart]{rpart.object}}, \code{\link{display}}
}

\keyword{tree}
