\name{classPrototypes}
\alias{classPrototypes}
\title{ The typical instances of each class - class prototypes}
\description{
For each class the most typical instances are returned based on the highest predicted probability for each class.
}
\usage{
classPrototypes(model, dataset, noPrototypes=10)
}
\arguments{
  \item{model}{ a \code{\link{CoreModel}} model.}
  \item{dataset}{ a dataset from which to get prototypes.}
  \item{noPrototypes}{number of instances of each class to return}
}
\details{
The function uses \code{predict.CoreModel(model, dataset)} for prediction of the \code{dataset} with
\code{model}. Based on the returned probabilities, it selects the \code{noPrototypes} instances with highest probabilities for each class to be 
typical representatives of that class, i.e., prototypes. The prototypes can be
visualized by calling e.g., \cr
\code{plot(model, dataset, rfGraphType="prototypes", noPrototypes = 10)}.
}
\value{
	A list with the most typical \code{noPrototypes} instances is returned. The list has the following attributes.
	\item{prototypes}{vector with indexes of the most typical instances}
	\item{clustering}{vector with class assignments for typical instances in vector \code{instances} }
	\item{levels}{the names of the class values.}
}
\examples{
dataset <- iris
md <- CoreModel(Species ~ ., dataset, model="rf", rfNoTrees=30,maxThreads=1)
typical <- classPrototypes(md, dataset, 10)
destroyModels(md) # clean up
}
\author{ John Adeyanju Alao (as a part of his BSc thesis) and Marko Robnik-Sikonja (thesis supervisor)}

\seealso{
\code{\link{predict.CoreModel}},
\code{\link{plot.CoreModel}}.
}
\references{
Leo Breiman: Random Forests. \emph{Machine Learning Journal}, 45:5-32, 2001
}

\keyword{robust}

