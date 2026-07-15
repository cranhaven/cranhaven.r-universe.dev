\name{display.CoreModel}
\alias{display.CoreModel}
\alias{display}

\title{ Displaying decision and regression trees }
\description{
    The method \code{display} prints the tree models returned by \code{CoreModel()}
    function. Depending of parameter \code{format} the output is prepared for either screen or in dot format.
}
\usage{
    \method{display}{CoreModel}(x, format=c("screen","dot"))
}
\arguments{
    \item{x}{The model structure as returned by \code{\link{CoreModel}}.}          
    \item{format}{ The type of output, i.e., prepared for screen display or in dot language}
 }
\details{
    The tree based models returned by function \code{\link{CoreModel}} are visualized.
    Only tree based models supported, including the trees which include other prediction models in their leaves.
    Tree based models available are decision trees (obtained by using parameter \code{model="tree"} in CoreModel),
    and regression trees (with \code{model="regTree"}). 
    
    Models in the leaves of decision trees
    can be set using parameter \code{modelType} in \code{\link{CoreModel}}. 
    At the moment naive Bayes and kNN are available, for details see \link{helpCore}.  
    
    Models in the leaves of regression trees can be set using parameter \code{modelTypeReg} in \code{\link{CoreModel}}. 
    At the moment kNN, kernel regression, and several types of linear models  are available,
    for details see \link{helpCore}. 
    
    The output in dot language can be used with graphViz visualization software to create model
    visualization in various formats. 
  }
\value{
    The method invisibly returns a printed character vector.
}
\examples{
# decision tree
dataset <- CO2
md <- CoreModel(Plant ~ ., dataset, model="tree")
display(md)
destroyModels(md) #clean up

# regression tree
dataset <- CO2
mdr <- CoreModel(uptake ~ ., dataset, model="regTree")
display(mdr, format="dot")
destroyModels(mdr) # clean up

}


\author{ Marko Robnik-Sikonja}


\seealso{
\code{\link{CoreModel}},
\code{\link{plot.CoreModel}}.
}

\keyword{tree}
