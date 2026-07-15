\name{plot.CoreModel}
\alias{plot.CoreModel}
\title{ Visualization of CoreModel models }
\description{
    The method \code{plot} visualizes the models returned by CoreModel()
    function or summaries obtained by applying these models to data.
    Different plots can be produced depending on the type of the model.
}
\usage{
    \method{plot}{CoreModel}(x, trainSet, rfGraphType=c("attrEval", "outliers", "scaling",
    "prototypes", "attrEvalCluster"), clustering=NULL, ...) 
}
\arguments{
    \item{x}{The model structure as returned by \code{\link{CoreModel}}.}          
    \item{trainSet}{ The data frame containing training data which produced the model \code{x}. }
    \item{rfGraphType}{ The type of the graph to produce for random forest models. See details.}
    \item{clustering}{The clustering of the training instances used in some model types. See details.}
    \item{\dots }{ Other options controlling graphical output passed to additional graphical functions.}
}
\details{
    The output of function \code{\link{CoreModel}} is visualized. Depending on the model type, different visualizations 
    are produced. Currently, classification tree, regression tree, and random forests are supported 
    (models "tree", "regTree", "rf", and "rfNear").
    
    For classification and regression trees (models "tree" and "regTree") the visualization produces a graph 
    representing structure
    of classification and regression tree, respectively. This process exploits graphical capabilities of 
    \code{rpart.plot} package. Internal structures of 
    \code{CoreModel} are converted to \code{\link[rpart]{rpart.object}} and then visualized by calling
    \code{\link[rpart.plot]{rpart.plot}} using default parameters. Any additional parameters are passed on to this function. For further 
    control use the \code{\link{getRpartModel}} function and call the function \code{\link[rpart.plot]{rpart.plot}} 
    or \code{\link[rpart]{plot.rpart}} with different parameters.  
    Note that \code{rpart.plot} can only display a single value in a leaf, which is not appropriate for model trees using e.g., 
    linear regression in the leaves. For these cases function \code{\link{display}} is a better alternative.
    
    For random forest models (models "rf" and "rfNear") different types of visualizations can be produced depending on the
    \code{graphType} parameter:
    \itemize{
    \item \code{"attrEval"} the attributes are evaluated with random forest model and the importance scores are then
                          visualized. For details see \code{\link{rfAttrEval}}.
    \item \code{"attrEvalClustering"} similarly to the \code{"attrEval"} the attributes are evaluated with random forest 
                            model and the importance scores are then visualized, but the importance scores are generated
                            for each cluster separately. The parameter \code{clustering} provides clustering information on
                            the \code{trainSet}. If \code{clustering} parameter is set to NULL, the class values are used as 
                            clustering information and visualization of attribute importance for each class separately is
                             generated.
                            For details see \code{\link{rfAttrEvalClustering}}.                         
    \item \code{"outliers"} the random forest proximity measure of training instances in \code{trainSet}
                         is visualized and outliers for each class separately can be detected.
                         For details see \code{\link{rfProximity}} and \code{\link{rfOutliers}}.  
    \item \code{"prototypes"} typical instances are found based on predicted class probabilities
            and their values are visualized (see \code{\link{classPrototypes}}). 
    \item \code{"scaling"} returns a scaling plot of training instances in a two dimensional space using
                     random forest based proximity as the distance (see \code{\link{rfProximity}} 
                     and a scaling function \code{\link{cmdscale}}).
    }       
}
\value{
    The method returns no value.
}
\examples{
# decision tree
dataset <- iris
md <- CoreModel(Species ~ ., dataset, model="tree")
plot(md, dataset) # additional parameters are passed directly to rpart.plot

# Additional visualizations can be obtained by explicit conversion to rpart.object 
#rpm <- getRpartModel(md,dataset)
# and than setting graphical parameters in plot.rpart and text.rpart
#require(rpart)
# E.g., set angle to tan(0.5)=45 (degrees) and length of branches at least 5, 
# try to make a dendrogram more compact
#plot(rpm, branch=0.5, minbranch=5, compress=TRUE)
#(pretty=0) full names of attributes, numbers to 3 decimals, 
#text(rpm, pretty=0, digits=3)

destroyModels(md) # clean up

# regression tree
dataset <- CO2
mdr <- CoreModel(uptake ~ ., dataset, model="regTree")
plot(mdr, dataset)
destroyModels(mdr) # clean up

#random forests
dataset <- iris
mdRF <- CoreModel(Species ~ ., dataset, model="rf", rfNoTrees=30, maxThreads=1)
plot(mdRF, dataset, rfGraphType="attrEval")
plot(mdRF, dataset, rfGraphType="outliers")
plot(mdRF, dataset, rfGraphType="scaling")
plot(mdRF, dataset, rfGraphType="prototypes")
plot(mdRF, dataset, rfGraphType="attrEvalCluster", clustering=NULL)
destroyModels(mdRF) # clean up

}


\author{ John Adeyanju Alao (initial implementation) and Marko Robnik-Sikonja (integration, improvements)}


\seealso{
\code{\link{CoreModel}},
\code{\link{rfProximity}},
\code{\link[cluster]{pam}},
\code{\link{rfClustering}},
\code{\link{rfAttrEvalClustering}},
\code{\link{rfOutliers}},
\code{\link{classPrototypes}},
\code{\link{cmdscale}}
}

\references{
Leo Breiman: Random Forests. \emph{Machine Learning Journal}, 45:5-32, 2001
}


\keyword{cluster}
\keyword{robust}
\keyword{tree}
