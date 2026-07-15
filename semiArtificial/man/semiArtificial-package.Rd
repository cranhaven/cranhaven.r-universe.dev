\name{semiArtificial-package}
\alias{semiArtificial-package}
\alias{semiArtificial}

\docType{package}
\title{Generation and evaluation of semi-artificial data}
\description{
The package semiArtificial contains methods to generate and evaluate semi-artificial data sets.
Different data generators take a data set as an input, learn its properties using machine learning 
algorithms and generates new data with the same properties.
}
\details{
 The package currently includes the following data generators:
 \itemize{
\item a RBF network based generator using rbfDDA model from RSNNS package.
\item generator using density tree forest for unsupervised data,
\item generator using random forest for classification and regression.
}
Data evaluation support tools include:
\itemize{
\item statistical evaluation: mean, median,standard deviation, skewness, kurtosis, medcouple, L/RMC,
\item evaluation based on clustering using Adjusted Rand Index (ARI) and Fowlkes-Mallows index (FM),
\item evaluation based on prediction with a model, e.g., random forests.
}
Further software and development versions are available at \url{http://lkm.fri.uni-lj.si/rmarko/software/}. 
}
\author{
Marko Robnik-Sikonja 
}
\references{
Marko Robnik-Sikonja: Not enough data? Generate it!. \emph{Technical Report, University of Ljubljana, Faculty of Computer and Information Science}, 2014

Other references are available from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}

}

\keyword{ package }
\keyword{ datagen }
\keyword{ multivariate }

\seealso{
\code{\link{rbfDataGen}},
\code{\link{treeEnsemble}},
\code{\link{newdata}},
\code{\link{dataSimilarity}},
\code{\link{dsClustCompare}},
\code{\link{performanceCompare}}.
}
