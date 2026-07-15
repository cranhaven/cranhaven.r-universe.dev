\name{CORElearn-internal}
\alias{CORElearn-internal}
\title{Internal structures of CORElearn C++ part}
\description{
The package CORElearn is an R port of CORElearn data mining system.
This document is a short description of the C++ part which can also
serve as a standalone Linux or Windows data mining system,
its organization and main classes and data structures.
}
 
\details{
The C++ part is called from R functions collected in file \code{Rinterface.R}.
The C++ functions called from R and providing interface to R are collected in \code{Rfront.cpp}
and \code{Rconvert.cpp}. The front end for standalone version is in file \code{frontend.cpp}.
For many parts of the code there are two variants, classification and regression one.
Regression part usually has \code{Reg} somewhere in its name.
The main classes are 
\itemize{
\item \code{marray, mmatrix} are templates for storing vectors and matrixes
\item \code{dataStore}  contains data storage and data manipulation methods, of which the most important are
\itemize{
\item   \code{mmatrix<int> DiscData, DiscPredictData} contain values of discrete attributes and class for training and prediction (optional).
In classification column 0 always    stores class values.
\item \code{mmatrix<double> ContData, ContPredictData} contain values of numeric attribute and prediction values for training and prediction (optional). 
In regression column 0 always  stores target values.
\item \code{marray<attribute> AttrDesc} with information about attributes' types, number of values, min, max, column index in DiscData or ContData, ...
}
\item \code{estimation, estimationReg} evaluate attributes with different purposes: decision/regression tree splitting, binarization, 
discretization, constructive induction, feature selection, etc. Because of efficiency these classes store its own data in
\itemize{
\item   \code{mmatrix<int> DiscValues} containing discrete attributes and class values,
\item \code{mmatrix<double> ContValues} containing numeric attribute and prediction values.
}
\item \code{Options} stores and handles all the parameters of the system.
\item \code{featureTree, regressionTree} build all the models, predict with them, and create output.
}
}
\author{
Marko Robnik-Sikonja
}

\keyword{ models }
\keyword{ regression }
\keyword{ tree }
\keyword{ classif }

\seealso{
\code{\link{CORElearn}}, \code{\link{CoreModel}}, \code{\link{predict.CoreModel}},
\code{\link{modelEval}}, \code{\link{attrEval}}, \code{\link{ordEval}},
\code{\link{plot.ordEval}}, \code{\link{helpCore}}, \code{\link{paramCoreIO}},
\code{\link{infoCore}}, \code{\link{versionCore}}.
}


