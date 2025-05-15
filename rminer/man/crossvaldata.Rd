\name{crossvaldata}
\alias{crossvaldata}
\title{
Computes k-fold cross validation for rminer models.
}
\description{
Computes k-fold cross validation for rminer models.
}
\usage{
crossvaldata(x, data, theta.fit, theta.predict, ngroup = 10, 
             mode = "stratified", seed = NULL, model, task, feature = "none",
             ...)
}
\arguments{
  \item{x}{See \code{\link{fit}} for details.}
  \item{data}{See \code{\link{fit}} for details.}
  \item{theta.fit}{fitting function}
  \item{theta.predict}{prediction function}
  \item{ngroup}{number of folds}
  \item{mode}{Possibilities are: "stratified", "random" or "order" (see \code{\link{holdout}} for details).}
  \item{seed}{if \code{NULL} then no seed is used and the current R randomness is assumed; else a fixed seed is adopted to generate local random sample sequences, returning always the same result for the same seed (local means that it does not affect the state of other random number generations called after this function, see \code{\link{holdout}} example).}
  \item{model}{See \code{\link{fit}} for details.}
  \item{task}{See \code{\link{fit}} for details.}
  \item{feature}{See \code{\link{fit}} for details.}
  \item{\dots}{Additional parameters sent to \code{theta.fit} or \code{theta.predic} (e.g. \code{search})}
}
\details{
Standard k-fold cross-validation adopted for rminer models.
By default, for classification tasks ("class" or "prob") a stratified sampling is used 
(the class distributions are identical for each fold), unless \code{mode} is set to \code{random} or \code{order} 
(see \code{\link{holdout}} for details).
}
\note{A better control (e.g. use of several Runs) is achieved using the simpler \code{\link{mining}} function.}

\value{
Returns a list with:
      \itemize{
               \item $cv.fit -- all predictions (factor if \code{task="class"}, matrix if \code{task="prob"} or numeric if \code{task="reg"});
               \item $model -- vector list with the model for each fold. 
               \item $mpar -- vector list with the mpar for each fold;
               \item $attributes -- the selected attributes for each fold if a feature selection algorithm was adopted;
               \item $ngroup -- the number of folds;
               \item $leave.out -- the computed size for each fold (=\code{nrow(data)/ngroup});
               \item $groups -- vector list with the indexes of each group;
               \item $call -- the call of this function;
   
              }
}
\references{
Check the \code{\link[bootstrap]{crossval}} function.
}

\author{This function was adapted by Paulo Cortez from the \code{crossval} function of the bootstrap library (S original by R. Tibshirani and R port by F. Leisch).}

\seealso{
\code{\link{holdout}}, \code{\link{fit}}, \code{\link{mining}} and \code{\link{predict.fit}}.}
\examples{
### dontrun is used when the execution of the example requires some computational effort.
\dontrun{
 data(iris)
 # 3-fold cross validation using fit and predict
 # the control argument is sent to rpart function 
 # rpart.control() is from the rpart package
 M=crossvaldata(Species~.,iris,fit,predict,ngroup=3,seed=12345,model="rpart",
                task="prob", control = rpart::rpart.control(cp=0.05))
 print("cross validation object:")
 print(M)
 C=mmetric(iris$Species,M$cv.fit,metric="CONF")
 print("confusion matrix:")
 print(C)
}
}
\keyword{ models }
