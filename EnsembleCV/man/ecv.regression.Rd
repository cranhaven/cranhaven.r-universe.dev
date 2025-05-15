\name{ecv.regression}
\alias{ecv.regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cross-Validation-Based Integration of Regression Base Learners for Ensemble Learning
}
\description{
This function uses repeated cross-validation to find the base learner configuration with smallest error. It then trains and returns the chosen model (base learner and configuration), trained on the full data set.
}
\usage{
ecv.regression(formula, data
  , baselearner.control = ecv.regression.baselearner.control()
  , integrator.control = ecv.regression.integrator.control()
  , ncores = 1, filemethod = FALSE, print.level = 1
  , preschedule = TRUE
  , schedule.method = c("random", "as.is", "task.length")
  , task.length
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{Formula expressing response variable and covariates.}
  \item{data}{Data frame containing the response variable and covariates.}
  \item{baselearner.control}{Control structure determining the base learners, their configurations, and data partitioning details. See \code{\link{ecv.regression.baselearner.control}}.}
  \item{integrator.control}{Control structure governing integrator behavior. See \code{\link{ecv.regression.integrator.control}}.}
  \item{ncores}{Number of cores used for parallel training of base learners.}
  \item{filemethod}{Boolean flag indicating whether or not to save estimation objects to disk or not. Using \code{filemethod=T} reduces RAM pressure.}
  \item{print.level}{Controlling verbosity level.}
  \item{preschedule}{Boolean flag, indicating whether base learner training jobs must be scheduled statically (\code{TRUE}) or dynamically (\code{FALSE}).}
  \item{schedule.method}{Method used for scheduling tasks on threads. In "as.is" tasks are assigned to threads in a round-robin fashion for static scheduling. In dynamic scheduling, tasks form a queue without any re-ordering. In "random", tasks are first randomly shuffled, and the rest is similar to "as.is". In "task.length", a heuristic algorithm is used in static scheduling for assigning tasks to threads to minimize load imbalance, i.e. make total task lengths in threads roughly equal. In dynamic scheduling, tasks are sorted in descending order of expected length to form the task queue.}
  \item{task.length}{Vector of estimated task lengths, to be used in the "task.length" method of scheduling.}
}

\value{
An object of classes \code{ecv.regression} (if \code{filemethod==TRUE}, also has class of \code{ecv.file}), a list with the following elements:
  \item{call}{Copy of function call.}
  \item{formula}{Copy of formula argument in function call.}
  \item{instance.list}{An object of class \code{\link[EnsembleBase]{Instance.List-class}}, containing all permutations of base learner configurations and random data partitions generated in the body of the function.}
  \item{integrator.config}{Copy of configuration object passed to the integrator. Object of class \code{\linkS4class{Regression.Select.MinAvgErr.Config}}.}
  \item{method}{Integration method. Currently, only "default" is supported.}
  \item{est}{A list with these elements: 1) \code{baselearner.cv.batch}, an object of class \code{\link[EnsembleBase]{Regression.CV.Batch.FitObj-class}} containing the fit object from CV batch training of base learners; 2) \code{baselearner.batch}, an object of class \code{\link[EnsembleBase]{Regression.Batch.FitObj-class}} containing the fit object from batch training of base learners on entire \code{data}; 3) \code{integrator}, an object of class \code{\linkS4class{Regression.Select.MinAvgErr.FitObj}} containing the fit object returned by the integrator.}
  \item{y}{Copy of response variable vector.}
  \item{pred}{Within-sample prediction of the ensemble model.}
  \item{filemethod}{Copy of passed-in \code{filemethod} argument.}
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

\seealso{
\code{\link{ecv.regression.baselearner.control}}, \code{\link{ecv.regression.integrator.control}}, \code{\link[EnsembleBase]{Instance.List-class}}, \code{\linkS4class{Regression.Select.MinAvgErr.Config}}, \code{\link[EnsembleBase]{Regression.CV.Batch.FitObj-class}}, \code{\link[EnsembleBase]{Regression.Batch.FitObj-class}}, \code{\linkS4class{Regression.Select.MinAvgErr.FitObj}}
}
\examples{
data(servo)
myformula <- class~motor+screw+pgain+vgain
perc.train <- 0.7
index.train <- sample(1:nrow(servo), size = round(perc.train*nrow(servo)))
data.train <- servo[index.train,]
data.predict <- servo[-index.train,]
## to run longer test using all 5 default regression base learners
## try: est <- ecv.regression(myformula, data.train, ncores=2)
est <- ecv.regression(myformula, data.train, ncores=2
  , baselearner.control = 
      ecv.regression.baselearner.control(baselearners = c("knn")))
newpred <- predict(est, data.predict)
}
