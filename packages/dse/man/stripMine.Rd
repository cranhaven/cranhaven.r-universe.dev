\name{stripMine}
\alias{stripMine}
\title{Select a Data Subset and Model}
\description{Select a data subset and model.}
\usage{
    stripMine(all.data, essential.data=1, 
                       estimation.sample=.5, 
                       discard.before=1, horizons=1:12,quiet=FALSE,
                       estimation.methods=NULL,
                       step.size=NULL)
}
\arguments{
    \item{all.data}{An object of class TSdata.}
    \item{essential.data}{A vector indicating the important series.}
    \item{estimation.sample}{The portion of the data to use for estimation.}
    \item{discard.before}{
    Period before which data should be disacrded when calculating the 
    forecast covariances.}
    \item{horizons}{Forecast horizons which should be considered.}
    \item{quiet}{If T then estimation information is not printed. quiet=TRUE may also have to be
    set in the arguments to estimation methods.}
    \item{estimation.methods}{
    A list indicating the model estimation method to use. The list should contain
    one element. The name of the element indicates the estimation method to use
    and the value of the element is a list of arguments to pass to the 
    estimation method.}
    \item{step.size}{
    An integer indicting how many dta subset/model steps should be attempted.
    This may be necessary to accommodate memory constraints on the system. 
    (see below.)}
}
\value{
The returned result contains a list (\code{forecastCov}) of the forecast
covariance on the essential data for the various models and data
subsets. It can be plotted with the generic function tfplot. Additional
information in the result comes from the function arguments.
}
\details{
Calculate the predictions cov for essential.data of models estimated 
 with estimation methods indicated by estimation.methods. 
 estimation.methods is a list with syntax similar to programs
  for comparing estimation methods (eg. estimateModels), BUT ONLY 
  THE FIRST element (estimation method) is considered.
Essential.data indicates the subset of output variables to included in all
 models. It should be a vector of the indices. All possible combinations of
 input series and other output series data are considered. If omitted, essential.data is taken as the 
 first output series.
 Only forecast covariances for essential data are returned.
 discard.before is an integer indicating 1+the number of points in the
 beginning of predictions to discard for calculating prediction covariances.
 estimation.sample indicates the portion of the data to use for estimation.
  If estimation.sample is an integer then it is used to indicate the number
  of points in the sample to use for estimation. If it is a fracton it is
  used to indicate the portion of points to use for estimation. The remainder
  of the sample is used for evaluating predictions.
  If step.size is NULL then all possible data permutations are attempted.
   Because S has a hard-coded limit in the number of synchronize calls this is
   not always possible (For loops call synchronize.) An error message:
     Error in synchronize(1): No room in database table
   If step.size is not NULL it should be a positive integer. In this case 
   variable permutions are divided up into
   steps of the given size. The result returned by the function can be used
   to continue from the last step:
    intermediate.result <- stripMine(data, ...)
      intermediate.result <- stripMine(intermediate.result)
      intermediate.result <- stripMine(intermediate.result)
      result <- stripMine(intermediate.result)
  This can be done either interactively or in a batch process, but cannot be
  done in a function because the database table is not cleared until the top
  level expression is complete.
  The class of an intermediate result is stripMine.intermediate.result and
  the class of the final result is
         c('forecastCovEstimatorsWRTdata.subsets', 'forecastCov')
  If the final result is used in a call to stripMine then it is just 
  returned, so extra calls do not cause errors and are very quick.
  This is useful when you are too lazy to calculate the exact number of steps.
}
\seealso{
\code{\link{estBlackBox4}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
z <- stripMine(eg1.DSE.data.diff, 
    estimation.methods=list(bft=list(max.lag=2, verbose=FALSE)))
}
\concept{DSE}
\keyword{ts}

