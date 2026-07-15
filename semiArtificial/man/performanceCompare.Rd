\name{performanceCompare}
\alias{performanceCompare}


\title{ Evaluate similarity of two data sets based on predictive performance}
\description{
   Depending on the type of problem (classification or regression), a classification performance (accuracy, AUC, brierScore, etc) 
   or regression performance (RMSE, MSE, MAE, RMAE, etc) on two data sets is used to compare the similarity of two data sets.
    }
\usage{
performanceCompare(data1, data2, formula, model="rf", stat=NULL, ...) 
}

\arguments{
  \item{data1}{ A \code{data.frame} containing the reference data.}
  \item{data2}{ A \code{data.frame} with the same number and names of columns as \code{data1}.}
  \item{formula}{ A \code{formula} specifying the response and predictive variables.}
  \item{model}{ A predictive model used for performance comparison. The default value "rf" stands for random forest, 
  but any classification or regression model supported by function \code{CoreModel} in 
  \href{https://CRAN.R-project.org/package=CORElearn}{CORElearn} package can be used. }
  \item{stat}{ A statistics used as performance indicator. The default value is NULL and means that for classification "accuracy" is used,
   and for regression "RMSE"" (relative mean squared error) is used. Other values supported and output by  \code{modelEval} 
   from \href{https://CRAN.R-project.org/package=CORElearn}{CORElearn} package can be used e.g., "AUC" or "brierScore".}
  \item{... }{ Additional parameters passed to \code{CoreModel} function. } 
}
\details{
   The function compares data stored in \code{data1} with \code{data2} by comparing models constructed on \code{data1}
    and evaluated on both \code{data1} and \code{data2} with models built on \code{data2} and evaluated on 
    both \code{data1} and \code{data2}. The difference between these performances are indicative on similarity of 
    the data sets if used in machine learning and data mining. The performance indicator used is determined 
    by parameter \code{stat}.
 }
\value{
  The method returns a list of performance indicators computed on both data sets:
 \item{diff.m1}{The difference between performance of model built on \code{data1} (and evaluated on both \code{data1} and \code{data2}.)}
 \item{diff.m2}{The difference between performance of model built on \code{data2} (and evaluated on both \code{data1} and \code{data2}.)}
  \item{perf.m1d1}{The performance of model built on \code{data1} on \code{data1}. }
  \item{perf.m1d2}{The performance of model built on \code{data1} on \code{data2}. }
  \item{perf.m2d1}{The performance of model built on \code{data2} on \code{data1}. }
  \item{perf.m2d2}{The performance of model built on \code{data2} on \code{data2}. }
}

\author{ Marko Robnik-Sikonja}

\seealso{
\code{\link{newdata.RBFgenerator}}.
}
\examples{
# use iris data set

# create RBF generator
irisGenerator<- rbfDataGen(Species~.,iris)

# use the generator to create new data
irisNew <- newdata(irisGenerator, size=200)

# compare statistics of original and new data
performanceCompare(iris, irisNew, Species~.)

}

\keyword{ datagen }
\keyword{ multivariate }