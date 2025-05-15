\name{predict.fit}
\alias{predict-methods}
\alias{predict.fit}
\alias{predict,model-method}
\title{predict method for fit objects (rminer)}
\description{predict method for fit objects (rminer)}

\section{Methods}{
\describe{

\item{\code{signature(object = "model")}}{ describe this method here }
}}


\arguments{

  \item{object}{a model object created by \code{\link{fit}}}
  \item{newdata}{a data frame or matrix containing new data}

}

\details{
         Returns predictions for a fit model. Note: the \code{...} optional argument is currently only used by \code{cubist} model (see example).
        }
\value{
   If \code{task} is \code{prob} returns a matrix, where each column is the class probability.\cr
   If \code{task} is \code{class} returns a factor.\cr
   If \code{task} is \code{reg} returns a numeric vector.\cr
   }

\seealso{
\code{\link{fit}}, \code{\link{mining}}, \code{\link{mgraph}}, \code{\link{mmetric}}, \code{\link{savemining}}, \code{\link{CasesSeries}}, \code{\link{lforecast}} and \code{\link{Importance}}.
}

\references{
\itemize{
\item To check for more details about rminer and for citation purposes:\cr
P. Cortez.\cr
Data Mining with Neural Networks and Support Vector Machines Using the R/rminer Tool.\cr
In P. Perner (Ed.), Advances in Data Mining - Applications and Theoretical Aspects 10th Industrial Conference on Data Mining (ICDM 2010), Lecture Notes in Artificial Intelligence 6171, pp. 572-583, Berlin, Germany, July, 2010. Springer. ISBN: 978-3-642-14399-1.\cr
@Springer: \url{https://link.springer.com/chapter/10.1007/978-3-642-14400-4_44}\cr
\url{http://www3.dsi.uminho.pt/pcortez/2010-rminer.pdf}\cr

\item This tutorial shows additional code examples:\cr
P. Cortez.\cr
A tutorial on using the rminer R package for data mining tasks.\cr
Teaching Report, Department of Information Systems, ALGORITMI Research Centre, Engineering School, University of Minho, Guimaraes, 
Portugal, July 2015.\cr
\url{http://hdl.handle.net/1822/36210}\cr
}
}

\examples{
### simple classification example with logistic regression
data(iris)
M=fit(Species~.,iris,model="lr")
P=predict(M,iris)
print(mmetric(iris$Species,P,"CONF")) # confusion matrix

### simple regression example
data(sa_ssin)
H=holdout(sa_ssin$y,ratio=0.5,seed=12345)
Y=sa_ssin[H$ts,]$y # desired test set
# fit multiple regression on training data (half of samples)
M=fit(y~.,sa_ssin[H$tr,],model="mr") # multiple regression
P1=predict(M,sa_ssin[H$ts,]) # predictions on test set
print(mmetric(Y,P1,"MAE")) # mean absolute error

### fit cubist model
M=fit(y~.,sa_ssin[H$tr,],model="cubist") #
P2=predict(M,sa_ssin[H$ts,],neighbors=3) #
print(mmetric(Y,P2,"MAE")) # mean absolute error
P3=predict(M,sa_ssin[H$ts,],neighbors=7) #
print(mmetric(Y,P3,"MAE")) # mean absolute error

### check fit for more examples
}

\keyword{methods}
\keyword{classif}
\keyword{regression}
\keyword{neural}
\keyword{nonlinear}
