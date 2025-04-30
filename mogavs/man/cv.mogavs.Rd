\name{cv.mogavs}
\alias{cv.mogavs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{k-Fold Crossvalidation for a mogavs model
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Performs k-fold CV for a model of class \code{mogavs} via the cvTools package.
}
\usage{
cv.mogavs(mogavs, nvar, data, y_ind, K = 10, R = 1, order = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mogavs}{
%%     ~~Describe \code{mogavs} here~~
A model of class \code{mogavs}.
}
  \item{nvar}{
%%     ~~Describe \code{nvar} here~~
The number of variables for which you want to run k-fold CV.
}
  \item{data}{
%%     ~~Describe \code{data} here~~
  Used data set.
}
  \item{y_ind}{
%%     ~~Describe \code{y_ind} here~~
The column number for the y-variable in the dataset.
}
  \item{K}{
%%     ~~Describe \code{K} here~~
Number of folds in the cross-validation, default K=10.
}
  \item{R}{
%%     ~~Describe \code{R} here~~
Number of repeats for the CV, default R=1.
}
  \item{order}{
%%     ~~Describe \code{order} here~~
Logical, whether the result should be sorted by the column \code{CVerror}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Perform k-fold cross-validation for all the linear models with \code{nvar} number of variables, which have been tried during the course of the genetic algorithm. 
}
\value{
%%  ~Describe the value returned
A data frame with the following columns:
\item{archInd}{The row index of the linear model in the \code{archiveSet} of the \code{mogavs} model.}
\item{formula}{The formula of the linear model as a character string.}
\item{CVerror}{The root mean square error of the model.}
\item{CVse}{The standard error of the model across the \code{R} runs of the cross-validation. NA if R=1.}
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
%%  ~~who you are~~
Tommi Pajala <tommi.pajala@aalto.fi>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mogavsToLinear}}

}
\examples{
data(sampleData)
mod<-mogavs(y~.,data=sampleData,maxGenerations=20)
cv.mogavs(mod,nvar=3,data=sampleData,y_ind=1,K=10,R=1,order=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}% __ONLY ONE__ keyword per line
