\name{epcreg.save}
\alias{epcreg.save}
\alias{epcreg.load}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Custom Functions for Disk I/O in \pkg{EnsemblePCReg} Package
}
\description{
These functions can be used whether \code{filemethod} flag is set to \code{TRUE} or \code{FALSE} during the \code{epcreg} call. Note that \code{epcreg.load} `returns' the estimation object (in contrast to the standard \code{load} method).
}
\usage{
epcreg.save(obj, file)
epcreg.load(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{Object of classes \code{"\link{epcreg}"} (and possibly \code{"epcreg.file"}), typically the output of call to function \code{epcreg}.}
  \item{file}{Filepath to where \code{obj} must be saved to / loaded from.}
}

\value{
Function \code{epcreg.load} returns the saved \code{obj}, with estimation files automatically copied to R temporary directory, and filepaths inside the \code{obj} fields updated to point to these new filepaths (if \code{filemethod} was set to \code{TRUE} in the call to \code{epcreg}).
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}


\seealso{
\code{\link{epcreg}}
}
\examples{
data(servo)
myformula <- class~motor+screw+pgain+vgain
perc.train <- 0.7
index.train <- sample(1:nrow(servo), size = round(perc.train*nrow(servo)))
data.train <- servo[index.train,]
data.predict <- servo[-index.train,]
est <- epcreg(myformula, data.train, ncores=2
  , baselearner.control=epcreg.baselearner.control(
    baselearners="knn"
    , baselearner.configs = make.configs("knn"
      , config.df = expand.grid(kernel = "rectangular"
        , k = c(5, 10)))), filemethod = TRUE)
epcreg.save(est, "somefile")
rm(est) # alternatively, exit and re-launch R session
est.loaded <- epcreg.load("somefile")
newpred <- predict(est.loaded, data.predict)
file.remove("somefile")
}

