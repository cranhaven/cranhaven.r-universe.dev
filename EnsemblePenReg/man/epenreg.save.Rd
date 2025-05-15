\name{epenreg.save}
\alias{epenreg.save}
\alias{epenreg.load}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Custom Functions for Disk I/O in \pkg{EnsemblePenReg} Package
}
\description{
These functions can be used whether \code{filemethod} flag is set to \code{TRUE} or \code{FALSE} during the \code{epenreg} call. Note that \code{epenreg.load} `returns' the estimation object (in contrast to the standard \code{load} method).
}
\usage{
epenreg.save(obj, file)
epenreg.load(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{Object of classes \code{"\link{epenreg}"} (and possibly \code{"epenreg.file"}), usually the output of call to function \code{epenreg}.}
  \item{file}{Filepath to where \code{obj} must be saved to / loaded from.}
}

\value{
Function \code{epenreg.load} returns the saved \code{obj}, with estimation files automatically copied to R temporary directory, and filepaths inside the \code{obj} fields updated to point to these new filepaths.
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}


\seealso{
\code{\link{epenreg}}
}
\examples{
\dontrun{
data(servo)
myformula <- class~motor+screw+pgain+vgain
perc.train <- 0.7
index.train <- sample(1:nrow(servo), size = round(perc.train*nrow(servo)))
data.train <- servo[index.train,]
data.predict <- servo[-index.train,]

est <- epenreg(myformula, data.train, ncores=2, filemethod=TRUE
  , baselearner.control=epenreg.baselearner.control(baselearners="knn"))
epenreg.save(est, "somefile")
rm(est) # alternatively, exit and re-launch R session
est.loaded <- epenreg.load("somefile")
newpred <- predict(est.loaded, data.predict)

# can also be used with filemethod set to FALSE
est <- epenreg(myformula, data.train, ncores=2, filemethod=FALSE
  , baselearner.control=epenreg.baselearner.control(baselearners="knn"))
epenreg.save(est, "somefile")
rm(est) # alternatively, exit and re-launch R session
est.loaded <- epenreg.load("somefile")
newpred <- predict(est.loaded, data.predict)
}
}

