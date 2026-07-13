\name{predict.isotonic}
\alias{predict.isotonic}
\title{
  Interpolation from Isotonic Regression
}
\description{
  Interpolates a prediction from an isotonic regression object
}
\usage{
\method{predict}{isotonic}(object,newdata,...)
}
\arguments{
  \item{object}{
    The isotonic regression object, consisting of a list with three
    components:
    \describe{
      \item{x}{A sorted vector of cut-off values for the step function.}
      \item{y}{The corresponding values for the fitted response variable.}
      \item{data}{The original response values used to fit the data.}
    }      
  }
  \item{newdata}{
    The values for which the function is to be calculated.
  }
  \item{...}{
    Any additional arguments are ignored.
  }
}
\details{
  For each element of newdata, this function identifies the first
  element of object$x that is larger, and looks up the corresponding
  element of object$y. 
}
\value{
  A vector of corresponding values of the step function.
}
\author{
Toby Kenney
}
\examples{
set.seed(1)
x1<-seq_len(1000)
y1<-rnorm(1000)+3*exp(-x1/500)
x2<-seq_len(100)*2.001
y2<-rnorm(100)+3*exp(-x2/500)

ir<-isotonic.regression(x1,y1,x2,y2)
fitted<-predict(ir,newdata=seq_len(10000)/10)
}
