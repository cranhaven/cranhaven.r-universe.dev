\name{kmPredictWrapper}
\alias{kmPredictWrapper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for the Kriging model prediction
}
\description{
Wrapper for the Kriging model prediction function \code{\link{predict.km}} from package \code{\link[DiceKriging:DiceKriging-package]{DiceKriging}} to simplify the use of Kriging prediction functions as arguments for functions like \code{\link{estimateGraph}} or \code{\link{fast99}}.
}
\usage{
kmPredictWrapper(newdata, km.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{newdata}{a vector, matrix or data frame containing the points where to perform predictions
%%     ~~Describe \code{newdata} here~~
}
  \item{km.object}{an object of class \code{km}
%%     ~~Describe \code{m} here~~
}
}
\value{kriging mean computed at \code{newdata}
}
\author{
J. Fruth, O. Roustant
}

\seealso{
\code{\link{estimateGraph}}
}
\examples{
### graph estimation of a kriging prediction of the ishigami function
set.seed(1)
x <- matrix(runif(150,-pi,pi),100,3)
y <- ishigami.fun(x)
KM <- km(~1, design = data.frame(x), response = y)

g <- estimateGraph(f.mat = kmPredictWrapper, d = 3, n.tot = 10000, q.arg = 
  list(min = -pi, max = pi), method = "LiuOwen", km.object = KM) 
print(g$tii)
}
