\name{qkgda-class}
\docType{class}
\alias{qkgda-class}
\alias{prj}
\alias{eVal}
\alias{eVec}
\alias{label}
\alias{prj<-}
\alias{eVal<-}
\alias{eVec<-}
\alias{label<-}

\alias{prj,qkgda-method}
\alias{eVal,qkgda-method}
\alias{eVec,qkgda-method}
\alias{label,qkgda-method}
\alias{prj<-,qkgda-method}
\alias{eVal<-,qkgda-method}
\alias{eVec<-,qkgda-method}
\alias{label<-,qkgda-method}

\title{Class "qkgda"}
\description{ The qkernel Generalized Discriminant Analysis class}
\section{Objects of class "qkgda"}{
  Objects can be created by calls of the form \code{new("qkgda", ...)}.
  or by calling the \code{qkgda} function.
}
\section{Slots}{
  \describe{
    \item{\code{prj}:}{Object of class \code{"matrix"} containing the normalized projections on eigenvectors}
    \item{\code{eVal}:}{Object of class \code{"matrix"} containing the corresponding eigenvalues}
    \item{\code{eVec}:}{Object of class \code{"matrix"} containing the corresponding eigenvectors}
    \item{\code{label}:}{Object of class \code{"matrix"} containing the categorical variables that the categorical data be assigned to one of the categories}
  }
}
\section{Methods}{
  \describe{
    \item{prj}{\code{signature(object = "qkgda")}: returns the normalized projections}
    \item{eVal}{\code{signature(object = "qkgda")}: returns the eigenvalues}
    \item{eVec}{\code{signature(object = "qkgda")}: returns the eigenvectors}
    \item{kcall}{\code{signature(object = "qkgda")}: returns the performed call}
    \item{cndkernf}{\code{signature(object = "qkgda")}: returns the used kernel function}
    \item{predict}{\code{signature(object = "qkgda")}: embeds new data}
    \item{xmatrix}{\code{signature(object = "qkgda")}: returns the used data matrix}
  }
}

\author{Yusen Zhang\cr \email{yusenzhang@126.com}}

\seealso{
  \code{\link{qkernel-class}},
  \code{\link{cndkernel-class}}
}
\examples{
Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]), Sp = rep(c("1","2","3"), rep(50,3)))
testset <- sample(1:150,20)
train <- as.matrix(iris[-testset,-5])
test <- as.matrix(iris[testset,-5])
Sp = rep(c("1","2","3"), rep(50,3))
labels <-as.numeric(Sp)
trainlabel <- labels[-testset]
testlabel <- labels[testset]

kgda1 <- qkgda(train, label=trainlabel, kernel = "ratibase", qpar = list(c=1,q=0.9),features = 2)

prj(kgda1)
eVal(kgda1)
eVec(kgda1)
cndkernf(kgda1)
kcall(kgda1)
}
\keyword{classes}
