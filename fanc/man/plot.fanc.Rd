\name{plot.fanc}
\alias{plot.fanc}
\alias{MakeInterface}
\alias{cbExposeCanvas}
\alias{cbExposeLabel}
\alias{cbValueChanged}
\alias{cbValueChangedLabel}
\alias{cbExposeLabelGamma}
\alias{cbExposeLabelLambda}
\alias{cbValueChangedGamma}
\alias{cbValueChangedLabelGamma}
\alias{cbValueChangedLabelLambda}
\alias{cbValueChangedLambda}
\title{
plot the solution path from a "fanc" object.
}
\description{
This functions plots the solution paths from a "fanc" object for fixed value of gamma.  
}
\usage{
\method{plot}{fanc}(x, Window.Height=500, type=NULL, df.method="active", ...)
}
\arguments{
  \item{x}{
Fitted \code{"fanc"} model object.
}
  \item{Window.Height}{
A window height. The default is 500.
}
  \item{type}{
Two plot types are supported.  If \code{"path"}, the path diagram is depicted.  If \code{"heatmap"}, the heatmap is depicted.
}
  \item{df.method}{
Two types of degrees of freedom are supported.  If \code{"reparametrization"}, the degrees of freedom of the MC+ are reparametrized based on the degrees of freedom of the lasso.  If \code{"active"}, the degrees of freedom of are the number of nonzero parameters.
}
  \item{\dots}{Other graphical parameters to plot}
}
\value{
NULL
}
\references{
Hirose, K. and Yamamoto, M. (2014). 
\emph{Sparse estimation via nonconcave penalized likelihood in a factor analysis model},\cr
  \emph{Statistics and Computing, in press}
}
\author{
Kei Hirose\cr
\email{mail@keihirose.com}
}

\seealso{\code{fanc} and \code{out} objects.}

