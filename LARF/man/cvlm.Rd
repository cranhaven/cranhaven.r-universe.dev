\name{cvlm}
\alias{cvlm}

\title{Cross-validation of a Linear Regression Model}
\description{
Provides cross-validation of a linear regression model}
\usage{
cvlm(form.lm, data, m=10, seed = NULL)
}

\arguments{
  \item{form.lm}{formula of the regression model.}
  \item{data}{data including outcome and covaraites.}
  \item{m}{the number of folds to be used in cross-validation.}
  \item{seed}{random starting number used to replicate cross-validation.}
}

\details{
This function finds the optimal order of the covariates power series through cross-validation.
}

\value{
\item{sumres}{Sum of residual squares divided by degree of freedom.}
\item{df}{Degree of freedom which equals to the number of valid predictions minus the number of parameters.}
\item{m}{the number of folds to be used in cross-validation.}
\item{seed}{The random seed.}
}

\note{
In making the code, we adopted part of the \code{CVlm} in \code{DAAG} (Maindonald and Braun, 2015). \cr
\url{https://cran.r-project.org/package=DAAG}

}
\author{
Weihua An, Departments of Sociology and Statistics, Indiana University Bloomington, \email{weihuaan@indiana.edu}. \cr
Xuefu Wang, Department of Statistics, Indiana University Bloomington, \email{wangxuef@umail.iu.edu}.
}

\seealso{
\code{\link{larf}}, \code{\link{npse}}
}

