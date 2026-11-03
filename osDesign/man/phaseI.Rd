\name{phaseI}
\Rdversion{1.7}
\alias{phaseI}
\title{
Expected phase I stratification
}
\description{
  \code{phaseI()} provides the expected phase I counts, based on a pre-specified population and outcome model. If phase II sample sizes are provided, the (expected) phase II sampling probabilities are also reported.
}
\usage{
phaseI(betaTruth, X, N, strata=NULL, expandX="all", etaTerms=NULL,
      nII0=NULL, nII1=NULL,
      cohort=TRUE, NI=NULL, digits=NULL)
}
\arguments{
	\item{betaTruth}{
    Regression coefficients from the logistic regression model.
}
  \item{X}{
    Design matrix for the logistic regression model. The first column should correspond to intercept. For each exposure, the baseline group should be coded as 0, the first level as 1, and so on.
}
  \item{N}{
    A numeric vector providing the sample size for each row of the design matrix, \code{X}.
}
\item{strata}{
    A numeric vector indicating which columns of the design matrix, \code{X}, are used to form the phase I stratification variable. \code{strata=1} specifies the intercept and is, therefore, equivalent to a case-control study.
}
  \item{expandX}{
    Character vector indicating which columns of \code{X} to expand as a series of dummy variables. Useful when at least one exposure is continuous (and should not be expanded). Default is `all'; other options include `none' or character vector of column names. See Details, below.
  }
  \item{etaTerms}{
    Character vector indicating which columns of \code{X} are to be included in the model. See Details, below.
  }
  \item{nII0}{
    A vector of sample sizes at phase II for controls. The length must correspond to the number of unique values for phase I stratification variable.
}
  \item{nII1}{
    A vector of sample sizes at phase II for cases. The length must correspond to the number of unique values phase I stratification variable.
}
\item{cohort}{
    Logical flag. TRUE indicates phase I is drawn as a cohort; FALSE indicates phase I is drawn as a case-control sample.
}
\item{NI}{
    A pair of integers providing the outcome-specific phase I sample
    sizes when the phase I data are drawn as a case-control sample. The
    first element corresponds to the controls and the second to the
    cases.
  }
  \item{digits}{
    Integer indicating the precision to be used for the reporting of the (expected) sampling probabilities
}

}
\details{
  The correspondence between \code{betaTruth} and \code{X}, specifically the ordering of elements, is based on successive use of \code{\link{factor}} to each column of \code{X} which is expanded via the \code{expandX} argument. Each exposure that is expanded must conform to a {0, 1, 2, ...} integer-based coding convention.
  
  The \code{etaTerms} argument is useful when only certain columns in \code{X} are to be included in the model. In the context of the two-phase design, this might be the case if phase I stratifies on some surrogate exposure and a more detailed/accurate measure is to be included in the main model.
}
\value{
	\code{phaseI()} returns an object of class "phaseI" that, at a minimum includes:
	\item{phaseI}{Expected phase I counts, based on a pre-specified population and outcome model}
	\item{covm}{Model based variance-covariance matrix. This is available for \code{method} = "PL" and "ML".}
	\item{cove}{Empirical variance-covariance matrix. This is available for all the three methods.}
	\item{fail}{Indicator of whether or not the phase I and/or the phase II constraints are satisfied; only relevant for the ML estimator.}
	
		If phase II sample sizes were provided as part of the initial call, the object additionally includes:
	\item{phaseII}{The supplied phase II sample sizes.}
	\item{phaseIIprobs}{Expected phase II sampling probabilities.}
}
\references{
Haneuse, S. and Saegusa, T. and Lumley, T. (2011) "osDesign: An R Package for the Analysis, Evaluation, and Design of Two-Phase and Case-Control Studies." Journal of Statistical Software, 43(11), 1-29.
}
\author{
  Sebastien Haneuse, Takumi Saegusa
}
\examples{
##
data(Ohio)

## Design matrix that forms the basis for model and phase I 
## stata specification 
##
XM <- cbind(Int=1, Ohio[,1:3])      ## main effects only
XI <- cbind(XM, SbyR=XM[,3]*XM[,4]) ## interaction between sex and race

## 'True' values for the underlying logistic model
##
fitM <- glm(cbind(Death, N-Death) ~ factor(Age) + Sex + Race, data=Ohio,
            family=binomial)
fitI <- glm(cbind(Death, N-Death) ~ factor(Age) + Sex * Race, data=Ohio,
            family=binomial)

## Stratified sampling by race
##
phaseI(betaTruth=fitM$coef, X=XM, N=Ohio$N, strata=4,
       nII0=c(125, 125),
       nII1=c(125, 125))

## Stratified sampling by age and sex
##
phaseI(betaTruth=fitM$coef, X=XM, N=Ohio$N, strata=c(2,3))
##
phaseI(betaTruth=fitM$coef, X=XM, N=Ohio$N, strata=c(2,3),
       nII0=(30+1:6),
       nII1=(40+1:6))
}

