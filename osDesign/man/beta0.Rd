\name{beta0}
\Rdversion{1.7}
\alias{beta0}
\title{
  Calculate the intercept of a logistic regression model, given a vector of log-odds ratio parameters and an overall prevalence.
}
\description{
When conducting power calculations, one is often interested in examining power for various 'effect sizes'. Suppose the logistic regression is specified via the vector of coefficients (\code{beta0}, \code{betaX}); the first element is the intercept and the second consists of a vector of log odds ratio parameters. In many settings, the overall outcome prevalence in the population of interest is known or, at least, fixed. Modifying any given element of \code{betaX} will automatically modify the overall prevalence, unless there is a corresponding change in \code{beta0}. The function \code{beta0()} calculates the value of \code{beta0} that minimizes the difference between the target outcome prevalence, \code{rhoY}, and prevalence induced by the model in conjuction with the assumed marginal exposure distribution.
}
\usage{
beta0(betaX, X, N, rhoY, expandX="all")
}
\arguments{
  \item{betaX}{
    Numeric vector of log-odds ratio parameters for the logistic regression model.
}
  \item{X}{
    Design matrix for the logistic regression model. The first column should correspond to intercept. For each exposure, the baseline group should be coded as 0, the first level as 1, and so on.
}
  \item{N}{
    A numeric vector providing the sample size for each row of the design matrix, \code{X}. 
}
  \item{rhoY}{
    Target outcome prevalence in the population.
}
  \item{expandX}{
    Character vector indicating which columns of \code{X} to expand as a series of dummy variables. Useful when at least one exposure is continuous (and should not be expanded). Default is `all'; the other option is `none' or character vector of column names.
  }
}
\details{
	The minimization is performed using the \code{\link{optimize}} function.
}
\value{
	Numeric value of the intercept parameter in a logistic regression model. 
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

## 
XM   <- cbind(Int=1, Ohio[,1:3])
fitM <- glm(cbind(Death, N-Death) ~ factor(Age) + Sex + Race, data=Ohio,
            family=binomial)

## Overall prevalence in the observed data
##
sum(Ohio$Death)/sum(Ohio$N)

## Intercept corresponding to the original vector of log-odds ratios
## 
fitM$coef
beta0(betaX=fitM$coef[-1], X=XM, N=Ohio$N, rhoY=sum(Ohio$Death)/sum(Ohio$N))

## Reduction of Sex effect by 50%
##
betaXm    <- fitM$coef[-1]
betaXm[3] <- betaXm[3] * 0.5
beta0(betaX=betaXm, X=XM, N=Ohio$N, rhoY=sum(Ohio$Death)/sum(Ohio$N))

## Doubling of Race effect
##
betaXm    <- fitM$coef[-1]
betaXm[4] <- betaXm[4] * 2
beta0(betaX=betaXm, X=XM, N=Ohio$N, rhoY=sum(Ohio$Death)/sum(Ohio$N))
}

