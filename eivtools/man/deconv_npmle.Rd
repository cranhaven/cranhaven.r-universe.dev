\name{deconv_npmle}
\alias{deconv_npmle}

\title{Nonparametric MLE deconvolution under heteroskedastic normal error}

\description{Implements a version of the Rabe-Hesketh et al. (2003)
  algorithm for computing the nonparametric MLE of a univariate latent
  variable distribution from observed error-prone measures.  Allows for
  normal heteroskedastic measurement error with variance that depends on
  the latent variable, such as with estimates of latent ability from
  item response theory models.}

\usage{
deconv_npmle(W, csem,
gridspec = list(fixed=FALSE, xmin=-5, xmax=5, numpoints=2000),
lambda = 0.0005, lltol = 1e-7, psmall = 0.00005,
discrete = FALSE, quietly = FALSE)
}

\arguments{
  \item{W}{
    Vector of observed measures, where \code{W[i]} is assumed to
    be an error-prone measure of a latent variable \code{X[i]}
  }

  \item{csem}{
    A function of a single variable \code{x} that returns the
    conditional standard deviation of \code{W} given \code{X=x}.
    It needs to be able to accept a vector input and return a
    vector of the same length as the input.
  }

  \item{gridspec}{
    A named list specifying the grid over which the NPMLE will be
    computed.  It must have a logical component \code{fixed} indicating
    if the NPMLE grid is fixed (TRUE), or if the final grid is chosen by
    optimization over a candidate grid (FALSE).  The default is
    FALSE. The remaining components of \code{gridspec} specify the grid
    in one of two mutually exclusive ways.  In the first way,
    \code{gridspec} must contain elements \code{xmin} providing the
    minimum grid value, \code{xmax} providing the maximum grid value,
    and \code{numpoints} providing the desired number of points.  In
    this case, the grid will be \code{numpoints} equally-spaced values
    ranging from \code{xmin} to \code{xmax}.  In the second way,
    \code{gridspec} must contain an element \code{grid}, a numeric
    vector providing the actual desired grid.  It must be an arbitrary
    sequence of increasing numeric values with no missing values.
  }

  \item{lambda}{
    Step size, on probability scale, in Rabe-Hesketh et al. (2003)
    algorithm.  See reference for details.
  }
  
  \item{lltol}{
    Algorithm stops when the improvement to the log likelihood does not
    exceed \code{lltol.}
  }

  \item{psmall}{
    If a mass point in the estimated latent distribution evolves to have
    probability less than \code{psmall,} it gets dropped.
  }

  \item{discrete}{
    Not currently implemented.
  }

  \item{quietly}{
    If \code{FALSE} (the default), prints iteration-by-iteration
    progress.
  }
}

\details{
  The assumed model is \code{W = X + U} where the conditional
  distribution of \code{U} given \code{X = x} is assumed to be normal
  with mean zero and standard deviation \code{csem(x)}.  The function
  uses \code{W} to estimate a discrete latent distribution for \code{X}
  that maximizes the likelihood of the observed data.  The function
  optimizes the mass points (among a grid of candidate values) and the
  associated probabilities.

  In the special case of homoskedastic error, the function \code{csem}
  must be defined such that when passed a vector of length \code{n}, it
  returns a vector of length \code{n} where each element is a common
  constant.

  The default values of \code{xmin} and \code{xmin} in \code{gridspec}
  are generally appropriate only for a latent variable on a standardized
  scale with mean zero and variance one, and should be set to
  appropriate values given the scale of \code{W}.
}

\value{A list with elements

  \itemize{
    \item{gridspec: }{Information about the initial grid}
      
    \item{.history: }{Iteration-by-iteration evolution of the estimated
      distribution, if \code{gridspec$fixed} is FALSE.  Otherwise it is
      an empty list}
    
    \item{px: }{A dataframe providing the final NPMLE distribution.  There
      are as many rows as there are mass points in the estimated
      distribution; fields described below}
    
    \item{reliability: }{An estimate of the reliability of \code{W}, equal
      to the estimated variance of \code{X} divided by the sample variance
      of \code{W}}
    
    \item{simex_varfuncs: }{A dataframe with as many rows as there are
      unique values of \code{W}, providing estimated plug-in variance
      functions to use for SIMEX data generation with latent
      heteroskedastic error as described in Lockwood and McCaffrey
      (forthcoming); see references.  Fields described below} }
  
  The fields of \code{px} are:

  \itemize{
    \item{x: }{Location of mass point}
    
    \item{csem: }{Value of function \code{csem} at mass point}
    
    \item{p: }{probability at mass point}
    
    \item{ll: }{log likelihood at solution}
    
    \item{ex: }{Estimate of mean of latent distribution}
    
    \item{varx: }{Estimate of variance of latent distribution}
  }

  The fields of \code{simex_varfuncs} are:

  \itemize{
    \item{W: }{Unique observed values \code{w} of \code{W}}
    
    \item{gW: }{The square of \code{csem} evaluated at \code{W = w}}
    
    \item{gEXW: }{The square of \code{csem} evaluated at \code{E[X | W=w]}, the
      conditional mean of \code{X} given \code{W=w}}
    
    \item{EgXW: }{The conditional mean of the square of \code{csem} of
      \code{X} given \code{W=w}, equal to \code{E[g(X) | W=w]}}
  }
}

\references{
  Lockwood J.R. and McCaffrey D.F. (2014). \dQuote{Correcting for test
    score measurement error in ANCOVA models for estimating treatment
    effects,} \emph{Journal of Educational and Behavioral Statistics}
  39(1):22-52.

  Lockwood J.R. and McCaffrey D.F. (2017).
  \dQuote{Simulation-extrapolation with latent heteroskedastic
    variance,} \emph{Psychometrika} 82(3):717-736.
  
  Rabe-Hesketh S., Pickles A. and Skrondal A. (2003). \dQuote{Correcting
  for covariate measurement error in logistic regression using
  nonparametric maximum likelihood estimation,} \emph{Statistical
  Modelling} 3:215-232.
}

\author{
  J.R. Lockwood \email{jrlockwood@ets.org}
}

\seealso{
  \code{\link{testscores}},\code{\link{eivreg}}
}

\examples{
data(testscores)

## get the unique values of the lag 1 math score and CSEM
## values and approximate the CSEM function using approxfun()
tmp <- unique(testscores[,c("math_lag1","math_lag1_csem")])
print(tmp <- tmp[order(tmp$math_lag1),])

.csem <- approxfun(tmp$math_lag1, tmp$math_lag1_csem, rule=2:2)
plot(tmp$math_lag1, tmp$math_lag1_csem)
lines(tmp$math_lag1, .csem(tmp$math_lag1), col="blue")

## get NPMLE distribution of latent lag 1 math achievement
m <- deconv_npmle(W        = testscores$math_lag1,
                  csem     = .csem,
                  gridspec = list(fixed = FALSE,
                                  xmin = min(testscores$math_lag1),
                                  xmax = max(testscores$math_lag1),
                                  numpoints = 10000),
                  quietly  = TRUE)

print(m$px)

## estimated mean is approximately the mean of W, but
## the estimated variance is less than the variance of W,
## as it should be
print(c(empirical = mean(testscores$math_lag1),
        estimated = m$px$ex[1]))

print(c(empirical = var(testscores$math_lag1),
        estimated = m$px$varx[1]))

## estimated reliability of W:
print(m$reliability)

## if implementing SIMEX, simex_varfuncs provides plug-in
## options to use for the heteroskedastic error variance
## of each observed W
print(m$simex_varfuncs)

## simple "value-added" estimates of school effects on math,
## adjusting for measurement error in the lag 1 math score.
testscores$schoolid <- factor(testscores$schoolid)

meiv <- eivreg(math ~ math_lag1 + sped + frl + schoolid,
         data = testscores,
         reliability = c(math_lag1 = m$reliability),
         contrasts = list(schoolid = "contr.sum"))

print(summary(meiv))

## alternative deconvolution with fixed grid
m <- deconv_npmle(W        = testscores$math_lag1,
                  csem     = .csem,
                  gridspec = list(fixed = TRUE,
                                  xmin = min(testscores$math_lag1),
                                  xmax = max(testscores$math_lag1),
                                  numpoints = 40),
                  quietly  = TRUE)

print(m$px)

}

\keyword{models}
