\name{rXb}
\title{Generate Data Design Matrix \eqn{X} and Coefficient Vector \eqn{\beta}}
\alias{rXb}
\alias{rX}
\concept{generate datasets}
\description{
  Generate a random design matrix \eqn{X} and coefficient vector \eqn{\beta}
  useful for simulations of (high dimensional) linear models.
  In particular, the function \code{rXb()} can be used to exactly
  recreate the reference linear model datasets of the hdi paper.
}
\usage{
rXb(n, p, s0,
    xtype = c("toeplitz", "exp.decay", "equi.corr"),
    btype = "U[-2,2]",
    permuted = FALSE, iteration = NA, do2S = TRUE,
    x.par = switch(xtype,
                   "toeplitz"  = 0.9,
                   "equi.corr" = 0.8,
                   "exp.decay" = c(0.4, 5)),
   verbose = TRUE)

rX(n, p, xtype, permuted, do2S = TRUE,
   par = switch(xtype,
                "toeplitz"  = 0.9,
                "equi.corr" = 0.8,
                "exp.decay" = c(0.4, 5)))
}
\arguments{
  \item{n}{integer; the sample size \eqn{n} (paper had always \code{n = 100}).}
  \item{p}{integer; the number of coefficients in the linear
    model. (paper had always \code{p = 500}).}
  \item{s0}{integer number of \emph{nonzero} coefficients desired in the
    model; hence at most \code{p}.}
  \item{xtype}{a \code{\link{character}} string specifying the type of design matrix
    one wants to generate.  Must be one of \code{"toeplitz"},
    \code{"equi.corr"} or \code{"exp.decay"}.}
  \item{btype}{a \code{\link{character}} string specifying the type of
    nonzero coefficients (\dQuote{beta}) one wants to generate.  In the hdi
    paper, this has been one of
    "U[-2,2]", "U[0,2]", "U[0,4]", "bfix1", "bfix2" and "bfix10".  In
    general, any string of the form \code{"U[a,b]"} or \code{"bfix<c>"}
    is allowed, where \code{a}, \code{b}, and \code{<c>} must be numbers
    (with \eqn{a \le b}{a <= b}).}
  \item{permuted}{logical specifying if the columns of the design matrix
    should be permuted.}
  \item{iteration}{integer or \code{NA} specifying if seeds should be
    set to generate reproducible
    realizations of the design type and coefficients type. \code{NA}
    corresponds to not setting seeds. Iteration numbers 1 to 50
    correspond to the setups from the paper.
    If a seed is set, the original \code{\link{.Random.seed}} at the
    point of entering the function is saved and is restored upon exit of
    the data generation.
    If \code{NA}, the current \code{\link{.Random.seed}} is
    taken as usual in \R.}
  \item{do2S}{logical indicating if in the case of \code{xtype}s
    \code{"toeplitz"} or \code{"equi.corr"}, the \eqn{p \times p}{p * p}
    covariance matrix should be inverted twice.  Must be true, to
    regenerate the \eqn{X} matrices from the hdi paper exactly
    \dQuote{to the last bit}.}
  \item{x.par,par}{the parameters to be used for the design matrix.  Must be
    a numeric vector of length one or two.  The default uses the
    parameters also used in the hdi paper.}
  \item{verbose}{should the function give a message if seeds are being
    set? (logical).}
}
\details{
  \bold{Generation of the design matrix \eqn{X}:}
  \cr
  For all \code{xtype}'s, the \eqn{X} matrix will be multivariate
  normal, with mean zero and (co)variance matrix \eqn{\Sigma = }\code{C}
  determined from \code{xtype}, \code{x.par} and \eqn{p} as follows:
  \describe{
    \item{\code{xtype = "toeplitz"}:}{\code{C <- par ^ abs(toeplitz(0:(p-1)))}}
    \item{\code{xtype = "equi.corr"}:}{\eqn{\Sigma_{i,j} = \code{par}}
      for \eqn{i \ne j}{i != j}, and \eqn{= 1}
      for \eqn{i = j}, i.e., on the diagonal.}
    \item{\code{xtype = "exp.decay"}:}{\code{C <- solve(par[1] ^
	abs(toeplitz(0:(p-1)) / par[2]))}}
  }
}
\value{
  \describe{
    \item{For \code{rXb()}:}{A \code{\link{list}} with components
      \describe{
	\item{x}{the generated \eqn{n \times p}{n * p} design matrix \eqn{X}.}
	\item{beta}{the generated coefficient vector \eqn{\beta} (\sQuote{beta}).}}
    }
    \item{For \code{rX()}:}{the generated \eqn{n \times p}{n * p} design
      matrix \eqn{X}.}
  }
}
\references{
  Dezeure, R., \enc{Bühlmann}{Buhlmann}, P., Meier, L. and
  Meinshausen, N. (2015) 
  High-dimensional inference: confidence intervals, p-values and
  R-software hdi.
  \emph{Statistical Science} \bold{30}, 533--558.}
\author{Ruben Dezeure \email{dezeure@stat.math.ethz.ch}}
\examples{
## Generate the first realization of the linear model with design matrix
## type Toeplitz and coefficients type uniform between -2 and 2

dset <- rXb(n = 80, p = 20, s0 = 3,
            xtype = "toeplitz", btype = "U[-2,2]")
x <- dset$x
beta <- dset$beta

## generate 100 response vectors of this linear model
y <- as.vector( x \%*\% beta ) + replicate(100, rnorm(nrow(x)))

## Use  'beta_min' fulfilling  beta's  (non standard 'btype'):
str(ds2 <- rXb(n = 50, p = 12, s0 = 3,
               xtype = "exp.decay", btype = "U[0.1, 5]"))

## Generate a design matrix of type "toeplitz"
set.seed(3) # making it reproducible
X3 <- rX(n = 800, p = 500, xtype = "toeplitz", permuted = FALSE)

## permute the columns
set.seed(3)
Xp <- rX(n = 800, p = 500, xtype = "toeplitz", permuted = TRUE)
}
\keyword{datagen}
\keyword{regression}
