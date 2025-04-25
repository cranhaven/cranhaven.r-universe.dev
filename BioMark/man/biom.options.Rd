\encoding{latin1}
\name{biom.options}
\alias{biom.options}
\title{Set or return options for stability-based biomarker selection}
\description{
  A function to set options for stability-based biomarker selection in
  the \pkg{BioMark} package, or to return the current options.
}
\usage{
biom.options(\dots, reset = FALSE)
}
\arguments{
  \item{\dots}{a single list, a single character vector, or any number
    of named arguments (\var{name = value}).}
  \item{reset}{logical: if TRUE all options are set to their factory
    defaults.}
}
\details{
  If called with no arguments, or with an empty list as the single
  argument, \code{biom.options} returns the current options.

  If called with a character vector as the single argument, a list with
  the arguments named in the vector are returned.

  If called with a non-empty list as the single arguments, the list
  elements should be named, and are treated as named arguments to the
  function.

  Otherwise, \code{biom.options} should be called with one or more named
  arguments \var{name = value}.  For each argument, the option named
  \var{name} will be given the value \var{value}.

  The options are saved in an envirtonment variable
  \code{.biom.Options}, and remain in effect until the end of the
  session.  If the environment is saved upon exit, they will be
  remembered in the next session.

  The recognised options are:
  \describe{
    \item{max.seg}{Maximal number of jackknife iterations. Default: 100.}
    \item{oob.size, oob.fraction}{Size of the out-of-bag fraction, either
      given as an absolute number (oob.size) or as a fraction. Default is
      to leave out ten percent. If oob.size is given explicitly, it takes
      precedence over oob.fraction. Default: oob.fraction = .1.}
    \item{variable.fraction}{Use 1 to always include all variables - use a
      smaller fraction to have a different random subset of all variables
      in each iteration (stability-based identification). Default: .7.}
    \item{ntop}{The number of "top" coefficients taken into account in
      stability-based biomarker identification. If a variable appears
      consistently among the \code{ntop} biggest coefficients, it is said
      to be stable. If ntop is a number between 0 and 1, it is taken to
      indicate the fraction of variables to be included in the model.
      Default: 10.}
    \item{min.present}{The minimal fraction of times a variable should be in
      the top list to be considered as a potential biomarker
      (stability-based identification). Setting this
      argument to 0 will lead to a list containing all coefficients that
      were present in the top list at least once - a value of 1 only
      returns those variables that are selected in every
      iteration. Default: .1.}
    \item{nset}{The number of permutations to establish null
      distributions for PCR, PLS and VIP statistics in the
      Higher-Criticism approach. Default: 10,000.}
    \item{fmethods}{All biomarker selection methods available within
      BioMark. Currently equal to \code{c("studentt", "shrinkt", "pcr",
	"pls", "vip", "lasso"}. } 
    \item{univ.methods}{The names of the univariate biomarker selection
      methods currently known to BioMark. Currently equal to
      \code{c("studentt", "shrinkt")}}
    \item{HCalpha}{The default of the alpha parameter in the HC
      method. Value: 0.1.}
    \item{lasso}{a list of arguments passed to the underlying
      \code{glmnet} function, such as \code{family}, \code{nlambda},
      \code{alpha}, \code{lambda}, or \code{lambda.min.ratio}. For
      binary classification, the "binomial" family is the default, but
      the most similar setting compared to the other methods in the package
      is \code{family = "gaussian"}. For choices other than the default,
      a warning is printed to the screen.}
  }
}
\section{Side Effects}{
  If any named argument (or list element) was provided,
  \code{biom.options} updates the elements of the option list
  \code{.biom.Options$options}.
}
\value{
  A list with the (possibly changed) options.  If any named argument (or
  list element) was provided, the list is returned invisibly.
}
\author{Ron Wehrens}
\seealso{\code{\link{glmnet}}}
\note{This function is based on the \code{\link[pls]{pls.options}}
  function in package \pkg{pls}.
}
\examples{
## Return current options:
biom.options()
biom.options("max.seg")

## Set options:
biom.options(max.seg = 100, oob.fraction = .2)
biom.options(lasso = list(alpha = .75, nlambda = 50))
biom.options()
## the next line removes some options - for these, glmnet defaults will be used
biom.options(lasso = list(alpha = .9, family = "binomial"))

## Restore factory settings:
biom.options(reset = TRUE)
}
\keyword{classification}
\keyword{multivariate}
