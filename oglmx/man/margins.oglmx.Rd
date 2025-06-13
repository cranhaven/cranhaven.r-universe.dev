\name{margins.oglmx}
\alias{margins.oglmx}
\alias{print.margins.oglmx}
\alias{mfx.oglmx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate marginal effects for \code{oglmx} objects.
%%  ~~function to do ... ~~
}
\description{
This function constructs marginal effects and calculates standard errors for all models estimated by the \code{oglmx} function. Standard errors are obtained using the delta method.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
margins.oglmx(object, atmeans = TRUE, AME = FALSE, location = NULL, outcomes = "All",
              ascontinuous = FALSE, Vars = NULL)

\method{print}{margins.oglmx}(x, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    object of class "\code{oglmx}".
%%     ~~Describe \code{object} here~~
}
  \item{Vars}{
    vector specifying variables for which marginal effects are desired.
%%     ~~Describe \code{Vars} here~~
}
  \item{outcomes}{
  either character string "\code{All}", the default option, or a numeric vector indicating the outcomes for which the marginal effect is desired. 
%%     ~~Describe \code{outcomes} here~~
}
  \item{atmeans}{
    logical. If \code{TRUE} then the marginal effects are calculated at the means of the variables in the equations for the mean and variance of the latent variable.
%%     ~~Describe \code{atmeans} here~~
}
  \item{AME}{
  logical. If \code{TRUE} the marginal effects are averaged across observations.
%%     ~~Describe \code{AME} here~~
}
  \item{ascontinuous}{
  logical. If \code{TRUE} binary variables are treated as if continuous to calculate marginal effects.
%%     ~~Describe \code{ascontinuous} here~~
}
%  \item{dummyzero}{
%  logical. If \code{atmeans=TRUE} and \code{dummyzero=TRUE} marginal effects are calculated with continuous variables at their means and binary variables set to zero.
%%     ~~Describe \code{dummyzero} here~~
%}
  \item{location}{
  \code{NULL}, a numeric vector, or a list containing two numeric vectors. Allows the user to specify the values of the explanatory variables at which the marginal effect is to be calculated. For a homoskedastic model the input should be a numeric vector of length equal to the number of variables in the model matrix. For a heterskedastic model the input should be a list, the first element should be a vector of length equal to the number of variables in the mean equation and the second is a vector of length equal to the number of variables in the variance equation.
%%     ~~Describe \code{location} here~~
}
  \item{\dots}{
    additional arguments to \code{print} method. Currently ignored.
  }
  \item{x}{object of class \code{margins.oglmx}.}
  
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
an object of class \code{margins.oglmx}. The object consists of a list containing data matrices, each matrix corresponding to an outcome for which the marginal effect was desired. Columns of each matrix correspond to the estimated marginal effect, its standard error, t-statistics and two sided p-value. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Nathan Carroll, \email{nathan.carroll@ur.de}
%%  ~~who you are~~
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
