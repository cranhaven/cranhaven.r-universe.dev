\name{ordAOV}
\alias{ordAOV}
\title{ANOVA for factors with ordered levels}
\description{This function performs analysis of variance when the
factor(s) of interest has/have ordinal scale level. For testing, values from
the null distribution are simulated.}

\usage{
ordAOV(x, y, type = c("RLRT", "LRT"), nsim = 10000,
null.sample = NULL, ...)
}
\arguments{
  \item{x}{a vector or matrix of integers 1,2,... giving the observed levels
  of the ordinal factor(s). If \code{x} is a matrix, it is assumed that
  each column corresponds to one ordinal factor.}
  \item{y}{the vector of response values.}
  \item{type}{the type of test to carry out: likelihood ratio ("LRT") or
  restricted likelihood ratio ("RLRT").}
  \item{nsim}{number of values to simulate from the null distribution.}
  \item{null.sample}{a vector, or a list of vectors (in case of multi-factorial
  ANOVA) containing values already simulated from the null distribution
  (overrides \code{nsim})}
  \item{...}{additional arguments to \code{\link{LRTSim}} and
  \code{\link{RLRTSim}}, respectively.}
}
\details{
The method assumes that ordinal factor levels (contained in vector/columns of
matrix \code{x}) take values 1,2,...,max, where max denotes the highest level
of the respective factor observed in the data. Every level between 1 and max has
to be observed at least once.

The method uses a mixed effects formulation of the usual one- or multi-factorial
ANOVA model (with main effects only) while penalizing (squared) differences of
adjacent means. Testing for equal means across factor levels is done by
(restricted) likelihood ratio testing for a zero variance component in a linear
mixed model. For simulating values from the finite sample null
distribution of the (restricted) likelihood ratio statistic, the
algorithms implemented in Package \code{\link{RLRsim}} are used. See
\code{\link{LRTSim}} and \code{\link{RLRTSim}} for further information.

If \code{x} is a vector (or one-column matrix), one-factorial ANOVA is applied,
and it is simulated from the exact finite sample null distribution as derived by
Crainiceanu & Ruppert (2004). If \code{x} is a matrix, multi-factorial ANOVA
(with main effects only) is done, and the approximation of the finite sample null
distribution proposed by Greven et al. (2008) is used. Simulation
studies by Gertheiss (2014) suggest that for ANOVA with ordinal factors RLRT
should rather be used than LRT.
}

\value{
In case of one-factorial ANOVA, a list of class \code{htest} containing the
following components (see also \code{\link{exactLRT}} and \code{\link{exactRLRT}}):

  \item{statistic}{the observed (restricted) likelihood ratio.}
  \item{p}{p-value for the observed test statistic.}
  \item{method}{a character string indicating what type of test was performed
  and how many values were simulated to determine the critical value.}
  \item{sample}{the samples from the null distribution returned by
  \code{\link{LRTSim}} and \code{\link{RLRTSim}, respectively.}}
  
In case of multi-factorial ANOVA, a list (of lists) with the jth component
giving the results above when testing the main effect of factor j.
}

\references{
Crainiceanu, C. and D. Ruppert (2004). \emph{Likelihood ratio tests in linear
mixed models with one variance component}, Journal of the Royal Statistical
Society B, 66, 165-185.

Gertheiss, J. (2014). \emph{ANOVA for factors with ordered levels}, Journal of
Agricultural, Biological and Environmental Statistics, 19, 258-277.

Gertheiss, J. and F. Oehrlein (2011). \emph{Testing relevance and linearity of
ordinal predictors}, Electronic Journal of Statistics, 5, 1935-1959.

Greven, S., C. Crainiceanu, H. Kuechenhoff, and A. Peters (2008). \emph{Restricted
likelihood ratio testing for zero variance components in linear mixed models},
Journal of Computational and Graphical Statistics, 17, 870-891.

Scheipl, F., S. Greven, and H. Kuechenhoff (2008). \emph{Size and power of tests
for a zero random effect variance or polynomial regression in additive and linear
mixed models}, Computational Statistics & Data Analysis, 52, 3283-3299.
}

\author{Jan Gertheiss}


\seealso{\code{\link{LRTSim}}, \code{\link{RLRTSim}}}


\examples{
# load some data
data(ICFCoreSetCWP)

# the pysical health component summary
y <- ICFCoreSetCWP$phcs

# consider the first ordinal factor
x <- ICFCoreSetCWP[,1]

# adequate coding
x <- as.integer(x - min(x) + 1)

# ANOVA
ordAOV(x, y, type = "RLRT", nsim=1000000)
}

\keyword{anova}
