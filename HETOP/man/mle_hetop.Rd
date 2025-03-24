\name{mle_hetop}
\alias{mle_hetop}

\title{Maximum Likelihood Estimation of Heteroskedastic Ordered Probit (HETOP) Model}

\description{
  Computes MLEs of \code{G} group means and standard deviations using
  count data from \code{K} ordinal categories under a heteroskedastic
  ordered probit model.  Estimation is conducted conditional on two
  fixed cutpoints, and additional constraints on group parameters are
  imposed if needed to achieve identification in the presence of sparse
  counts.
}

\usage{
mle_hetop(ngk, fixedcuts, svals=NULL, iterlim = 1500, ...)
}

\arguments{
  \item{ngk}{
    Numeric matrix of dimension \code{G x K} in which column \code{k} of
    row \code{g} indicates the number of units from group \code{g}
    falling into category \code{k}.
  }

  \item{fixedcuts}{
    A vector of length 2 providing the first two cutpoints, to identify
    the location and scale of the group parameters.  Note that this
    suffices for any \code{K >= 3}.
  }

  \item{svals}{
    Optional vector of starting values.  Its length is \code{2G + (K-3)}
    when no groups have sparse counts that affect identifiability;
    otherwise it must be smaller.  See Details.
  }

  \item{iterlim}{
    Maximum number of iterations used in optimization (passed to
    \code{\link{nlm}}).
  }

  \item{...}{
    Any other arguments for \code{\link{nlm}}.
  }
}

\details{
  This function requires \code{K >= 3}.  If \code{ngk} has all nonzero
  counts, all model parameters are identified.  Alternatively, arbitrary
  identification rules are required to ensure the existence of the MLE
  when there are one or more groups with nonzero counts in fewer than
  three categories.  This function adopts the following rules.  For any
  group with nonzero counts in fewer than three categories, the log of
  the group standard deviation is constrained to equal the mean of the
  log standard deviations for the remaining groups.  Further constraints
  are imposed to handle groups for which all data fall into either the
  lowest or highest category.  Let \code{S} be the set of groups for
  which it is not the case that all data fall into an extreme category.
  Then for any group with all data in the lowest category, the mean for
  that group is constrained to be the minimum of the group means over
  \code{S}.  Similarly, for any group with all data in the highest
  category, the mean for that grou is constrained to be the maximum of
  the group means over \code{S}.

  The location and scale of the group means are identified for the
  purpose of conducting the estimation by fixing two of the cutpoints.
  However in practice it may be desirable to express the group means and
  standard deviations on a scale that is more easily interpreted; see
  Reardon et al. (2017) for details.  This function reports estimates on
  four different scales: (1) the original estimation scale with two
  fixed cutpoints; (2) a scale defined by forcing the group means and
  log group standard deviations each to have weighted mean of zero,
  where weights are proportional to the total count for each group; (3)
  a scale where the population mean of the latent variable is zero and
  the population standard deviation is one; and (4) a scale similar to
  (3) but where a bias correction is applied.  See Reardon et al. (2017)
  for details on this bias correction.

  The function also returns an estimated intracluster correlation (ICC)
  of the latent variable, defined as the ratio of the between-group
  variance of the latent variable to its marginal variance.  Scales
  (1)-(3) above lead to the same estimated ICC; scale (4) uses a
  bias-corrected estimate of the ICC which will not in general equal the
  estimate from scales (1)-(3).
}

\value{
  A list with the following components:

  \item{est_fc}{A list of estimated group means, group standard
    deviations, cutpoints and ICC on scale (1).}

  \item{est_zero}{A list of estimated group means, group standard
    deviations, cutpoints and ICC on scale (2).}

  \item{est_star}{A list of estimated group means, group standard
    deviations, cutpoints and ICC on scale (3).}

  \item{est_starbc}{A list of estimated group means, group standard
    deviations, cutpoints and ICC on scale (4).}
  
  \item{nlmdetails}{The object returned by \code{\link{nlm}} that
    summarizes detailed of the optimization.}

  \item{pstatus}{A dataframe, with one row for each group, summarizing
    the estimation status of the mean and standard deviation for each
    group.  A value of \code{est} means that the parameter was estimated
    without constraints.  A value of \code{mean}, used for the group
    standard deviations, indicates that the parameter was constrained.
    Values of \code{min} or \code{max}, used for the group means,
    indicate that the parameter was constrained.}
}

\references{
  Reardon S., Shear B.R., Castellano K.E. and Ho A.D. (2017).
  \dQuote{Using heteroskedastic ordered probit models to recover moments
    of continuous test score distributions from coarsened data,}
  \emph{Journal of Educational and Behavioral Statistics} 42(1):3--45.

  Lockwood J.R., Castellano K.E. and Shear B.R. (2018).
  \dQuote{Flexible Bayesian models for inferences from coarsened,
    group-level achievement data,}
  \emph{Journal of Educational and Behavioral Statistics.} 43(6):663--692.
}

\author{
  J.R. Lockwood \email{jrlockwood@ets.org}
}

\examples{
set.seed(1001)

## define true parameters
G         <- 10
mug       <- seq(from= -2.0, to= 2.0, length=G)
sigmag    <- seq(from=  2.0, to= 0.8, length=G)
cutpoints <- c(-1.0, 0.0, 0.8)

## generate data with large counts
ng   <- rep(100000,G)
ngk  <- gendata_hetop(G, K = 4, ng, mug, sigmag, cutpoints)
print(ngk)

## compute MLE and check parameter recovery:
m    <- mle_hetop(ngk, fixedcuts = c(-1.0, 0.0))
print(cbind(true = mug,       est = m$est_fc$mug))
print(cbind(true = sigmag,    est = m$est_fc$sigmag))
print(cbind(true = cutpoints, est = m$est_fc$cutpoints))

## estimates on other scales:
p    <- ng/sum(ng)
print(sum(p * m$est_zero$mug))
print(sum(p * log(m$est_zero$sigmag)))

print(sum(p * m$est_star$mug))
print(sum(p * (m$est_star$mug^2 + m$est_star$sigmag^2)))

## dealing with sparse counts
ngk_sparse <- matrix(rpois(G*4, lambda=5), ncol=4)
ngk_sparse[1,] <- c(5,8,0,0)
ngk_sparse[2,] <- c(0,10,10,0)
ngk_sparse[3,] <- c(12,0,0,0)
ngk_sparse[4,] <- c(0,0,0,10)
print(ngk_sparse)

m    <- mle_hetop(ngk_sparse, fixedcuts = c(-1.0, 0.0))
print(m$pstatus)
print(unique(m$est_fc$sigmag[1:4]))
print(exp(mean(log(m$est_fc$sigmag[5:10]))))
print(m$est_fc$mug[3])
print(min(m$est_fc$mug[-3]))
print(m$est_fc$mug[4])
print(max(m$est_fc$mug[-4]))
}

\keyword{models}
