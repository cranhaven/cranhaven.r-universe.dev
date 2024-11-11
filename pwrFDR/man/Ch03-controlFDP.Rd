\name{controlFDP}
\alias{controlFDP}
\title{
  Helper function for the BHCLT FDP control method
}
\description{
  Helper function for the BHCLT FDP control method. Calculates
  a reduced FDR required to bound the the false discovery proportion
  in probability using asymptotic approximation.
}
\usage{
  controlFDP(effect.size, n.sample, r.1, alpha, delta, groups = 2, 
    N.tests, type, grpj.per.grp1, control, formula, data) 
}
\arguments{
  \item{effect.size}{
    The effect size (mean over standard deviation) for test statistics 
    having non-zero means. Assumed to be a constant (in magnitude) over
    non-zero mean test statistics.
  }
  \item{n.sample}{
    The number of experimental replicates.
  }
  \item{r.1}{
    The proportion of simultaneous tests that are non-centrally located 
  }
  \item{alpha}{
    The upper bound on the probability that the FDP exceeds delta.
  }
  \item{delta}{
    The exceedance thresh-hold for the FDP tail probability control method 
    (BHCLT or Romano) \eqn{P\{ FDP > \delta \} < \alpha}. The default 
    value is \eqn{\alpha}.
  }
  \item{groups}{
    The number of experimental groups to compare. Default value is 2.
  }
  \item{N.tests}{
    The number of simultaneous hypothesis tests. 
  }
  \item{type}{
    A character string specifying, in the groups=2 case, whether the
    test is 'paired', 'balanced', or 'unbalanced' and in the case when
    groups >=3, whether the test is 'balanced' or 'unbalanced'. The
    default in all cases is 'balanced'. Left unspecified in the one
    sample (groups=1) case. 
  }
  \item{grpj.per.grp1}{
    Required when \code{type}="unbalanced", specifies the group 0 to
    group 1 ratio in the two group case, and in the case of 3 or more
    groups, the group j to group 1 ratio, where group 1 is the group
    with the largest effect under the alternative hypothesis. 
  }
  \item{control}{
    Optionally, a list with components with the following components:
    'groups', used when distop=3 (F-dist), specifying number of groups. 
    'tol' is a convergence criterion used in iterative methods
          which is set to 1e-8 by default 
    'max.iter' is an iteration limit, set to 20 for the iterated
               function limit and 1000 for all others by default
    'distop', specifying the distribution family of the central and
              non-centrally located sub-populations. =1 gives normal (2
	      groups)=2 gives t- (2 groups) and =3 gives F- (2+ groups)    
    'CS', correlation structure, for use only with 'method="simulation"'
          which will simulate m simulatenous tests with correlations 'rho'
          in blocks of size 'n.WC'. Specify as list CS = list(rho=0.80,n.WC=50) 
	  for example.
    'sim.level' sim level 2 results in more detail at the expense of
                slightly more computational time.
    'low.power.stop' in simulation option, will result in an error message if
             the power computed via FixedPoint method is too low, which
             result in no solution for the BHCLT option. Default setting
	     is TRUE. Set to FALSE to over-ride.
    'FDP.meth.thresh' fine-tunes the 'Auto' voodoo (see above). Leave
                     this alone.
    'verb' vebosity level.
  }
  \item{formula}{Optionally, the function can be used to _estimate_ f*
    from a given dataset of sorted p-values.  In this case we specify
    formula, which is a formula of the form \code{pval~1} where 'pval'
    is the name of the p-value variable in the dataset, \code{dataset}
    (see 
  }
  \item{data}{
    The name of the dataset.
  }
}
\details{
  Uses a CLT for the FDP to calculate a reduced alpha required to
  bound the the false discovery rate in probability...e.g. finds
  alpha* so that when the BH-FDR procedure is controlled at alpha*,
  we ensure that

  Pr( V_m/R_m > (1-r) alpha ) < (1-r) alpha

  where 'alpha' is the original false discovery rate and
  'r' is the proportion of non-null distributed test
  statistics.
}
\value{
  \item{alpha.star}{The reduced alpha required to bound the FDP in probability}
  \item{obj}{Objective value at 'alpha.star'... should be close to 0}
  \item{L.star}{The bound on the FDP, should be (1-r) f.  See above.}
  \item{P.star}{The probability that the FDP is greater than L.star. See above.}
  \item{average.power}{Resulting average power.}
  \item{c.g}{The BH-FDR threshold on the scale of the test statistics.}
  \item{gamma}{The proportion of all 'm' tests declared significant.}
  \item{objective}{Result of optimization yielding the 'average.power'.}
  \item{err.III}{Mass on the wrong side of the threshold.}
  \item{sigma.rtm.SoM}{Asymptotic variance of the true positive fraction.}
}
\references{
  Izmirlian G. (2020) Strong consistency and asymptotic normality for
    quantities related to the Benjamini-Hochberg false discovery rate
    procedure. Statistics and Probability Letters; 108713,
    <doi:10.1016/j.spl.2020.108713>

  Izmirlian G. (2017) Average Power and \eqn{\lambda}-power in
    Multiple Testing Scenarios when the Benjamini-Hochberg False
    Discovery Rate Procedure is Used. <arXiv:1801.03989>
}
\author{
  Grant Izmirlian <izmirlian at nih dot gov>
}
\seealso{
  \code{\link{pwrFDR}}
}
\examples{
## at alpha=0.15 and other parameters, it takes n.sample=46 replicates for 
## average power > 80%
pwr.46.15 <- pwrFDR(alpha=0.15, r.1=0.03, N.tests=1000, effect.size=0.79, n.sample=46)

## when there are 'only' N.tests=1000 simultaneous tests, the distribution of the
## false discovery fraction, FDP, is not so highly spiked at the alpha=0.15
## You need to set the alpha down to alpha=0.0657 to ensure that  Pr( T/J > 0.145 ) < 0.0657
fstr <- controlFDP(alpha=0.15, r.1=0.03, N.tests=1000, effect.size=0.8, n.sample=46)

## at all the above settings, with alpha=0.0657 at an n.sample of 46, we only have 69% 
## average power.
pwr.46.0657 <- pwrFDR(alpha=0.065747, r.1=0.03, N.tests=1000, effect.size=0.79, n.sample=46)

## it'll cost 7 more replicates to get the average power up over 80%.
pwr.53.0657 <- pwrFDR(alpha=0.065747, r.1=0.03, N.tests=1000, effect.size=0.8, n.sample=53)

## it costs only 8.75% more to get it right!
}
\keyword{FDR}
\keyword{Benjamini}
\keyword{Hochberg}
\keyword{microarrays}
\keyword{Multiple.Testing}
\keyword{average.power}
\keyword{k.power}
\keyword{lambda.power}
