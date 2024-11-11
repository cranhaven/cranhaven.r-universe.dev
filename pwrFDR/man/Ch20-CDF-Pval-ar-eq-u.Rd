\name{CDF.Pval.ar.eq.u}
\alias{CDF.Pval.ar.eq.u}
\title{
  Calculates the fixed point for the Romano procedure.
}
\description{
  Calculates the fixed point for the Romano procedure, e.g. finds
  u which solves u = G( psi(u, d) a) where G is the common
  p-value CDF, and psi(u, d) = u d/(1 - (1-a) u). Essentially an
  internal function and included at the user level for pedagogic
  purposes.
}
\usage{
  CDF.Pval.ar.eq.u(effect.size, n.sample, r.1, alpha, delta, groups, type,
                   grpj.per.grp1, control)
}
\arguments{
  \item{effect.size}{
    The effect size (mean over standard deviation) for test statistics 
    having non-zero means. Assumed to be a constant (in magnitude) over
    non-zero mean test statistics.
  }
  \item{n.sample}{
    The number of experimental replicates. Required for calculation
    of power
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
    The number of experimental groups to compare. Must be integral and
    >=1. The default value is 2.
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
  \item{control}{Optionally, a list with components with the following  
                 components: \cr
           'tol' is a convergence criterion used in iterative 
   	         methods which is set to 1e-8 by default.
	   'max.iter' is an iteration limit, set to 20 for the iterated
	         function limit and 1000 for all others by default.
	   'distop', specifying the distribution family of the central
	         and non-centrally located sub-populations. distopt=1
		 gives normal (2 groups), distop=2 gives t- (2 groups)
		 and distopt=3 gives F- (2+ groups) \cr
	   'CS', correlation structure, for use only with 'method="simulation"'  
	         which will simulate m simulatenous tests with correlations 
                 'rho' in blocks of size 'n.WC'. Specify as a list
		 CS = list(rho=0.80,n.WC=50) for example.  \cr
	   'sim.level' sim level 2 (default) stipulates, when FDP.control.method
	         is set to "BHCLT", or "both", R.st.ht and T.st.ht are
	         computed in addition to R.st and T.st (see above). \cr
           'low.power.stop' in simulation option, will result in an error message 
   	         if the power computed via FixedPoint method is too low, which 
	         result in no solution for the BHCLT option. Default setting is TRUE. 
	         Set to FALSE to over-ride this behavior. \cr
           'FDP.meth.thresh' fine-tunes the 'Auto' voodoo (see above). Leave 
	         this alone. \cr
		 'verb' vebosity level.
  }
}
\value{
  An object of class \code{cdf} which contains components
  \item{call}{The call which produced the result
  }
  \item{gamma}{The fixed point for the Romano method.
  }
}
\references{
  Izmirlian G. (2020) Strong consistency and asymptotic normality for
    quantities related to the Benjamini-Hochberg false discovery rate
    procedure. Statistics and Probability Letters; 108713,
    <doi:10.1016/j.spl.2020.108713>.

  Izmirlian G. (2017) Average Power and \eqn{\lambda}-power in
    Multiple Testing Scenarios when the Benjamini-Hochberg False
    Discovery Rate Procedure is Used. <arXiv:1801.03989>

  Jung S-H. (2005) Sample size for FDR-control in microarray data
    analysis. Bioinformatics; 21:3097-3104.
  
  Liu P. and Hwang J-T. G. (2007) Quick calculation for sample size while
    controlling false discovery rate with application to microarray
    analysis. Bioinformatics; 23:739-746.

  Lehmann E. L., Romano J. P.. Generalizations of the familywise error
    rate. Ann. Stat.. 2005;33(3):1138–1154.

  Romano Joseph P., Shaikh Azeem M.. Stepup procedures for control of
    generalizations of the familywise error rate. Ann. Stat..
    2006;34(4):1850-1873. 
}
\author{
  Grant Izmirlian <izmirlian at nih dot gov>
}
\seealso{
  \code{\link{CDF.Pval.ua.eq.u}}
}
\examples{
  ## An example showing that the Romano method is more conservative than the BHCLT method
  ## which is in turn more conservative than the BH-FDR method based upon ordering of the
  ## significant call proportions, R_m/m

  ## First find alpha.star for the BH-CLT method at level alpha=0.15
  a.st.BHCLT <-controlFDP(effect.size=0.8,r.1=0.05,N.tests=1000,n.sample=70,alpha=0.15)$alpha.star

  ## now find the significant call fraction under the BH-FDR method at level alpha=0.15
  gamma.BHFDR <- CDF.Pval.ua.eq.u(effect.size = 0.8, n.sample = 70, r.1 = 0.05, alpha=0.15)

  ## now find the significant call fraction under the Romano method at level alpha=0.15
  gamma.romano <- CDF.Pval.ar.eq.u(effect.size = 0.8, n.sample = 70, r.1 = 0.05, alpha=0.15)

  ## now find the significant call fraction under the BH-CLT method at level alpha=0.15
  gamma.BHCLT <- CDF.Pval.ua.eq.u(effect.size = 0.8, n.sample = 70, r.1 = 0.05, alpha=a.st.BHCLT)
}
\keyword{FDR}
\keyword{Benjamini}
\keyword{Hochberg}
\keyword{microarrays}
\keyword{Multiple.Testing}
\keyword{average.power}
\keyword{k.power}
\keyword{lambda.power}
