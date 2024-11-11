\name{cCDF.ToM}
\alias{cCDF.ToM}
\title{
  Computes the complimentary CDF for the true positive proportion, T_m/M_m.
}
\description{
  Computes the complimentary CDF for the true positive proportion, T_m/M_m
  via asymptotic approximation. Included here mainly for pedagogic purposes. 
}
\usage{
cCDF.ToM(u, effect.size, n.sample, r.1, alpha, delta, groups = 2, N.tests,
         type = c("paired", "balanced", "unbalanced"), grpj.per.grp1 = NULL,
         FDP.control.method = "BHFDR", control = list(tol = 1e-08,
         max.iter = c(1000, 20), distopt = 1, CS = list(NULL), sim.level = 2,
         low.power.stop = TRUE, FDP.meth.thresh = FDP.cntl.mth.thrsh.def, verb = FALSE))
}
\arguments{
  \item{u}{
    A sorted vector of values on the interval [0, 1] for which the cCDF
    of T_m/M_m should be computed.
  }
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
    The false discovery rate (in the BH case) or the upper bound on the
    probability that the FDP exceeds delta (Romano case)
  }
  \item{delta}{
    If the "FDP.control.method" is set to 'Romano' or 'BHCLT', then the
    user can set the exceedance thresh-hold for the FDP tail probability
    control \eqn{P\{ FDP > \delta \} < \alpha}. The default value is
    \eqn{\alpha}.
  }
  \item{groups}{
    The number of experimental groups to compare. Must be integral and
    >=1. The default value is 2.
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
  \item{FDP.control.method}{
    A character string specifying how the false discovery proportion (FDP) is to be
    controlled. You may specify the whole word or any shortened uniquely
    identifying truncation. \cr
     "BHFDR": the usual BH-FDR \cr
     "BHCLT": use asymptotic approximation to the distribution of the FDP 
              to find a smaller FDR which guarantees probability less
              than alpha that the FDP exceeds alpha. \cr
    "Romano": use Romano's method which guarantees probability less than 
              alpha that the FDP exceeds alpha. \cr
      "Auto": in 'FixedPoint' mode, the program will use its own 
              wisdom to determine which choice above to make. The 
              order of conservatism is Romano > BHCLT > BHFDR, but 
              BHFDR offers only expected control while the other two 
              guarantee bounds on the excedance probabilty. If the 
	      distribution of the FDP is nearly degenerate, then BHFDR 
	      is the best option. Otherwise, if it can be reliably used, 
              BHCLT would be the best choice. The 'effective' denominator, 
	      gamma*N.tests, in the CLT determines when the approximation 
	      is good enough and the asymptotic standard error of the FDP 
	      determines when the distribution is dispersed enough to matter.
              Use "Auto" to run through these checks and determine the best. 
              A return argument, 'Auto', displays the choice made. See 
              output components and details. \cr
      "both": in 'simulation' mode, compute statistics R and T under BHCLT
              and Romano (in addition to BHFDR). Corresponding
	      statistics are denoted R.st, T.st corresponding to BHCLT
	      control of the FDP, and R.R and T.R corresponding to
	      Romano control of the FDP. If sim.level is set to 2,
	      (default) the statistics R.st.ht and T.st.ht, which are
	      the number rejected and number true positives under BHCLT
	      where r_0 = 1-r_1, gamma, and alpha.star have been estimated 
	      from the P-value data and then alpha.star computed from
              these.
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
  \item{cCDF.ToM}{A data frame with columns \code{u} and \code{cCDF.ToM}
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
  \code{\link{cCDF.Rom}}
  \code{\link{cCDF.VoR}}
  \code{\link{pwrFDR}}
}
\examples{
  library(pwrFDR)

  u <- seq(from=0,to=1,len=100000)
  rslt <- cCDF.ToM(u=u, effect.size=0.9, n.sample=70, r.1=0.05, alpha=0.15, N.tests=1000,
                   FDP.control.method="Auto")

  ## plot the result
  with(rslt$cCDF.ToM, plot(u, cCDF.ToM, type="s"))

  ## compute the mean and median as a check 
  DX <- function(x)c(x[1], diff(x))
  .mean. <- with(rslt$cCDF.ToM, sum(cCDF.ToM*DX(u)))
  .median. <- with(rslt$cCDF.ToM, u[max(which(cCDF.ToM>0.5))])
}
\keyword{FDR}
\keyword{Benjamini}
\keyword{Hochberg}
\keyword{microarrays}
\keyword{Multiple.Testing}
\keyword{average.power}
\keyword{k.power}
\keyword{lambda.power}
