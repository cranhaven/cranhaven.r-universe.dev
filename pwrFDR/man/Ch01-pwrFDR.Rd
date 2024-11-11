\name{pwrFDR}
\alias{pwrFDR}
\title{
  Ensemble power or sample size under selected control of the FDP 
}
\description{
  This is a function for calculating two differing notions of power, or deriving sample
  sizes for specified requisite power in multiple testing experiments under a variety of methods for
  control of the distribution of the False Discovery Proportion (FDP). More specifically, one can
  choose to control the FDP distribution according to control of its (i) mean, e.g. the usual BH-FDR
  procedure, or via the probability that it exceeds a given value, delta, via (ii) the Romano
  procedure, or via (iii) my procedure based upon asymptotic approximation. Likewise, we can think of
  the power in multiple testing experiments in terms of a summary of the distribution of the
  True Positive Proportion (TPP). The package will compute power, sample size or any other missing
  parameter required for power based upon (i) the mean of the TPP which is the average power
  (ii) the probability that the TPP exceeds a given value, lambda, via my asymptotic approximation
  procedure. The theoretical results are described in Izmirlian, G. (2020), and an applied paper 
  describing the methodology with a simulation study is in preparation.
}
\usage{
  pwrFDR(effect.size, n.sample, r.1, alpha, delta=NULL, groups=2, N.tests,
         average.power, tp.power, lambda, type=c("paired","balanced","unbalanced"),
         grpj.per.grp1=NULL, FDP.control.method=c("BHFDR","BHCLT","Romano","Auto","both"),
         method=c("FixedPoint", "simulation"), n.sim=1000, temp.file,
         control=list(tol=1e-8, max.iter=c(1000,20), distopt=1, CS=list(NULL),sim.level=2,
         low.power.stop=TRUE, FDP.meth.thresh=FDP.cntl.mth.thrsh.def, verb=FALSE,
         ast.le.a=TRUE))
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
    The false discovery rate (in the BH case) or the upper bound on the
    probability that the FDP exceeds delta (BHCLT and Romano case)
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
  \item{average.power}{
    The desired average power. Sample size calculation requires
    specification of either 'average.power' or 'tp.power'.
  }
  \item{tp.power}{
    The desired tp-power (see details for explanation). Sample
    size calculation requires specification of either 'average.power'
    or 'tp.power'.
  }
  \item{lambda}{
    The tp-power threshold, required when calculating the tp-power
    (see details for explanation) or when calculating the sample size
    required for tp-power.    
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
  \item{method}{Specify the method whereby the average power is calculated. 
                You may specify the whole word or any unqiuely indentifying  
                truncation.\cr
          "FixedPoint": use the fixed point method, e.g., 
	        first find the solution to the equation u = G(alpha u) where 
	        G is the CDF of the pooled P-values. This solution gives 
   	        'gamma', the positive proportion. The average power and other 
	        quantities are then determined e.g. 
	        average.power = G_1( gamma alpha), where G_1 is the CDF of the 
	        P-values corresponding to statistics drawn under H_A. \cr
          "simulation": uses brute force simulation to determine the average power.
                NOTE: the "FixedPoint" technique is approximate in that it 
                results in large sample values.
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
	   'verb' vebosity level.\cr
	   'ast.le.a' leaving this at the default value TRUE forces
	         'alpha.star', the solution under FDP.method.control="BHCLT",
		 to be less than the specified 'alpha'.
  }
  \item{n.sim}{If 'simulation' method is chosen you may specify number of
  	      simulations. Default is 1000.
  }
  \item{temp.file}{If 'simulation' method is chosen you may specify a tempfile 
              where the current simulation replicate is updated. Very 
              usefull for batch runs. You can use the included utility
              'gentempfilenm'
  }
}
\details{This function will compute one of a variety of ensemble powers
         under a given choice of FDP control methods. The underlying
	 model assumes that the m simultaneous test statistics are
	 i.i.d., each being formed from k samples which can be paired
	 (k=2), balanced or unbalanced (k>=2), k=1,2,..., and distributed
	 according to one of the available relevent distribution types
	 (see above). The location parameter for each of the statistical
	 tests is either 0 (null hypothesis) or a specified constant
	 effect size (alternative hypothesis), with the identity of these
	 two possibilities in each of the m cases being an i.i.d. unmeasured 
	 latent bernouli variable with density r.1, the mixing
	 proportion. The m simultaneous statistical tests partition into
	 those which are distributed according to the alternative,
	 numbering M_m, and those distributed according to the NULL,
	 numbering m-M_m. Once a selected thresholding method is
	 applied, the m statistics can also be partitioned into those
	 which are called significant, numbering R_m, and those which
	 are not, numbering m-R_m. Each of the test statistics is thus
	 given two labels, alternative hypothesis membership and whether
	 a significant call was made. Of the R_m significant calls, T_m
	 are true positives and V_m are false positives. This results in
	 the following table.  
         \tabular{rrccc}{
         1. \tab             \tab rej H0  \tab acc H0	         \tab row Total \cr
         2. \tab H0 is FALSE \tab T_m     \tab M.1-T             \tab	 M_m    \cr
         3. \tab H0 is TRUE  \tab R_m-T_m \tab (m-M_m)-(R_m-T_m) \tab  m - M_m  \cr
         4. \tab col Total   \tab R       \tab m-R_m	         \tab	 m      \cr}

         The ratio of the false positive count to the significant call
         count, V_m/R_m, is called the False Discovery Proportion
         (FDP). Thresholding methods which result in the most 
         reproducibility seek to control the FDP distribution. The most
         well known is the Benjamini-Hochberg False Discovery Rate
         (BH-FDR) procedure. It guarantees that the FDR, which is the
         expected FDP, will be less than a stipulated alpha

         \eqn{E[ V_m / R_m ] < \alpha}

         While it is true that for large m, the distribution of the FDP,
         V_m/M_m will become spiked at its mean, \eqn{(1-r_1)\alpha}, in
         many commonly occuring situations, there will still be
         non-negligible dispersion in the distribution of the FDP. For
         this reason, any validity promised by the BH-FDR procedure does
         not actually apply on a case to case basis, and individual FDP's
         may differ non-negligibly from the FDR. For this reason, the
         function supplies two other methods of FDP control in addition to
         FDP.control.method="BHFDR". These two alternate methods,
         FDP.control.method="Romano" and FDP.control.method="BHCLT"
         guarantee control of the tail probability of the FDP
         distribution:
       
         \eqn{P\{ V_m/R_m > \delta \} < \alpha}

         The lower bound \eqn{\delta} is left arbitrary for greater
         flexibility, \eqn{\delta=\alpha} being the default. There is
         also an automatic option, FDP.control.method="Auto", which lets
         the function decide which of the three FDP control methods is
         the most advisable in a given situation. The two tail
         probability control options are preferred when the standard
         error of the FDP exceeds a cutoff given in the default
         'control' settings:
	 
         se[V_m/R_m] / alpha > FDP.cntl.mth.thrsh.def[1]
  
         The default is 10\%. When the standard error to alpha ratio is
         10\% or less then the BHFDR, being the least conservative, is
         preferred. When the se to alpha ratio is 10\% or more, then
         Romano and BHCLT are decided between, with the BHCLT (asymptotic
         approximation) being less conservative than Romano and therefor
         preferred if the CLT approximation is adequate. This will be the
	 case provided \eqn{m} is large enough,

	 \eqn{m \geq} FDP.cntl.mth.thrsh.def[2]. The default is 50. 
             
         The concept of ensemble power for the purposes of this function,
         concern the distribution of the true positive proportion (TPP),
         \eqn{T_m/M_m}. The most well known is the average power, which
         is the expected value of the TPP, which is called the true
         positve rate (TPR):

         \eqn{E[ T_m/M_m ] = average power}

         For large m, the distribution of the TPP will be spiked at its
	 mean, which is the asymptotic average power. This is used in
	 the function in the average power computation. As was the case
	 for the FDP, there are many commonly occuring situations when
	 the distribution of the TPP will still be non-negligibly
	 dispersed. For this reason, we provide an alternate notion of
	 power which is based upon the tail probability of the TPP
	 distribution: 

         \eqn{P\{ T_m/M_m > \lambda \} = tp-power}

         This is computed via asymptotic approximation and also requires
         that \eqn{m} be large enough: \eqn{m > 50}. The user decides
	 when the tp-power is to be preferred. A good check is to look
	 at the ratio of the se[TPP] to average power ratio which is the
	 sigma.rtm.TPP/average.power/N.tests^0.5 If this ratio is
         unacceptibly large (10\% or so) than the tp-power is preferred.
	 
	 For the "FixedPoint" method (default) and for any specified choice 
	 of FDP.control method, the function can be used in the following ways:

	 1. Specify 'n.sample', 'effect.size', 'r.1' and 'alpha'. Calculates
	 'average power'

	 2. Specify 'n.sample', 'effect.size', 'r.1', 'alpha' and 'lambda'. 
	 'N.tests' is also required. The function wil calculate the 'tp.power'
	 in addition to the 'average power'.

	 3. Specify the 'average.power' or the pair 'tp.power' and 'lambda'. 
 	 Specify all but one of the parameters, 'n.sample', 'effect.size', 'r.1' 
	 and 'alpha'. The function will calculate the value of the missing 
	 parameter required for the specified 'average power' or 'tp-power'. 
	 Note: a solution is guaranteed for missing 'n.sample' and missing
	 'effect.size', but not necessarily for missing 'r.1' or 'alpha'. 
}
\value{
  An object of class "pwr" with with components including:
  \item{call}{The call which produced the result}
  \item{average.power}{Resulting average power.}
  \item{tp.power}{When 'lambda' is specified, the tp-power is also computed}
  \item{L.eq}{The lambda at which the tp-power and average-power are equal.}
  \item{n.sample}{If 'n.sample' is missing from the argument list, then
    the sample size required for the specified average- or lambda-
    power.}
  \item{alpha.star}{If 'FDP.control.method' was set to "BHCLT" or it
    resulted from the "Auto" setting, the alpha at which the probability
    that the FDP exceeds alpha.star is less than or equal to the
    originally specified alpha.} 
  \item{c.g}{The FDP control method threshold on the scale of the test statistics.}
  \item{gamma}{The proportion of all 'm' tests declared significant.}
  \item{objective}{Result of optimization yielding the average or tp- power.}
  \item{err.III}{Mass on the wrong side of the threshold.}
  \item{sigma.rtm.ToM}{Asymptotic standard deviation of the true positive
    fraction.}
  \item{Auto}{If 'FDP.control.method' was set to "Auto", this returns
    the resulting choice (a string) which was made internally.}
  \item{se.by.a}{The ratio of the standard error of the FDP to alpha,
    the nominal FDR, which gives an indication of the dispersion of its
    distribution relative to the nominal FDP. Used by the "Auto"
    specification.}
  \item{gma.Ntsts}{the effective denominator in the CLT asymptotic
    approximation to the distribution of the FDP, which equals
    the positive proportion, 'gamma', times the number of simultaneous
    tests, 'm'.}
  \item{detail}{The extractor function, \code{\link{detail}}, will
    return simulation replicates. See the linked documentation}
} 
\references{
  Izmirlian G. (2020) Strong consistency and asymptotic normality for
    quantities related to the Benjamini-Hochberg false discovery rate
    procedure. Statistics and Probability Letters; 108713,
    <doi:10.1016/j.spl.2020.108713>

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
  \code{\link{pwrFDR.grid}}
  \code{\link{controlFDP}}
}
\examples{
## Example 1a: average power

   rslt.avgp <- pwrFDR(effect.size=0.79, n.sample=46, r.1=2000/54675, alpha=0.15)
   rslt.avgp

## Example 1b: average power, FDP.control.method set to "Auto", N.tests=1000

   rslt.avgp.auto <- pwrFDR(effect.size = 0.79, n.sample = 46, r.1 = 2000/54675, alpha = 0.15, 
                            N.tests = 1000, FDP.control.method = "Auto")
   rslt.avgp.auto

## Example 1c: average power, FDP.control.method set to "Auto", N.tests=2000

   rslt.avgp.auto <- update(rslt.avgp.auto, N.tests = 2000)
   rslt.avgp.auto

## Example 1d: tp-power

   rslt.lpwr <- pwrFDR(effect.size=0.79, n.sample=46, r.1=2000/54675,
                       alpha=0.15, lambda=0.80, N.tests=54675)
   rslt.lpwr

## Example 1e: sample size required for given average power

   rslt.ss.avgp <- pwrFDR(effect.size=0.79, average.power=0.82,
                          r.1=2000/54675, alpha=0.15)
   rslt.ss.avgp

## Example 1f: sample size required for given tp-power

   rslt.ss.lpwr <- pwrFDR(effect.size=0.79, tp.power=0.82, lambda=0.80,
                          r.1=2000/54675, alpha=0.15, N.tests=54675)
   rslt.ss.lpwr

## Example 1g: simulation

   rslt.sim <- update(rslt.avgp, method="sim", n.sim=500, N.tests=1000)
   rslt.sim

## Example 1h: simulation

   rslt.sim <- update(rslt.avgp, method="sim", FDP.control.method="both",
                      n.sim=500, N.tests=1000)
   rslt.sim

## Example 2: methods for adding, subtracting, multiplying, dividing, exp, log,
## logit and inverse logit

   rslt.avgp - rslt.sim
   logit(rslt.avgp)       ## etc
   
## Example 3: Compare the asymptotic distribution of T/M with kernel
##            density estimate from simulated data 

   pdf <- with(detail(rslt.sim)$reps, density(T/M1))

   med <- with(detail(rslt.sim)$reps, median(T/M1))
   avg <- rslt.sim$average.power
   sd <- rslt.sim$se.ToM

   rng.x <- range(pdf$x)
   rng.y <- range(c(pdf$y, dnorm(pdf$x, mean=avg, sd=sd)))

   plot(rng.x, rng.y, xlab="u", ylab="PDF for T/M", type="n")
   with(pdf, lines(x, y))
   lines(rep(rslt.sim$average.power, 2), rng.y, lty=2)
   lines(pdf$x, dnorm(pdf$x, mean=avg, sd=sd), lty=3)
}
\keyword{FDR}
\keyword{Benjamini}
\keyword{Hochberg}
\keyword{microarrays}
\keyword{Multiple.Testing}
\keyword{average.power}
\keyword{k.power}
\keyword{lambda.power}
