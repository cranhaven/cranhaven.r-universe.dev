\name{CDF.Pval.HA}
\alias{CDF.Pval.HA}
\title{
  CDF of p-values for test statistics distribted under HA.
}
\description{
  Computes the CDF of p-values for test statistics distribted under HA.
}
\usage{
CDF.Pval.HA(u, effect.size, n.sample, r.1, groups = 2, type="balanced",
            grpj.per.grp1=1, control)
}
\arguments{
  \item{u}{
    Argument of the CDF. Result will be Pr( P_i <= u )
  }
  \item{effect.size}{
    The effect size (mean over standard deviation) for test statistics 
    having non-zero means. Assumed to be a constant (in magnitude) over
    non-zero mean test statistics.
  }
  \item{n.sample}{
    The number of experimental replicates.
  }
  \item{r.1}{
    The proportion of all test statistics that are distributed under HA.
  }
  \item{groups}{
    The number of experimental groups to compare. Default value is 2.
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
    'max.iter' is an iteration limit, set to 20 for function iteration
     and 1000 for all others by default
    'distop', specifying the distribution family of the central and
      non-centrally located sub-populations. =1 gives normal (2 groups)
      =2 gives t- (2 groups) and =3 gives F- (2+ groups)    
  }
}
\details{
  Computes the CDF of p-values for test statistics distribted under HA.
  If Fc_0 is the cCDF of a test statistic under H0 and Fc_A is the cCDF
  of a test statistic under HA then the CDF of a P-value for a test
  statistic distributed under HA is
  
  G_A(u) = Fc_A(Fc_0^{-1}(u))

  The limiting true positive fraction is the infinite simultaneous tests
  average power,

  lim_m T_m/M_m = average.power (a.s.),

  which is used to approximate the average power for finite 'm', is
  G_1 at gamma alpha:

  G_1( gamma alpha) = average.pwer

  where alpha is the nominal FDR and gamma = lim_m R_m/m (a.s.) is the limiting
  positive call fraction.
}
\value{
  A list with components
  \item{call}{The call which produced the result}
  \item{u}{The argument that was passed to the function}
  \item{CDF.Pval.HA}{The value of the CDF}
}
\references{
  Izmirlian G. (2020) Strong consistency and asymptotic normality for
    quantities related to the Benjamini-Hochberg false discovery rate
    procedure. Statistics and Probability Letters; 108713,
    <doi:10.1016/j.spl.2020.108713>.  
  
  Izmirlian G. (2017) Average Power and \eqn{\lambda}-power in
    Multiple Testing Scenarios when the Benjamini-Hochberg False
    Discovery Rate Procedure is Used. <arXiv:1801.03989>
    
  Genovese, C. and L. Wasserman. (2004) A stochastic process approach to
   false discovery control. Annals of Statistics. 32 (3), 1035-1061.
}
\author{
  Grant Izmirlian <izmirlian at nih dot gov>
}
\seealso{
  \code{\link{CDF.Pval}}
}
\examples{
  ## First calculate an average power for a given set of parameters
  rslt.avgp <- pwrFDR(effect.size=0.79, n.sample=42, r.1=0.05, alpha=0.15)

  ## Now verify that G_A( gamma f ) = average.power

  gma <- rslt.avgp$gamma
  alpha <- rslt.avgp$call$alpha

  GA.gma.alpha <- CDF.Pval.HA(u=gma*alpha, r.1=0.05, effect.size=0.79, n.sample=42)

  c(G.gm.alpha=GA.gma.alpha$CDF.Pval.HA$CDF.Pval.HA, average.power=rslt.avgp$average.power)
}
\keyword{FDR}
\keyword{Benjamini}
\keyword{Hochberg}
\keyword{microarrays}
\keyword{Multiple.Testing}
\keyword{average.power}
\keyword{k.power}
\keyword{lambda.power}
