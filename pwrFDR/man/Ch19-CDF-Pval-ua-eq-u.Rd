\name{CDF.Pval.ua.eq.u}
\alias{CDF.Pval.ua.eq.u}
\title{
  Function which solves the implicit equation u = G( u alpha) 
}
\description{
  Function which solves the implicit equation u = G( u alpha) where G is
  the pooled P-value CDF and alpha is the FDR
}
\usage{
  CDF.Pval.ua.eq.u(effect.size, n.sample, r.1, alpha, groups, type, 
                   grpj.per.grp1, control)
  }
\arguments{
  \item{effect.size}{
    The per statistic effect size
  }
  \item{n.sample}{
    The per statistic sample size
  }
  \item{r.1}{
    The proportion of Statistics distributed according to the alternative distribution
  }
  \item{alpha}{
    The false discovery rate.
  }
  \item{groups}{
    Number of experimental groups from which the test statistic is calculated
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
    'max.iter' is an iteration limit, set to 1000 by default
    'distop', specifying the distribution family of the central and
      non-centrally located sub-populations. =1 gives normal (2 groups)
      =2 gives t- (2 groups) and =3 gives F- (2+ groups)    
    'CS', correlation structure, for use only with 'method="simulation"'
      which will simulate m simulatenous tests with correlations 'rho'
      in blocks of size 'n.WC'. Specify as list
      CS = list(rho=0.80, n.WC=50) for example      
  }
}
\value{
  A list with a single component, 
  \item{gamma}{
    The solution of the implicit equation u = G( u alpha), where G is
    the pooled P-value CDF. This represents the infinite tests limiting
    proportion of hypothesis tests that are called significant by the
    BH-FDR procedure at alpha.
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
}
\author{
  Grant Izmirlian <izmirlian at nih dot gov>
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
