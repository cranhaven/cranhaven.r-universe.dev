\name{dists}
\alias{dists}
\docType{data}
\title{
  The Distribution family object
}
\description{
  The \code{pwrFDR} package currently incorporates 3 distribution types,
  normal, t and F. The first two of these are strictly for statistics formed 
  from two group comparison while the third is for statistics formed from the 
  omnibus test of any difference among an arbitrary number of groups >=2. The
  structure is general and user expandable. One must specify the density, 
  CDF and quantile function for a given distribution and its parameters under
  the null and under the alternative. These parameters must be expressions
  to be evaluated inside the kernel of the power program, functions of the
  arguments \code{n.sample}, \code{groups} and \code{effect.size}. This is 
  not used directly by the user at all unless she (he) wants to add a 
  distribution type. 
}
\format{
  A data frame with 3 observations on the following 6 variables.
  \describe{
    \item{\code{pars0}}{a list vector having components 'c(nd, p1, p2, ...)'
                        where 'nd' is the distribution number starting with 0,
                        and p1, p2, ..., are paramters of the distribution,
                        which are functions of 'n.sample', 'groups' and 
                        'effect.size' as mentioned above.  These must be 
                        expressed as a call e.g.  
                        as.call(expression(c, nd, p1, p2, ...))  etc.
                        'pars0' are the parameters under the null.}
    \item{\code{pars1}}{a list vector. See directly above. Parameters under the 
                        alternative.}
    \item{\code{minv}}{a list vector with components given the values -Inf or 0,
                       which will be used to decide if the two sided corrections
                       are used or not.}
    \item{\code{ddist}}{a list vector with components set to functions, each one 
                       computing the probability density function corresponding
                       to the particular distribution. A function of
		       arguments 'x' and 'par'. See details below.}
    \item{\code{pdist}}{a list vector with components set to the functions, each
                        one computing the cumulative distribution function 
			corresponding to the particular distribution. A function
			of arguments 'x' and 'par'. See details below.}
    \item{\code{qdist}}{a list vector with components set to the functions, each
                        one computing the quantile function (inverse cumulative 
                        distribution function) corresponding to the particular 
                        distribution. A function of arguments 'x' and 'par'. 
                        See details below.}
  }
}
\details{
   \code{dists} is a data.frame with components \code{pars0}, \code{pars1}, 
   \code{minv}, \code{ddist}, \code{pdist}, and \code{qdist}. For the three 
   available distribution options, "normal", "t" and "f", the components
   \code{pars0} and \code{pars1} take the following form:\cr

\tabular{ll}{
1. pars0                              \tab  pars1                                    \cr
2. c(0,ncp=0,sd=1)                    \tab  c(0,ncp=.NCP.,sd=1)                      \cr
3. c(1,ncp=0,ndf=.DF.)                \tab  c(1,ncp=.NCP.,ndf=.DF.)                  \cr
4. c(2,ncp=0,ndf1=groups-1,ndf2=.DF.) \tab  c(2,ncp=.NCP.^2,ndf1=groups-1,ndf2=.DF.) \cr}
The component \code{minv} gives the minumum value of the support set of the
distribution. For the above named three available distribution options,
\code{minv} is set to the values -Inf, -Inf and 0, respectively. The components
\code{ddist}, \code{pdist}, and \code{qdist} contain functions defining the
density, CDF, and quantile function, respectively. For the above named three
available distribution options, \code{ddist} takes the following form:\cr
\tabular{l}{
1. ddist                                                             \cr
2. function (x, par) dnorm(x, mean = par[2], sd = par[3])            \cr
3. function (x, par) dt(x, ncp = par[2], df = par[3])                \cr
4. function (x, par) df(x, ncp = par[2], df1 = par[3], df2 = par[4]) \cr}

The components \code{pdist} and \code{qdist} are nearly identical to the
component \code{ddist}, but with pnorm, pt, pf and qnorm, qt, qf replacing
dnorm, dt and df, respectively.\cr
The variables, \code{.NCP.} and \code{.DF.} named above are defined within the
functions in which \code{ddist} is used based upon corresponding expressions,
\code{NCP} and \code{DF}. These expressions currently contain 3 component
expressions, one for each of the available test types, "paired", "balanced"
and "unbalanced".\cr
\code{NCP} is currently defined:\cr
\tabular{l}{
1. NCP                                                                  \cr
expression(n.sample^0.5*effect.size,(n.sample/groups)^0.5*effect.size,  \cr
   ((n.sample-1)/(1+sum((n.sample-1)/(nii.sample-1))))^0.5*effect.size) \cr}
and \code{DF} is currently defined:\cr
\tabular{l}{
1. DF                                                                   \cr
expression(n.sample - 1, groups * (n.sample - 1),                       \cr
           groups^2*(n.sample-1)/(1+sum((n.sample-1)/(nii.sample-1))))	\cr}
}
\source{
  This isn't 'data' data, its a kind of a 'family' object.

  Izmirlian G. (2020) Strong consistency and asymptotic normality for
    quantities related to the Benjamini-Hochberg false discovery rate
    procedure. Statistics and Probability Letters;
    <doi:10.1016/j.spl.2020.108713>
  
  Izmirlian G. (2017) Average Power and \eqn{\lambda}-power in
    Multiple Testing Scenarios when the Benjamini-Hochberg False
    Discovery Rate Procedure is Used. <arXiv:1801.03989>
}
