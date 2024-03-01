\name{fampower}
\alias{fampower}
\title{
  Simulation-based power calculation for genetic effect
}
\description{
  Computes the power of detecting genetic effect in the penetrance model based on a family-based simulation study.
}
\usage{
fampower(N.fam, N.sim, effectsize, beta.sex, alpha = 0.05, side = 2, design = "pop", 
variation = "none", interaction = FALSE, depend = NULL, base.dist = "Weibull", 
frailty.dist = NULL, base.parms, allelefreq = c(0.02, 0.2), dominant.m = TRUE, 
dominant.s = TRUE, mrate = 0, hr = 0, probandage = c(45, 2), agemin = 20, agemax = 100)
% \method{summary}{simulation}(object, \dots)
}

\arguments{
\item{N.fam}{
Number of families to generate.
}
\item{N.sim}{
Number of simulations.
}
\item{effectsize}{
Effect size of the major mutated gene (\code{beta.gene}) to detect under the alternative hypothesis. When \code{interaction=TRUE}, both the main and interaction effects should be specified, \code{effectsize = c(beta.gene, beta.int).}
}
\item{beta.sex}{
Gender effect that is fixed in the model.
}
\item{alpha}{
Significance level. Default value is 0.05.
}
\item{side}{
Number of sides for the alternative hypothesis. Possible choices are 1 for one-sided test and 2 for two-sided test. Default value is 2.
}
\item{design}{
Family based study design used in the simulations. Possible choices are: \code{"pop"}, \code{"pop+"}, \code{"cli"}, \code{"cli+"} or \code{"twostage"}, where \code{"pop"} is for the population-based design that families are ascertained by affected probands, \code{"pop+"} is similar to \code{"pop"} but with mutation carrier probands, \code{"cli"} is for the clinic-based design that includes affected probands with at least one parent and one sibling affected, \code{"cli+"} is similar to \code{"cli"} but with mutation carrier probands and \code{"twostage"} for two-stage design that randomly samples families from the population in the first stage and oversamples high risk families in the second stage that includes at least two affected members in the family. Default is \code{"pop"}.
}
\item{variation}{
Source of residual familial correlation.  Possible choices are: \code{"frailty"} for frailty shared within families, \code{"secondgene"} for second gene variation, or \code{"none"} for no residual familial correlation. Default is \code{"none"}.
}
\item{interaction}{
Logical; if \code{TRUE}, the interaction between gender and mutation status is allowed, otherwise no interaction is allowed. Default is \code{FALSE}.
}

\item{depend}{
Variance of the frailty distribution. Dependence within families increases with depend value.  Default value is \code{NULL}. Value \code{ > 0 } should be specified when \code{variation = "frailty"}. 
}

\item{base.dist}{
Choice of baseline hazard distribution.  Possible choices are: \code{"Weibull"}, \code{"loglogistic"}, \code{"Gompertz"}, \code{"lognormal"} \code{"gamma",} or \code{"logBurr"}. Default is \code{"Weibull"}.
}

\item{frailty.dist}{
Choice of frailty distribution.  Possible choices are: \code{"gamma"} for gamma distribution or \code{"lognormal"} for log normal distribution when \code{variation = "frailty"}. Default is \code{NULL}.
}
\item{base.parms}{
Vector of parameter values for baseline hazard function. 

\code{base.parms = c(lambda, rho)}, where lambda and rho are the shape and scale parameters, respectively. If \code{base.dist = "logBurr"} is chosen, three parameters should be specified for \code{base.parms = c(lambda, rho, eta)}. 
}
\item{allelefreq}{
Vector of population allele frequencies of major and second disease gene alleles. Frequencies must be between 0 and 1. Default frequencies are 0.02 for major gene allele and 0.2 for second gene allele, \code{allelefreq=c(0.02, 0.2)}.}
\item{dominant.m}{Logical; if \code{TRUE}, the genetic model of the major gene is dominant, otherwise recessive.}
\item{dominant.s}{Logical; if \code{TRUE}, the genetic model of the second gene is dominant, otherwise recessive.}
\item{mrate}{
Proportion of missing genotypes, value between 0 and 1. Default value is 0.}
\item{hr}{
Proportion of high risk families, which include at least two affected members, to be sampled from the two stage sampling. This value should be specified when \code{design = "twostage"}. Default value is 0. Value should lie between 0 and 1.
}
\item{probandage}{
Vector of mean and standard deviation for the proband age. Default values are mean of 45 years and standard deviation of 2 years, \code{probandage = c(45, 2)}.
}
\item{agemin}{
Minimum age of disease onset or minimum age. Default is 20 years of age.
}
\item{agemax}{
Maximum age of disease onset or maximum age. Default is 100 years of age.
}

}
\details{

The power of testing \eqn{H_0: \beta_{gene} = 0}{H0: bgene = 0 } vs. \eqn{H_1: \beta_{gene} = }{H1: bgene = } \code{effectsize} is obtained by the proportion of times the null hypothesis is rejected out of the \code{N.sim} simulations.

When \code{interaction = TRUE}, the powers of both the main effect of mutated gend and the interaction effect of mutated gene and gender will be computed.


}
\value{
  Returns 
  \item{power}{Power of detecting the genetic effect.}
  }

    \author{
    Yun-Hee Choi
    }
    \seealso{
    \code{\link{simfam}}
    }
    \examples{
    
## Example 1: obtain the power for testing the genetic effect 
# based on 50 POP families simulated using 100 simulations
\dontrun{
set.seed(4321)
fampower(N.fam = 50, N.sim = 100, effectsize = 1, beta.sex = 0.8, alpha = 0.05, side = 2, 
design = "pop+", variation = "none", base.dist = "Weibull", allelefreq = 0.02, 
base.parms = c(0.01, 3))}
    
## Example 2: obtain the power for both the main and interaction effects  
# based on 50 POP families simulated using 100 simulations
\dontrun{
set.seed(4321)
fampower(N.fam = 50, N.sim = 100, effectsize = c(1.5, 1), beta.sex = 0.8, alpha = 0.05, 
side = 2, interaction = TRUE,  design = "pop+", variation = "none", base.dist = "Weibull", 
allelefreq = 0.02, base.parms = c(0.01, 3))}
}
    \keyword{Simulation}