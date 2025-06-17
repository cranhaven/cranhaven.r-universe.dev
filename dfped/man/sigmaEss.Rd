\name{sigmaEss}
\alias{sigmaEss}

\title{
The variance of the effective sample size (ESS).
}

\description{
Let \eqn{\pi_{ESS}(\alpha)} be the prior normal distribution \eqn{\mathcal{N} (\mu_\alpha, \sigma^{2}_{\alpha,ESS})}.
The variance \eqn{\sigma^{2}_{\alpha,ESS}} was fixed such that the information introduced by the prior would be equivalent to the information introduced by a
fixed number of patients, which was calibrated to control the amount of information. This approach is based on the effective sample size (ESS): the higher the ESS, the more informative the prior. For an ESS \eqn{m^{*}}, parameters \eqn{(\mu_\alpha, \sigma^{2}_{\alpha,ESS})} were chosen such that \deqn{min_{m} \delta(m, \mu_\alpha, \sigma^{2}_{\alpha,ESS})) = m^{*}}
}

\usage{
sigmaEss(mStar, sigma, Mmin, Mmax, meana, c, wm, Tmc)
}

\arguments{
    \item{mStar}{
        The number of patients anticipated for the trial.
    }
    \item{sigma}{
        The vector of sigma.
    }
    \item{Mmin}{
        The minimum number of patients for which the effective sample size (ESS) is computed.
    }
    \item{Mmax}{
        The maximum number of patients for which the effective sample size (ESS) is computed.
    }
    \item{meana}{
        Mean value of the prior distribution (known or chosen).
    }
    \item{c}{
        The maximum number of iteration for the algorithm to compute the ESS. See references for more details.
    }
    \item{wm}{
        The working model.
    }
    \item{Tmc}{
        The number of draw in the normal distribution in the ESS algorithm. See references for more details.
    }
}

\references{
Petit, C., et al, (2016) Unified approach for extrapolation and bridging of adult information in early phase dose-finding paediatric studies, Statistical Methods in Medical Research, <doi:10.1177/0962280216671348>.
    
Morita S., Thall P.F., and Muller P. (2008) Determining the effective sample size of a parametric prior. Biometrics.

Morita S. (2011) Application of the continual reassessment method to a phase I dose-finding trial in japanese patients: East meets west. Stat. Med.
} 

\author{
    Artemis Toumazi \email{artemis.toumazi@gmail.com},
    Caroline Petit \email{caroline.petit@crc.jussieu.fr},
    Sarah Zohar \email{sarah.zohar@inserm.fr} 
}


\examples{
\dontrun{
    wm_mat <- c(0.10, 0.21, 0.33, 0.55, 0.76 )
    wm_allo <- c(0.13, 0.27, 0.48, 0.70, 0.88)
    wm_linear <- c(0.07, 0.13, 0.21, 0.33, 0.55)
    c <- 10000
    meana <- 0.88
    Tmc <- 100000
    Mmax <- 30
    Mmin <- 1
    sigma_vect <- seq(0.1, 2, by = 0.01)
    mStar <- 30
    sigmaEss(mStar, sigma_vect, Mmin, Mmax, meana, c, wm_mat, Tmc)
}
}

