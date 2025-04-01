\name{InDisc}
\alias{InDisc}
\title{
A unified approach for obtaining and estimating unidimensional and multidimensional Item Response Theory (IRT) Dual Models (DMs).
}
\description{
A unified approach for obtaining and estimating unidimensional and multidimensional Item Response Theory (IRT) Dual Models (DMs) has been proposed by Ferrando (2019) and Ferrando & Navarro-Gonzalez (2020, submitted). DMs are intended for personality and attitude measures, are based on a Thurstonian response process, and are, essentially, extended standard IRT models with an extra person parameter that models the discriminating power of the individual. So, both items and individuals are considered as sources of measurement error in DMs.
}
\usage{
InDisc(SCO, nfactors = 1, nquad = 30, model = "linear", approp = FALSE, display = TRUE)
}
\arguments{
  \item{SCO}{
Raw sample scores.
  }
  \item{nfactors}{
Number of factors to be assessed, between 1 (default) and 4.
  }
  \item{nquad}{
The number of quadrature points for EAP estimation (default is 30).
  }
  \item{model}{
TThe model to be used: 'graded' (DTGRM) or 'linear' (DTCRM).
  }
  \item{approp}{
Determines if the appropiateness indices will be computed and printed in the console (logical variable, FALSE by default).
  }
  \item{display}{
Determines if the output will be displayed in the console, TRUE by default. If it is TRUE, the output is returned silently and if it is FALSE, the output is returned in the console.
  }
}
\details{
\code{InDisc} is based on the procedure proposed by Ferrando (2019) and Ferrando & Navarro-Gonzalez (2020, submitted) for estimating unidimensional and multidimensional Item Response Theory (IRT) Dual Models (DMs).Estimation is based on a two stage (calibration and scoring) random-regressors approach (McDonald, 1982). Item calibration at the first stage is the same as in the corresponding standard IRT models, is based on a factor-analytic Underlying-Variables approach, and uses an unweighted least squares, (ULS) minimum-residual criterion as implemented in the \code{\link{psych}} R package (Revelle, 2018). Individual trait scores and individual discriminations are obtained at the second stage using Expected a Posteriori (EAP) Bayes estimation. Overall, the combined ULS-EAP estimation procedure is simple, robust, and can handle large datasets, both in terms of sample size and test length.
}
\value{
  \item{INDIES}{
    Matrix including the theta scores, the PDDs, the PSDs (theta), the PSDs (PDD) and the reliabilities for the theta scores and the PDD for each participant.
  }
  \item{degrees_of_freedom}{
    Degrees of freedom for the model.
  }
  \item{Model_Chi_Square}{
    Chi Square statistic  for assessing model-data fit, with the indicated degrees of freedom.
  }
  \item{RMSR}{
    Root Mean Square of the Residuals.
  }
  \item{TLI}{
    Tucker Lewis Index goodness-of-fit index.
  }
  \item{RMSEA}{
    Root Mean Squared Error of Approximation.
  }
  \item{EVARI}{
    Average of the PDDs.
  }
  \item{reli_theta}{
    Marginal reliability of the trait estimates.
  }
  \item{aver_r_theta}{
    Average of the individual reliability (trait level).
  }
  \item{reli_PDD}{
    Marginal reliability of the PDD estimates.
  }
  \item{aver_r_PDD}{
    Average of the individual reliability (PDD).
  }
  \item{LR_stat}{
    Likelihood ratio statistic.
  }
  \item{Q_Chi_square}{
    Approximate Chi Square with N degrees of freedom based on the LRT_stat.
  }
}
\references{
Ferrando, P. J. (2019). A Comprehensive IRT Approach for Modeling Binary, Graded, and Continuous Responses With Error in Persons and Items. Applied Psychological Measurement, 43(5), 339-359. \doi{10.1177/0146621618817779}

Ferrando, P. J. & Navarro-Gonzalez, D.(2020). A multidimensional IRT model for continuous and graded responses with error in persons and items. Educational and Psychological Measurement (submitted).


McDonald, R. P. (1982). Linear versus models in item response theory. Applied Psychological Measurement, 6, 379-396. \doi{10.1177/014662168200600402}

Revelle, W. (2018) psych: P Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, \url{https://CRAN.R-project.org/package=psych} Version = 1.8.12.

}
\author{
Pere Joan Ferrando

David Navarro-Gonzalez
}
\examples{
## For speeding purposes, the number of observations and quadrature nodes have been
## reduced. For a proper use of InDisc, use the full dataset and the default quadrature
## nodes, and consider using the graded model.


InDisc(CTAC35[1:40,], nquad = 10, model = "linear", approp = FALSE, display = TRUE)
}
