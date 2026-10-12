\name{Contraceptives}
\alias{Contraceptives}
\docType{data}
\title{Drug Interaction with Contraceptives}
\description{
Drug interaction study with oral contraceptives
}
\format{
  A data frame with 44 observations on the following 6 variables.
  \describe{
    \item{\code{ID}}{ID number for each of the women}
    \item{\code{StudyPeriod}}{\code{1=first} or \code{2}=second}
    \item{\code{Treatment}}{\code{Drug} or \code{Placebo}}
    \item{\code{EE}}{Bioavailability of the ethinyl estradiol  component of the oral contraceptive (in pg*hr/ml)}
    \item{\code{ComparisonValues}}{Comparison values used for a Tukey nonadditivity plot}
    \item{\code{Residuals}}{Residuals used for a Tukey nonadditivity plot}
  }
}
\details{
Twenty-two female subjects were allocated randomly to one of two treatment sequences in a two period crossover design.  The two treatments were a new Drug D or placebo, both given concomitantly with a standard oral contraceptive which was given in both study periods.  The oral contraceptive has two components, ethinyl estradiol (EE) and norethindrone (NET).  The purpose of the study was to evaluate whether the presence of Drug D affected the bioavailability of each of the oral contraceptive components.  Note that our dataset does not include the NET variable.
}
\source{
Thomas E. Bradstreet & Deborah L. Panebianco (2017) "An Oral Contraceptive Drug Interaction Study", Journal of Statistics Education, 12:1, DOI: 10.1080/10691898.2004.11910719


}
\keyword{datasets}
