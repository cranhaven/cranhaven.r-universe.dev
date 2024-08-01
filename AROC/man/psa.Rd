\name{psa}
\alias{psa}
\docType{data}
\title{
Prostate specific antigen (PSA) biomarker study.
}
\description{
The dataset contains 71 prostate cases and 71 controls who participated in a lung cancer prevention trial (CARET, Beta-carotene and retinol trial). For details, see  Etzioni et al. (1999) and Pepe (2003).
}
\usage{data("psa")}
\format{
  A data frame with 683 observations on the following 6 variables.
  \describe{
    \item{\code{id}}{Patient identifier.}
    \item{\code{marker1}}{total prostate specific antigen (PSA).}
    \item{\code{marker2}}{free prostate specific antigen (PSA)}
    \item{\code{status}}{presence/absence of prostate cancer. The non-cancer patients are controls matched to cases on age and number of serum samples available for analysis (see Details).}
    \item{\code{age}}{patient age at blood draw (serum sample).}
    \item{\code{t}}{time (years) relative to prostate cancer diagnosis.}
  }
}
\details{
The CARET enrolled 12000 men, aged between 50 and 65 years, at high risk of lung cancer. For each subject on the study, serum samples were drawn at baseline and at two-year intervals after that. The data presented here represent a subsample of the original sample, and it was reported by Etzioni et al. (1999). It contains 71 cases of prostate cancer that occurred during the study. All these cases had, at least, three and up to eight serum samples. As far as controls are concerned, they were selected from the participants of the CARET study verifying that they had not been diagnosed with prostate cancer by the time of the original study, and the selection was done by matching to cases on date of birth and number of serum samples available for analysis.
}
\source{
The dataset can be downloaded from \url{https://research.fredhutch.org/diagnostic-biomarkers-center/en/datasets.html}.
}
\references{
Pepe, M. S. (2003). The Statistical Evaluation of Medical Tests for Classification and Prediction. Oxford Statistical Science Series. Oxford University Press, New York. 

Etzioni, R., Pepe, M. S., Longton, G., Hu. C., and Goodman, G. (1999). Incorporating the time dimension in receiver operating characteristic curves: A case study of prostate cancer. Medical Decision Making, 19(3), 242-251.
}
\examples{
data(psa)
summary(psa)
}
\keyword{datasets}
