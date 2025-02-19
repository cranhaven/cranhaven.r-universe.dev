\encoding{UTF-8}
\name{psa}
\alias{psa}
\title{Prostate Cancer Diagnosis Data}
\description{
  Longitudinal measurements of two Prostate Specific Antigen
  (PSA)-based biomarkers for 71 prostate cancer cases and 70 controls. }
\usage{psa}
\format{
  The \code{psa} data frame has 683 rows and 6 columns:  
  \describe{
    \item{\code{id}}{
      patient id.
    }
    \item{\code{marker1}}{
      total PSA.
    }
    \item{\code{marker2}}{
      ratio of free total PSA.
    }
    \item{\code{status}}{
      disease status of each subject, with \code{1} identifying
      subjects diagnosed with prostate cancer.
    }
    \item{\code{age}}{
      age in years.
    }
    \item{\code{t}}{
      time prior to diagnosis.
    }
  }
}
\details{
  The data were gathered from the Beta-Carotone and Retinol Efficacy
  Trial (CARET)---a lung cancer prevention trial, conducted at the Fred
  Hutchison Cancer Research Center. Further details on this study can be
  found in de Carvalho \emph{et al.} (2020).
}
\references{
  de Carvalho, M., Barney, B. and Page, G. L. (2020)
  Affinity-based measures of biomarker performance evaluation.
  \emph{Statistical Methods in Medical Research}, \bold{20}, 837-853.
}
\keyword{Medical}
