\encoding{UTF-8}
\name{lungcancer}
\alias{lungcancer}
\title{Lung Cancer Diagnosis}
\description{
  The \code{lungcancer} data frame has 241 rows and 3 columns. The data
  were gathered gathered from a case-control study, conducted at the
  Mayo Clinic in Rochester (Minnesota), which included 140 controls and
  101 lung cancer cases; only woman have been enrolled in the study.
}
\usage{lungcancer}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{marker}}{: 
      square root of sEGFR levels (soluble isoform of the epidermal
      growth factor receptor).
    }
    \item{\code{status}}{: 
      disease status, with \code{1} identifying lung cancer cases and
      \code{0} identifying controls.
    }
    \item{\code{pre}}{: 
      premonopausal indicator, with \code{1} identifying premonopausal
      women.
    }
    \item{\code{age}}{: 
      age in years.     
    }
  }
}
\references{
  Inácio de Carvalho, V., Jara, A. and de Carvalho, M. (2015)
  Bayesian nonparametric approaches for ROC curve inference.
  In: \emph{Nonparametric Bayesian Methods in Biostatistics and
    Bioinformatics}. Eds R. Mitra and P. Mueller. Cham: Springer.
}
\keyword{Medical}
