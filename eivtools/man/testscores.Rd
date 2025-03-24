\name{testscores}
\alias{testscores}
\docType{data}
\title{Example longitudinal test score data}
\description{
  Cohort of grade 6 students with mathematics test scores from a target
  school year, as well as mathematics and language test scores from the prior school
  year (grade 5). Measurement error in test scores quantified by conditional standard
  error of measurement (CSEM).
}
\usage{data(testscores)}
\format{
  A data frame with 4853 observations and 10 fields:
  \describe{
    \item{\code{stuid}}{Unique identifier for each student (one record per
      student)}
    \item{\code{schoolid}}{Unique identifier for each student's grade 
      6 school}
    \item{\code{math}}{Grade 6 mathematics test score}
    \item{\code{math_csem}}{CSEM for grade 6 mathematics test score}
    \item{\code{math_lag1}}{Grade 5 mathematics test score}
    \item{\code{math_lag1_csem}}{CSEM for grade 5 mathematics test score}
    \item{\code{lang_lag1}}{Grade 5 language test score}
    \item{\code{lang_lag1_csem}}{CSEM for grade 5 language test score}
    \item{\code{sped}}{1 = student designated as special education;
      0 otherwise}
    \item{\code{frl}}{1 = student participates in Free and Reduced Price
      lunch program;
      0 otherwise}
  }
}

\source{
  Anonymous
}

\keyword{datasets}
