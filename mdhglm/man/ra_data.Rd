\name{ra_data}
\alias{ra_data}
\docType{data}
\title{
Rheumatoid Arthritis Data
}
\description{
The Rheumatoid Arthritis Patients rePort Onset Re-activation sTudy
(RAPPORT study) is a longitudinal study that aims to identify an increase
in disease activity by self-reported questionnaires. A cohort of
159 patients is followed throughout one year. Self-reported questionnaires
are provided for patients every three months together with clinical
evaluations of patients disease status. Health assessment questionnaires
(HAQ) (20 questions from eight categories) and the Rheumatoid
Arthritis Disease Activity Index (RADAI) (five items, e.g., todays disease
activity in terms of swollen and tender joints) were used for patients
to self-report their functional status. We use binarized versions of HAQ
and RADAI, with cut-off points of 0.5 (HAQ) and 2.2 (RADAI). Lower
values indicate disease stability, while higher values show increased disease
activity.
A clinical examination was recorded using the disease activity score with
28 joint counts (DAS28), which is a composite score that includes for
example the swollen joints count. The DAS28 score varies between 0
and 10, and we assume Gaussian model to approximate its distribution.
Information about DAS28, RADAI, and HAQ taken at months 0, 3, 6,
9, and 12 were considered in this example. The analysis include gender
and baseline age of the patients as covariates.
}
\usage{data("ra_data")}
\format{
  A data frame with 2089 observations on the following 13 variables.
  \describe{
    \item{\code{id}}{subjects idenfier}
    \item{\code{sex}}{a factor for gender with levels \code{M} and \code{V}}
    \item{\code{age}}{age in year}
    \item{\code{yearsRA}}{years of RA}
    \item{\code{ethnic}}{a factor with levels \code{Dutch} and \code{Other}}
    \item{\code{edu}}{a factor with levels \code{Master/Bachelor} and \code{Other than Master/Bachelor}}
    \item{\code{X_NAME_}}{a factor with levels \code{das0}, \code{das12}, \code{das3}, \code{das6}, \code{das9}, \code{haq0}, \code{haq12}, \code{haq3}, \code{haq6}, \code{haq9}, \code{radai0}, \code{radai12}, \code{radai3}, \code{radai6} and \code{radai9}}
    \item{\code{X_LABEL_}}{a factor with levels \code{Das score (clinical) T=0}, \code{Das score (clinical) T=12}, \code{Das score (clinical) T=3}, \code{Das score (clinical) T=6}, \code{Das score (clinical) T=9}, \code{HAQ T=0}, \code{HAQ T=12}, \code{HAQ T=3}, \code{HAQ T=6}, \code{HAQ T=9}, \code{RADAI T=0}, \code{RADAI T=12}, \code{RADAI T=3}, \code{RADAI T=6} and \code{RADAI T=9}}
    \item{\code{outcome}}{outcome responses}
    \item{\code{outno}}{number for outcomes}
    \item{\code{outbin}}{outbin}
    \item{\code{outlabel}}{a factor with levels \code{remission} and \code{stable}}
    \item{\code{time}}{time in month}
  }
}

