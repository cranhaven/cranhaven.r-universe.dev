\name{stroke}
\alias{stroke}
\docType{data}
\title{Stroke Patients Data
}
\description{
Approximate 30 percent of hospitalized patients due to acute ischemic stroke
are placed in the risk of early neurologic deterioration (END) at their
hospital stay. About the patients with acute ischemic stroke, real-time
tracking of END would be actualized with monitoring the individual
risk assumption with continuous blood pressure (BP). This data-set has systolic BP (SBP) 
observed repeatedly after arriving at emergency room for two stroke patients
(one is END; the other is non-END).
}
\usage{data("stroke")}
\format{
  A data frame with 72 observations on the following 3 variables.
  \describe{
    \item{\code{time}}{observed time}
    \item{\code{y1}}{SBP for END patient}
    \item{\code{y2}}{SBP for non-END patient}
  }
}

