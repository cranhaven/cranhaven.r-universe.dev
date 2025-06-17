\name{ND}
\alias{ND}

\docType{data}
\title{an NEHANSE data example}

\description{
\code{ND} is an example data set extracted from National Health and Nutrition Examination Survey (NHANSE). The data set contains 100 samples and IDs and upper and lower bounds for 22 chemical compounds, including 7 dioxins, 9 furans, and 6 PCBs.
}

\usage{data(ND)}

\format{
  A data frame with 1643 observations and 45 variables. Variables contain
    \code{SEQN}: ID; \code{t1_TCDD}: lower bound for dioxin TCDD; \code{t2_TCDD}: upper bound for dioxin TCDD; \dots
    \code{t1_PCB_189}: lower bound for PCB_189 and \code{t2_PCB_189}: upper bound for PCB_189.
}


\references{
Yanming Li, Kerby Shedden, Brenda W. Gillespie and John A. Gillespie (2016). Calculating Profile Likelihood Estimates of the Correlation Coefficient in the Presence of Left, Right or Interval Censoring and Missing Data.
}


\examples{
   data(ND)
}

\keyword{dataset}
