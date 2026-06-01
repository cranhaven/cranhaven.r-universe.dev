\name{scr}
\alias{scr}
\docType{data}
\title{Serum Creatinine data}
\description{
In clinical trials, various response variables of interest are measured repeatedly
over time on the same subject, which can be analysed by using
the mdhglm package. At the same time, an event time representing recurrent
or terminating time is also obtained. For example, consider a
clinical study to investigate the chronic renal allograft dysfunction in
renal transplants (Sung et al., 1998). The renal function was evaluated
from the serum creatinine (sCr) values. Since the time interval between
the consecutive measurements differs from patient to patient, we focus
on the mean creatinine levels over six months. In addition, a single terminating
survival time (graft-loss time), measured by month, is observed
from each patient. During the study period, there were 13 graft losses
due to the kidney dysfunction. For other remaining patients, we assumed
that the censoring occurred at the last follow-up time. Thus, the censoring
rate is about 88 percent.
}
\usage{data("scr")}
\format{
  A data frame with 1395 observations on the following 9 variables.
  \describe{
    \item{\code{id}}{subjects identifier}
    \item{\code{month}}{visiting time in months}
    \item{\code{cr}}{serum creatinine value}
    \item{\code{sex}}{gender (1=male, 0=female)}
    \item{\code{age}}{age in years}
    \item{\code{icr}}{1/cr}
    \item{\code{sur_time}}{graft loss time}
    \item{\code{status}}{=1 for occurrence of event, 0=censoring}
    \item{\code{first}}{indicator first visit or not (1=first visit, 0=oterwise)}
  }
}
\references{
Sung, K.H., Kang, K.W., Kang, C.M., Kwak, J.Y., Park, T.S. and
Lee, S.Y. (1998). Study on the factors affecting the chronic renal
allograft dysfunction. The Korean Journal of Nephrology, 17, 483--493.
}

