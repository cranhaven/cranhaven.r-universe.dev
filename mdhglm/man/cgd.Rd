\name{cgd}
\alias{cgd}
\docType{data}
\title{CGD Infection Data}
\description{
The CGD data set presented by Fleming and Harrington (1991) and
Therneau and Lumley (2015) consists
of a placebo-controlled randomized trial of gamma interferon (rIFN-g)
in the treatment of chronic granulomatous disease (CGD). 128 patients
from 13 centers were tracked for around 1 year.
The number (i.e., cluster size) of patients per center ranged from 4 to
26. The survival times are the recurrent infection times
of each patient from the different centers. Censoring occurred at the
last observation for all patients, except one, who experienced a serious
infection on the data he left the study; in the CGD study about 63 per
cent of the data were censored. The recurrent infection times for a given
patient are likely to be correlated. However, each patient belongs to one
of the 13 centers; hence, the correlation may also be attributed to a
center effect.
}
\usage{data("cgd")}
\format{
  A data frame with 203 observations on the following 16 variables.
  \describe{
    \item{\code{id}}{subjects identifier for 128 patients}
    \item{\code{center}}{enrolling center for 13 centers}
    \item{\code{random}}{date of randomization}
    \item{\code{treat}}{a factor with levels \code{placebo} and \code{rIFN-g} (gamma interferon)}
    \item{\code{sex}}{a factor with levels \code{male} and \code{female}}
    \item{\code{age}}{age in years at study entry}
    \item{\code{height}}{height in cm at study entry}
    \item{\code{weight}}{weight in kg at study entry}
    \item{\code{inherit}}{a factor for pattern of inheritance with levels \code{X-linked} and \code{autosomal}}
    \item{\code{steroids}}{use of steroids at study entry with 1 (=yes) and 0(=no) }
    \item{\code{propylac}}{use of prophylactic antibiotics at study entry}
    \item{\code{hos.cat}}{a factor for centers into 4 goups with levels \code{US:NIH}, \code{US:other}, \code{Europe:Amsterdam} and \code{Europe:other}}
    \item{\code{tstart}}{start of each time interval}
    \item{\code{enum}}{observation number within subject}
    \item{\code{tstop}}{end of each time interval}
    \item{\code{status}}{status with 1 for infection and 0 for censoring}
  }
}
\references{
Fleming, T.R. and Harrington, D.P. (1991). Counting Processes and
Survival Analysis. John Wiley and Sons, New York.

Therneau, T. and Lumley, T. (2015). survival: Survival Analysis. R
package version 2.38-3.
}

