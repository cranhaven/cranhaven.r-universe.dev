\name{crack_growth}
\alias{crack_growth}
\docType{data}
\title{Crack Growth Data}
\description{
Hudak et al. (1978) presented data on crack growth data, which have been listed by Lu and Meeker
(1993). There are 21 metallic specimens, each subjected to 120,000 loading cycles, with the
crack lengths recorded every 10,000 cycles. We take t=no. cycles/100,000 here, so t(j)=j/1000
for j=1,...,12 coded by cycle. The crack increment sequences look rather irregular. Let l(i,j) be the
crack length of the ith specimen at the jth observation and let y(i,j)=l(i,j)-l(i,j-1) be the corresponding
increment of crack length, which has always a positive value. l(i,j-1) is coded by crack0 to use
as a covariate.
}
\usage{data("crack_growth")}
\format{
  A data frame with 241 observations on the following 5 variables.
  \describe{
    \item{\code{ID}}{id for each 241 observations}
    \item{\code{y}}{increment in crack length, y(i,j)=l(i,j)-l(i,j-1), where l(i,j) is the crack length of the ith specimen at the jth observation}
    \item{\code{crack0}}{previous crack length, l(i,j-1)}
    \item{\code{specimen}}{specimen number for 21 metallic specimens}
    \item{\code{cycle}}{number of cycles/100,000; t(j)=j/100, j=1,...,12}
  }
}
\references{
Hudak, S.J., Saxena, A. Bucci, R.J. and Malcom, R.C. (1978). Development of standard methods
of testing and analyzing fatigue crack growth rate data. Technical report. AFML-TR-78-40.
Westinghouse R and D Center, Wesinghouse Electric Corp., Pittsburgh, PA.

Lu, C.J. and Meeker, W.Q. (1993). Using degeneration measurements to estimate a time-to-failure
distribution, Technometrics, 35, 161--174.
}

