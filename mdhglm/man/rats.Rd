\name{rats}
\alias{rats}
\docType{data}
\title{Rat Data}
\description{
The data set presented by Mantel, et al. (1977) is based on a tumorigenesis
study of 50 litters of female rats. For each litter, one rat was
selected to receive the drug and the other two rats were placebo-treated
controls. Here each litter is treated as a cluster. The survival
time is the time to development of tumor, measured in weeks.
Death before occurrence of tumor yields a right-censored observation;
forty rats developed a tumor, leading to censoring of about 73 percent.
}
\usage{data("rats")}
\format{
  A data frame with 300 observations on the following 5 variables.
  \describe{
    \item{\code{litter}}{cluster number for 50 litters}
    \item{\code{rx}}{tratment (1=drug, 0=placebo)}
    \item{\code{time}}{time to development of tumor, measured in weeks}
    \item{\code{status}}{status (1=occurrence of event, 0=censoring)}
    \item{\code{sex}}{a factor with levels \code{f} (female) and \code{m} (male) }
  }
}
\references{
Mantel, N., Bohidar, N.R. and Ciminera, J.L. (1977). Mantel-haenszel
analyses of litter-matched time-to-response data, with modifications
for recovery of interlitter information. Cancer Research, 37, 3863--3868.
}

