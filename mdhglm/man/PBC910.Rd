\name{PBC910}
\alias{PBC910}
\docType{data}
\title{Primary Biliary Cirrhosis Data
}
\description{
Komarek (2015) presented a longitudinal data set from a Mayo Clinic trial on 312 patients with primary biliary cirrhosis
(PBC) conducted in 1974-1984. There are 1 to 5 visits per subject performed at time of months. 
At each visit, measurements of
three response variables are observed: continuous logarithmic bilirubin, discrete platelet count 
and dichotomous indication of blood
vessel malformations. 
}
\usage{data("PBC910")}
\format{
  A data frame with 918 observations on the following 9 variables.
  \describe{
    \item{\code{subject}}{patients idenfier}
    \item{\code{day}}{visiting time in day}
    \item{\code{month}}{visiting time in month}
    \item{\code{lbili}}{continuous logarithmic bilirubin}
    \item{\code{platelet}}{discrete platelet count}
    \item{\code{spiders}}{dichotomous indication of blood
vessel malformation}
  }
}
\references{
Komarek, A. (2015). mixAK: Multivariate Normal Mixture Models and
Mixtures of Generalized Linear Mixed Models Including Model Based
Clustering. R package version 4.2.
}

