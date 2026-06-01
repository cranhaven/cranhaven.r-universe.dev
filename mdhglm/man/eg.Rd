\name{eg}
\alias{eg}
\docType{data}
\title{
Ethylene Glycol Data
}
\description{
Price et al. (1985) presented data from a study on the developmental toxicity, 
ethylene glycol (EG),in mice. Responses are the malformation (binary response) and 
fetal weight (continuous response) outcomes for the experiment, which shows clear dose-related trends with respect to both outcomes.}
\usage{data("eg")}
\format{
  A data frame with 1028 observations on the following 5 variables.
  \describe{
    \item{\code{litter}}{litter number for 94 mice}
    \item{\code{dose}}{dose input}
    \item{\code{y1}}{fetal weight (g)}
    \item{\code{y2}}{malformation (0; No, 1; Yes)}
    \item{\code{dose2}}{dose*dose}
  }
}
\references{
Price, C.J., Kimmel, C.A., Tyl, R.W. and Marr. M.C. (1985). The developmental toxicity of ethylene glycol in rats and mice. Toxicolal Applied Pharmacology, 81, 113---127.
}

