\name{respiratory}
\alias{respiratory}
\docType{data}
\title{
Respiratory Data
}
\description{
This example is from a clinical trial comparing two treatments for a respiratory
illness (Strokes et al., 1995). In each of two centers, eligible patients
were randomly assigned to active treatment or placebo.
During treatment, respiratory status was determined
at four visits. Potential explanatory variables were centre, sex, 
and baseline respiratory status (all dichotomous), as well as
age(in years) at the time of study entry. There were 111 patients (54
active, 57 placebo).
}
\usage{data("respiratory")}
\format{
  A data frame with 444 observations on the following 8 variables.
  \describe{
    \item{\code{patient}}{patients identifier for 111 patients}
    \item{\code{treatment}}{a factor for treatment with levels \code{active} and \code{placebo}}
    \item{\code{sex}}{a factor for gender with levels \code{female} and \code{male}}
    \item{\code{age}}{age in years}
    \item{\code{center}}{center number for two centers}
    \item{\code{baseline}}{baseline respiratory status (0 or 1)}
    \item{\code{past}}{respiratory status in the previous visit}
    \item{\code{y}}{respiratory status (0=poor, 1=good)}
  }
}
\references{
Strokes, M.E., Davis, C.S., and Koch, G.G. (1995). Categorical Data
Analysis using the SAS System. SAS Institute, Cary, NC.
}

