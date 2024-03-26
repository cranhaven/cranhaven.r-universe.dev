\docType{package}
\name{ArchaeoChron}
\alias{ArchaeoChron}
\alias{ArchaeoChron-package}
\title{Bayesian Modeling of Archaeological Chronologies}
\description{
This package provides a list of functions for the Bayesian modeling of archaeological chronologies. The Bayesian models are implemented in JAGS (JAGS stands for Just Another Gibbs Sampler. It is a program for the analysis of Bayesian hierarchical models using Markov Chain Monte Carlo (MCMC) simulation. See <http://mcmc-jags.sourceforge.net/> and "JAGS Version 4.3.0 user manuel", Martin Plummer (2017) <https://sourceforge.net/projects/mcmc-jags/files/Manuals/>.). The inputs are measurements with their associated standard deviations and the study period. The output is the MCMC sample of the posterior distribution of the event date with or without radiocarbon calibration. 
}
\section{ArchaeoChron functions}{

\itemize{ 
  \item combination_Gauss() : A function for a simple combination of Gaussian dates 
  \item combinationWithOutliers_Gauss() : A function for combining Gaussian dates using the outliers model described in Bronk Ramsey, 2009. 
  \item combinationWithRandomEffect_Gauss() : A function for combining Gaussian dates introducing a random effect (see Congdom, 2010)
  \item eventModel_Gauss() : A function for combining Gaussian dates introducing an individual random effect (see Lanos and Philippe, 2017)

  \item chrono_Gauss() : A function for a simple chronology of Gaussian dates
  \item chronoOutliers_Gauss() : A function for the chronology of Gaussian dates associated with an outlier modeling (Bronk Ramsey, 2009) 
  \item chronoEvents_Gauss() : A function for the chronology of events combining Gaussian dates (Lanos and Philippe, 2017)

  \item eventModel_C14() : A function for combining radiocarbon dates
  }
  
}

