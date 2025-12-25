\name{SampleMissing}
\alias{SampleMissing}
\title{
Sample and update missing data
}
\description{
Sample and update missing data if missing data are presented in the input
}
\usage{
SampleMissing(MissData, para, orig, G_household, M, hyper)
}
\arguments{
  \item{MissData}{
  The missing data structure that provides all infro related to missing data}
  \item{para}{A list of the initial values of the parameters; output of the \verb{initParameters} function.}
  \item{orig}{A list object including all the necessary data variables needed by the sampler.}
  \item{G_household}{group level household index}
  \item{M}{individual level latent class  indexes}
  \item{hyper}{Hyper parameters for the prior distributions.}
}

