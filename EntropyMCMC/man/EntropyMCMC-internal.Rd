\name{EntropyMCMC-internal}
% the MCMC algorithms implemented
\alias{AMHaario}
\alias{RWHM}
\alias{IID_norm}
\alias{HMIS_norm}

% MCMCM-related functions
\alias{IID_step}
\alias{HM_step}
\alias{RWHM_step}
\alias{q_pdf_ISnorm}
\alias{q_proposal_ISnorm}
\alias{target_norm}
\alias{target_norm_param}

% utility functions
\alias{mvkde}
\alias{mvwkde}
\alias{cutTask.mc}
\alias{ellipse}
\alias{mvbw}
\alias{RmseEnt}
\alias{logdmvnorm}
\alias{dist.fun}

% seems to be needed for C calls consistency
\alias{entropyNNC}

% Entropy-related functions
\alias{Entropy.ANN}
\alias{Entropy.ANN.mc}
\alias{Entropy.ANN.vect}
\alias{Entropy.ANN.vect.mc}
\alias{Entropy.Gf}
\alias{Entropy.NN}
\alias{Entropy.NN.mc}
\alias{Entropy.Resubst}
\alias{Entropy.kNN}
\alias{Entropy.kNN.mc}
\alias{Entropy.kNN.vect}
\alias{Entropy.kNN.vect.mc}

\title{Internal 'EntropyMCMC' Functions}
\description{
  Various internal functions 
  for the package \pkg{EntropyMCMC}; usage for some of these
  are displayed below.
}
\usage{
target_norm(x, param)
RWHM_step(theta, target, q_pdf = gaussian_pdf, 
          q_proposal = gaussian_proposal, f_param, q_param, nba)
HM_step(theta, target, q_pdf, q_proposal, 
          f_param, q_param, nba)
}

\arguments{
  \item{x}{a vector of values, or objects.}
  \item{target}{a target  density definition.}
  \item{param}{target - or proposal -  density parameters (including data in a Bayesian model.}
  \item{f_param}{target density parameters (including data in a Bayesian model.}
  \item{q_param}{proposal density parameters.}
  \item{theta}{the \eqn{d}-dimensional parameter of the Markov chain.}
  \item{nba}{number of accepted moves along simulation, for HAstings-Metropolis MCMC's}
}

\details{
  These functions and objects (lists) are usually not to be called by the user.
}


\keyword{internal}
