\name{data.gen}
\alias{data.gen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Generation
}
\description{
Generates genotypes data matrix G (\code{sample_size} by \code{p}), vector of environmental measurments E, and an outcome vector Y of size \code{sample_size}. Simulates training, validation, and test datasets.
}
\usage{
data.gen(sample_size = 100, p = 20, n_g_non_zero = 15, n_gxe_non_zero = 10, 
         family = "gaussian", mode = "strong_hierarchical", 
         normalize = FALSE, normalize_response = FALSE, 
         seed = 1, pG = 0.2, pE = 0.3,
         n_confounders = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample_size}{sample size of the data}
  \item{p}{total number of main effects}
  \item{n_g_non_zero}{number of non-zero main effects to generate}
  \item{n_gxe_non_zero}{number of non-zero interaction effects to generate}
  \item{family}{"gaussian" for continous outcome Y and "binomial" for binary 0/1 outcome}
  \item{mode}{either "strong_hierarchical", "hierarchical", or "anti_hierarchical".
  In the \emph{strong hierarchical} mode the hierarchical structure is maintained (beta_g = 0  then beta_gxe = 0) and also |beta_g| >= |beta_gxe|. In the \emph{hierarchical} mode the  hierarchical structure is maintained, but |beta_G| < |beta_gxe|. In the \emph{anti_hierarchical} mode the hierarchical structure is violated (beta_g = 0  then beta_gxe != 0).}
  \item{normalize}{\code{TRUE} to normalize matrix \code{G} and vector \code{E}}
    \item{normalize_response}{\code{TRUE} to normalize vector \code{Y}}
  \item{pG}{genotypes prevalence, value from 0 to 1}
  \item{pE}{environment prevalence, value from 0 to 1}
  \item{seed}{random seed}
  \item{n_confounders}{number of confounders to generate, either \code{NULL} or \code{>1}}
}
\value{
A list of simulated datasets and generating coefficients
\item{G_train, G_valid, G_test}{generated genotypes matrices}
\item{E_train, E_valid, E_test}{generated vectors of environmental values}
\item{Y_train, Y_valid, Y_test}{generated outcome vectors}
\item{C_train, C_valid, C_test}{generated confounders matrices}
\item{GxE_train, GxE_valid, GxE_test}{generated GxE matrix}
\item{Beta_G}{main effect coefficients vector}
\item{Beta_GxE}{interaction coefficients vector}
\item{beta_0}{intercept coefficient value}
\item{beta_E}{environment coefficient value}
\item{Beta_C}{confounders coefficient values}
\item{index_beta_non_zero, 
      index_beta_gxe_non_zero,
      index_beta_zero, 
      index_beta_gxe_zero}{inner data generation variables}
\item{n_g_non_zero}{number of non-zero main effects generated}
\item{n_gxe_non_zero}{number of non-zero interactions generated}
\item{n_total_non_zero}{total number of non-zero variables}
\item{SNR_g}{signal-to-noise ratio for the main effects}
\item{SNR_gxe}{signal-to-noise ratio for the interactions}
\item{family, p, sample_size, mode, seed}{input simulation parameters}

}
\examples{
data = data.gen(sample_size=100, p=100)
G = data$G_train; GxE = data$GxE_train
E = data$E_train; Y = data$Y_train
}

