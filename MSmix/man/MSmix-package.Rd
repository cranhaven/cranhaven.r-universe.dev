\name{MSmix-package}
\alias{MSmix-package}
\alias{MSmix}
\docType{package}
\title{Finite Mixtures of Mallows Models with Spearman Distance for Full and Partial Rankings}
\description{
The \pkg{MSmix} package provides functions to fit and analyze finite
Mixtures of Mallows models with Spearman distance (a.k.a. \eqn{\theta}-model)
for full and partial rankings with arbitrary missing positions.
Inference is conducted within the maximum likelihood (ML) framework via EM algorithms.
Estimation uncertainty is tackled via diverse versions of bootstrapped and asymptotic confidence intervals.
}
\details{
The \emph{Mallows model} is one of the most popular and frequently applied
parametric distributions to analyze rankings of a finite set of items.
However, inference for this model is challenging due to the intractability
of the normalizing constant, also referred to as \emph{partition function}.
The present package performs ML estimation (MLE) of the Mallows model with Spearman distance
from full and partial rankings with arbitrary censoring patterns.
Thanks to the novel approximation of the model normalizing constant introduced by
Crispino, Mollica, Astuti and Tardella (2023), as well as the existence of
a closed-form expression of the MLE of the consensus ranking, \pkg{MSmix} can address inference even for a large number of items.
The package also allows to account for unobserved sample heterogeneity through MLE of
\emph{finite mixtures of Mallows models with Spearman distance} via EM algorithms, in order to
perform a model-based clustering of partial rankings into groups with similar preferences.

Computational efficiency is achieved with the use of a hybrid language, combining \code{R} and \code{C++} code,
and the possibility of parallel computation.

In addition to inferential techniques, the package provides various
functions for data manipulation, simulation, descriptive summary and model selection.

Specific S3 classes and methods are also supplied to enhance the usability
and foster exchange with other packages.

The suite of functions available in the \pkg{MSmix} package is composed of:

\strong{Ranking data manipulation}
\describe{
\item{\code{\link{data_conversion}}}{From rankings to orderings and vice versa.}
\item{\code{\link{data_censoring}}}{Censoring of full rankings.}
\item{\code{\link{data_completion}}}{Deterministic completion of partial rankings
with full reference rankings.}
\item{\code{\link{data_augmentation}}}{Generate all full rankings compatible with
partial rankings.}
}
\strong{Ranking data simulation}
\describe{
\item{\code{\link{rMSmix}}}{Random samples from finite mixtures of Mallows models
with Spearman distance.}
}
\strong{Ranking data description}
\describe{
\item{\code{\link{data_description}}}{Descriptive summaries for partial rankings.}
}
\strong{Model estimation}
\describe{
\item{\code{\link{fitMSmix}}}{MLE of mixtures of Mallows models with Spearman distance via EM algorithms.}
\item{\code{\link{likMSmix}}}{Likelihood evaluation for mixtures of Mallows models with Spearman distance.}
}
\strong{Model selection}
\describe{
\item{\code{\link{bicMSmix}}}{BIC value for the fitted mixture of Mallows models with Spearman distance.}
\item{\code{\link{aicMSmix}}}{AIC value for the fitted mixture of Mallows models with Spearman distance.}
}
\strong{Estimation uncertainty}
\describe{
\item{\code{\link{bootstrapMSmix}}}{Bootstrap confidence intervals for mixtures of Mallows models with Spearman distance.}
\item{\code{\link{confintMSmix}}}{Asymptotic confidence intervals for mixtures of Mallows models with Spearman distance.}
}
\strong{Spearman distance utilities}
\describe{
\item{\code{\link{spear_dist}}}{Spearman distance computation for full rankings.}
\item{\code{\link{spear_dist_distr}}}{Spearman distance distribution
under the uniform (null) model.}
\item{\code{\link{partition_fun_spear}}}{Partition function of the Mallows model with Spearman distance.}
\item{\code{\link{expected_spear_dist}}}{Expected Spearman distance under the Mallows model with Spearman distance.}
\item{\code{\link{var_spear_dist}}}{Variance of the Spearman distance under the Mallows model with Spearman distance.}
}
\strong{S3 class methods} \describe{
\item{\code{\link{print.bootMSmix}}}{Print the bootstrap confidence intervals of mixtures of Mallows models with Spearman distance.}
\item{\code{\link{print.data_descr}}}{Print the descriptive statistics for partial rankings.}
\item{\code{\link{print.emMSmix}}}{Print the MLEs of mixtures of Mallows models with Spearman distance.}
\item{\code{\link{print.summary.emMSmix}}}{Print the summary of the MLEs of mixtures of Mallows models with Spearman distance.}
\item{\code{\link{plot.bootMSmix}}}{Plot the bootstrap confidence intervals of mixtures of Mallows models with Spearman distance.}
\item{\code{\link{plot.data_descr}}}{Plot the descriptive statistics for partial rankings.}
\item{\code{\link{plot.dist}}}{Plot the Spearman distance matrix for full rankings.}
\item{\code{\link{plot.emMSmix}}}{Plot the MLEs of mixtures of Mallows models with Spearman distance.}
\item{\code{\link{summary.emMSmix}}}{Summary of the MLEs of mixtures of Mallows models with Spearman distance.}
}
\strong{Datasets}
\describe{
\item{\code{\link{ranks_antifragility}}}{Antifragility features of innovative startups (full rankings with covariates).}
\item{\code{\link{ranks_horror}}}{Arkham Horror data (full rankings).}
\item{\code{\link{ranks_beers}}}{Beers data (partial rankings with different censoring patterns and a covariate).}
\item{\code{\link{ranks_read_genres}}}{Reading preference data (partial top-5 rankings with covariates).}
\item{\code{\link{ranks_sports}}}{Sport preferences and habits (full rankings with covariates).}
}

Some quantities frequently recalled in the manual are the following:
\describe{
\item{\eqn{N}}{Sample size.}
\item{\eqn{n}}{Number of possible items.}
\item{\eqn{G}}{Number of mixture components.}
}
Data must be supplied as an integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix with partial rankings in each row and missing positions denoted as NA (rank = 1 indicates the
most-liked item). Partial sequences with a single missing entry are
automatically filled in, as they correspond to full rankings. In the present setting, ties are not allowed.
}
\references{
Crispino M, Mollica C, Astuti V and Tardella L (2023). Efficient
and accurate inference for mixtures of Mallows models with Spearman distance.
\emph{Statistics and Computing}, \bold{33}(98), DOI: 10.1007/s11222-023-10266-8.

Crispino M, Mollica C, Modugno L, Casadio Tarabusi E, and
Tardella L (2024+). MSmix: An R Package for clustering partial rankings
via mixtures of Mallows models with Spearman distance. \emph{(submitted)}.
}
\author{
Cristina Mollica, Marta Crispino, Lucia Modugno and Luca Tardella

Maintainer: Cristina Mollica <\href{mailto:cristina.mollica@uniroma1.it}{cristina.mollica@uniroma1.it}>
}
