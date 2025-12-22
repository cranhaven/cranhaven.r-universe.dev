% Please edit documentation in R/dmlalg.R
\docType{package}
\name{dmlalg}
\alias{dmlalg}
\title{dmlalg: double machine learning algorithms}
\description{
The \code{dmlalg} package contains implementations of
double machine learning (DML) algorithms in \code{R}.
}
\section{Partially linear models with confounding variables}{
    Our goal is to perform inference for the linear parameter in partially
    linear models with confounding variables.
    The standard DML estimator of the linear parameter has a two-stage least
    squares interpretation, which can lead to a large variance and overwide
    confidence intervals.
    We apply regularization to reduce the variance of the estimator,
    which produces narrower confidence intervals that are approximately valid.
    Nuisance terms can be flexibly estimated with machine learning algorithms.
    \describe{
    \item{\code{\link{regsdml}}}{Estimates the
    linear parameter in a partially linear model with
    confounding variables with regularized and standard
    DML methods.}
    \item{\code{\link{summary.regsdml}}}{A \code{summary} method for objects
    fitted with \code{\link{regsdml}}.}
    \item{\code{\link{confint.regsdml}}}{A \code{confint} method for objects
    fitted with \code{\link{regsdml}}.}
    \item{\code{\link{coef.regsdml}}}{A \code{coef} method for objects
    fitted with \code{\link{regsdml}}.}
    \item{\code{\link{vcov.regsdml}}}{A \code{vcov} method for objects
    fitted with \code{\link{regsdml}}.}
    \item{\code{\link{print.regsdml}}}{A \code{print} method for objects
    fitted with \code{\link{regsdml}}.}
    }
}
\section{Partially linear mixed-effects models with repeated measurements}{
    Our goal is to estimate and perform inference for the linear
    coefficient in a partially linear mixed-effects model
    with DML. Machine learning algorithms allows us to incorporate more
    complex interaction structures and high-dimensional variables.

    \describe{
    \item{\code{\link{mmdml}}}{Estimates the
    linear parameter in a PLMM with repeated
    measurements using
    double machine learning.}
    \item{\code{\link{confint.mmdml}}}{A \code{confint} method for objects
    fitted with \code{\link{mmdml}}.}
    \item{\code{\link{fixef.mmdml}}}{A \code{fixef} method for objects
    fitted with \code{\link{mmdml}}.}
    \item{\code{\link{print.mmdml}}}{A \code{print} method for objects
    fitted with \code{\link{mmdml}}.}
    \item{\code{\link{ranef.mmdml}}}{A \code{ranef} method for objects
    fitted with \code{\link{mmdml}}.}
    \item{\code{\link{residuals.mmdml}}}{A \code{residuals} method for objects
    fitted with \code{\link{mmdml}}.}
    \item{\code{\link{sigma.mmdml}}}{A \code{sigma} method for objects
    fitted with \code{\link{mmdml}}.}
    \item{\code{\link{summary.mmdml}}}{A \code{summary} method for objects
    fitted with \code{\link{mmdml}}.}
    \item{\code{\link{vcov.mmdml}}}{A \code{vcov} method for objects
    fitted with \code{\link{mmdml}}.}
    \item{\code{\link{VarCorr.mmdml}}}{A \code{VarCorr} method for objects
    fitted with \code{\link{mmdml}}.}
    }
}

\references{
C. Emmenegger and P. \enc{Bühlmann}{Buehlmann}.
Regularizing Double Machine Learning in Partially Linear Endogenous
Models, 2021. Preprint arXiv:2101.12525.

C. Emmenegger and P. \enc{Bühlmann}{Buehlmann}.
Double Machine Learning for Partially Linear Mixed-Effects Models with Repeated Measurements. Preprint arXiv:2108.13657.
}

