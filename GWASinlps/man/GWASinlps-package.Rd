\name{GWASinlps-package}

\alias{GWASinlps-package}

\docType{package}

\title{Non-local prior based iterative variable selection tool for genome-wide association study data, or other high-dimensional data}

\description{The \pkg{GWASinlps} package performs variable selection for data from genome-wide association studies (GWAS), or other high-dimensional data with continuous, binary or survival outcomes, combining in an iterative framework, the computational efficiency of the structured screen-and-select variable selection strategy based on some association learning and the parsimonious uncertainty quantification provided by the use of non-local priors (see the References).
}

\details{
\tabular{ll}{
Package: \tab GWASinlps\cr
Type: \tab Package\cr
Version: \tab 2.3\cr
Date: \tab 2024-10-19\cr
License: \tab GPL (>= 2)\cr
}

The main function: \cr
\code{\link{GWASinlps}}

The main function calls the following functions: \cr
\code{\link{nlpsLM}} \cr
\code{\link{nlpsGLM}} \cr
\code{\link{nlpsAFTM}}
}

\author{Nilotpal Sanyal <nilotpal.sanyal@gmail.com>

Maintainer: Nilotpal Sanyal <nilotpal.sanyal@gmail.com>
}

\references{
Sanyal et al. (2019), "GWASinlps: Non-local prior based iterative SNP selection tool for genome-wide association studies". Bioinformatics, 35(1), 1-11.

Sanyal, N. (2022). "Iterative variable selection for high-dimensional data with binary outcomes". arXiv preprint arXiv:2211.03190.
}











