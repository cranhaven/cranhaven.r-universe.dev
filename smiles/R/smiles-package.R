#' smiles: Sequential Method In Leading Evidence Synthesis
#'
#' @description
#' The purpose of R-package \emph{smiles} is to provide a useful collection of
#' functions designed to apply sequential method in data synthesis and evidence
#' evaluation. This package is developed based on \href{https://cran.r-project.org/package=meta}{\emph{meta} (version 7.0-0)} in \bold{R version 4.2.2 (2022-10-31 ucrt)}.
#'
#' @details
#' Current version consists of three functions, including a main function of
#' sequential analysis (i.e. \code{\link{DoTSA}}) and a visualization function
#' of sequential analysis, (i.e. \code{\link{PlotCoRNET}}).
#'
#' @name smiles-package
#'
#' @docType package
#'
#' @keywords package
#'
#'
## usethis namespace: start
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom boot inv.logit
#' @importFrom graphics arrows
#' @importFrom graphics axis
#' @importFrom graphics barplot
#' @importFrom graphics boxplot
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics pie
#' @importFrom graphics points
#' @importFrom graphics polygon
#' @importFrom graphics rect
#' @importFrom graphics segments
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom grDevices col2rgb
#' @importFrom grDevices colorRampPalette
#' @importFrom grDevices colors
#' @importFrom grDevices rainbow
#' @importFrom grDevices recordPlot
#' @importFrom grDevices rgb
#' @importFrom stats binom.test
#' @importFrom stats density
#' @importFrom stats dnorm
#' @importFrom stats IQR
#' @importFrom stats ks.test
#' @importFrom stats median
#' @importFrom stats pnorm
#' @importFrom stats pt
#' @importFrom stats qnorm
#' @importFrom stats qt
#' @importFrom stats quantile
#' @importFrom stats rnorm
#' @importFrom stats sd
#' @importFrom stats shapiro.test
#' @importFrom stats wilcox.test
#' @importFrom utils capture.output
#' @importFrom utils head
#' @importFrom utils tail
## usethis namespace: end
NULL
