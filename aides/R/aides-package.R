#' aides: Additive Information & Details of Evidence Synthesis
#'
#' @description
#' \emph{aides}, an R package, has been proposed to be a useful collection of
#' functions designed to offer supplementary information and intricacies in data
#' synthesis and evidence evaluation. Essentially, package \emph{aides} serves
#' as an aiding toolkit for pooled analysis of aggregated data, crafted with a
#' vision to support a more inclusive and informed approach to evidence-based
#' decision-making; and it is developed with values on flexibility, ease of use,
#' and comprehensibility. Package \emph{aides} will be updated with advances of
#' methodology of data synthesis and evidence evaluation. The initial goals are
#' to simplify analysis process for both professionals and public users, and to
#' support them in navigating the complexities of synthesized evidence. Long-term
#' goal of package \emph{aides} is to support knowledge translation and decision-making
#' based on the obtained information with comprehensive understanding of the
#' evidence.
#'
#' Package \emph{aides} is currently is developed using \bold{R version 4.2.2 (2022-10-31 ucrt)}.
#' Extra imported packages are as follows:
#'
#' \itemize{
#'  \item \href{https://cran.r-project.org/src/contrib/Archive/boot/boot_1.3-28.tar.gz}{\emph{boot} (version 1.3-28)}
#'  \item \href{https://cran.r-project.org/package=metafor}{\emph{metafor} (version 4.4-0)}
#'  \item \href{https://cran.r-project.org/package=meta}{\emph{meta} (version 7.0-0)}
#' }
#'
#'
#' @details
#' Current version consists of eight functions, including four functions for
#' examining fundamental assumptions before test of small-study effects (i.e.
#' function \code{\link{PlotDistrSS}}, \code{\link{TestDisparity}}, \code{\link{PlotDisparity}}, and \code{\link{TestDiscordance}})
#' and four functions for performing sequential-method-related analyses (i.e. \code{\link{DoSA}}, \code{\link{DoOSA}},
#' \code{\link{PlotOSA}}, and \code{\link{PlotPower}}).
#'
#' @name aides-package
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
