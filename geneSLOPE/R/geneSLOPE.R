#' Genome-Wide Association Study with SLOPE
#'
#' @description Package geneSLOPE performes genome-wide association study (GWAS) with \pkg{\link[SLOPE]{SLOPE}},
#' short for Sorted L-One Penalized Estimation. SLOPE is a
#' method for estimating the vector of coefficients in linear model. For details
#' about it see references.
#'
#' @details GWAS is splitted into three steps.
#' \itemize{
#' \item In the first step data is read using \pkg{\link[bigmemory]{bigmemory}} package and immediately
#' screened using marginal tests for each SNP
#' \item SNPs are clumped based on their correlations
#' \item SLOPE is performed on data where each clump has
#' one representative (therefore we ensure that variables in linear model
#' are not strognly correlated)
#' }
#' Version: 0.38.3
#'
#' @name geneSLOPE
#' @aliases geneSLOPE-package
#' @import ggplot2
#' @importFrom grid downViewport grid.locator upViewport
#' @importFrom SLOPE SLOPE
#' @importFrom stats aggregate cor lm lm.fit pf var qnorm
#' @importFrom utils head read.table setTxtProgressBar tail txtProgressBar
#' @importFrom bigmemory read.big.matrix
#' @author{
#' Malgorzata Bogdan, Damian Brzyski, Emmanuel J. Candes, Christine Peterson, Chiara Sabatti, Piotr Sobczyk
#'
#' Maintainer: Piotr Sobczyk \email{pj.sobczyk@@gmail.com}
#' }
#' @references \emph{SLOPE -- Adaptive Variable Selection via Convex Optimization},
#' Malgorzata Bogdan, Ewout van den Berg, Chiara Sabatti,
#' Weijie Su and Emmanuel Candes
#'
#' @examples
#' \donttest{
#' famFile <- system.file("extdata", "plinkPhenotypeExample.fam", package = "geneSLOPE")
#' mapFile <- system.file("extdata", "plinkMapExample.map", package = "geneSLOPE")
#' snpsFile <- system.file("extdata", "plinkDataExample.raw", package = "geneSLOPE")
#' phe <- read_phenotype(filename = famFile)
#' screening.result <- screen_snps(snpsFile, mapFile, phe, pValMax = 0.05, chunkSize = 1e2)
#' clumping.result <- clump_snps(screening.result, rho = 0.3, verbose = TRUE)
#' slope.result <- select_snps(clumping.result, fdr=0.1)
#' }
#' \dontrun{
#' gui_geneSLOPE()
#' }
"_PACKAGE"
NULL
