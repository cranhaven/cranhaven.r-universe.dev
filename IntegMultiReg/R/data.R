#' Simulated Multi-Platform Example Data
#'
#' A small simulated data set illustrating the integrative multi-regression
#' (IMR) setting: three molecular platforms measured on overlapping but
#' partially missing sets of subjects, clinical covariates available for
#' everyone, and three outcome types (continuous, binary and right-censored)
#' driven by the same latent signal.
#'
#' The platforms are observed on the following subjects, which induces four
#' non-empty availability subgroups (bitstrings over genomic/proteomic/
#' metabolomic): `011` (120 subjects), `111` (60), `101` (60) and `100` (60).
#'
#' @format A named list with the components:
#' \describe{
#'   \item{platforms}{A list of three data frames, `genomic` (20 features,
#'     subjects 1--240), `proteomic` (10 features, subjects 1--180) and
#'     `metabolomic` (8 features, subjects 121--300).  Each has an `id` column
#'     followed by the feature columns.}
#'   \item{covariates}{A data frame of three clinical covariates (`age`, `sex`,
#'     `stage`) with an `id` column, available for all 300 subjects.}
#'   \item{outcome}{The binary outcome data frame (alias of
#'     `outcome.binary`), used as the canonical demonstration outcome.}
#'   \item{outcome.binary}{Data frame with `id` and a 0/1 response `y`.}
#'   \item{outcome.continuous}{Data frame with `id` and a numeric response `y`.}
#'   \item{outcome.survival}{Data frame with `id`, observed `time` and event
#'     indicator `status` (1 = event, 0 = censored).}
#'   \item{truth}{A list giving, per platform, the column indices of the
#'     features with non-zero effects used to generate the outcomes.}
#' }
#'
#' @details The generating script is provided in `data-raw/make_simIMR.R`.
#'
#' @examples
#' data("simIMR", package = "IntegMultiReg")
#' sapply(simIMR$platforms, dim)
#' str(simIMR$truth)
#' @keywords datasets
"simIMR"


#' Reduced TCGA-KIRC Multi-Platform Survival Example
#'
#' A reduced real-data example derived from public UCSC Xena TCGA kidney renal
#' clear cell carcinoma (KIRC) data, not from controlled-access TCGA/GDC files.
#' The data set mirrors the multi-platform structure of Chekouo, Stingo, Doecke
#' and Do (2017): mRNA expression, miRNA expression and DNA methylation are
#' observed on overlapping but not identical sets of patients, with clinical
#' covariates and right-censored survival outcome.
#'
#' The full TCGA source files are not distributed with the package.  Instead,
#' this object stores a small screened feature panel suitable for examples and
#' vignette analyses.  Patient identifiers are package-internal labels
#' (`KIRC001`, `KIRC002`, ...) rather than TCGA barcodes; no barcode mapping is
#' distributed with the package.  Users should not attempt participant
#' re-identification or linkage to external resources.  The generating script is
#' provided in the source repository under `data-raw/make_kircIMR.R`.
#'
#' @format A named list with the components:
#' \describe{
#'   \item{platforms}{A list of three data frames, `mrna`, `mirna` and
#'     `methylation`.  Each has an `id` column containing package-internal
#'     patient labels followed by screened molecular features.}
#'   \item{covariates}{Clinical covariates with an `id` column: age at initial
#'     diagnosis, female sex indicator, pathologic stage and histologic grade.}
#'   \item{outcome}{Alias of `outcome.survival`, for convenience.}
#'   \item{outcome.survival}{Data frame with `id`, observed survival/follow-up
#'     time in days and event indicator `status` (1 = deceased, 0 = censored).}
#'   \item{feature_screening}{Per-platform feature-ranking information and a
#'     short description of the screening procedure used to create the reduced
#'     panel.}
#'   \item{platform_availability}{Patient-level logical matrix indicating which
#'     of the three platforms are available.}
#'   \item{subgroup_sizes}{Counts of the non-empty availability subgroups
#'     induced by the reduced data.}
#'   \item{model_subgroup_sizes}{The availability subgroups that would be
#'     modelled under the default real-data illustration threshold
#'     `ssize = 30`.}
#'   \item{paper_alignment}{Metadata describing how the reduced example maps to
#'     the Biometrics KIRC case study.}
#'   \item{source}{Source URLs and reference metadata.}
#' }
#'
#' @details The source data are public UCSC Xena TCGA-KIRC sampleMap files:
#' mRNA expression (`HiSeqV2`), miRNA expression (`miRNA_HiSeq_gene`), DNA
#' methylation (`HumanMethylation450`) and the KIRC clinical matrix.  Molecular
#' features are screened with univariable Cox models after basic filtering, and
#' missing molecular values in the retained panel are median-imputed within
#' feature.  The full Biometrics analysis used 776 mRNA, 91 miRNA and 729 DNA
#' methylation features; this package example retains 50, 30 and 50 features,
#' respectively, to keep examples lightweight.  The package does not distribute
#' controlled-access TCGA/GDC data or the full Biometrics/Wiley supplementary
#' data files.
#'
#' @references Chekouo T, Stingo FC, Doecke JD, Do K-A (2017). "A Bayesian
#' Integrative Approach for Multi-Platform Genomic Data: A Kidney Cancer Case
#' Study." \emph{Biometrics}, \strong{73}(2), 615--624.
#' \doi{10.1111/biom.12587}
#'
#' The Cancer Genome Atlas Research Network; National Cancer Institute Genomic
#' Data Commons; UCSC Xena.
#'
#' @examples
#' data("kircIMR", package = "IntegMultiReg")
#' sapply(kircIMR$platforms, dim)
#' kircIMR$subgroup_sizes
#' kircIMR$model_subgroup_sizes
#' @keywords datasets
"kircIMR"
