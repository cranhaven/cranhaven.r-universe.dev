#' Data from ACTG315 trial of HIV viral load in adults undergoing ART
#'
#' Data from the ACTG315 clinical trial of HIV-infected adults undergoing ART.
#' Data are included for 46 individuals, with HIV viral load measurements observed
#' on specific days up to 28 weeks after treatment initiation,
#' and converted to log10 RNA copies/ml. The RNA assay detection threshold was 100 copies/ml.
#' Additional columns include patient identifiers and CD4 T cell counts.
#'
#' @docType data
#'
#' @usage data(actg315raw)
#'
#' @format A data frame with 361 rows and 5 columns:
#' \describe{
#'   \item{Obs.No}{Row number}
#'   \item{Patid}{Numerical patient identifier}
#'   \item{Day}{Time of each observation, in days since treatment initiation}
#'   \item{log10.RNA.}{HIV viral load measurements, in log10 RNA copies/ml}
#'   \item{CD4}{CD4 T cell counts, in cells/mm^3}
#' }
#'
#' @keywords datasets
#'
#' @references Lederman et al (1998) JID 178(1), 70–79; Connick et al (2000) JID 181(1), 358–363;
#' Wu and Ding (1999) Biometrics 55(2), 410–418.
#'
#' @source \href{https://sph.uth.edu/divisions/biostatistics/wu/datasets/ACTG315LongitudinalDataViralLoad.htm}{Hulin Wu, Data Sets}
#'
#' @examples
#' library(dplyr)
#' data(actg315raw)
#'
#' actg315 <- actg315raw %>%
#'     mutate(vl = 10^log10.RNA.) %>%
#'     select(id = Patid, time = Day, vl)
#'
#' print(head(actg315))
#'
#' \donttest{plot_data(actg315, detection_threshold = 100)}
"actg315raw"
