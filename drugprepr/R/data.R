#' Example data from the Clinical Practice Research Datalink (CPRD).
#'
#' A dataset containing prescription information for two individuals.
#' The dataset is a hypothetical dataset resembling the real CPRD data.
#'
#' @format A data frame with 18 rows and 9 variables:
#' \describe{
#'   \item{patid}{unique identifier given to a patient in CPRD GOLD}
#'   \item{pracid}{unique identifier given to a practice in CPRD GOLD}
#'   \item{start_date}{Beginning of the prescription period}
#'   \item{prodcode}{CPRD unique code for the treatment selected by the GP}
#'   \item{dossageid}{Identifier that allows dosage information on the event to be retrieved from Common Dosages Lookup table}
#'   \item{text}{Prescription instruction for the prescribed product, as entered by the GP}
#'   \item{qty}{Total quantity entered by the GP for the prescribed product}
#'   \item{numdays}{Number of treatment days prescribed for a specific therapy event}
#'   \item{dose_duration}{an estimated prescription duration, as entered by CPRD}
#'   ...
#' }
#' @source \url{https://cprdcw.cprd.com/_docs/CPRD_GOLD_Full_Data_Specification_v2.0.pdf}
"dataset1"

#' Example min-max data.
#'
#' A dataset containing minimum and maximum possible values for quantity and
#' number of daily dose for given prescription.
#' The dataset is hypothetical.
#'
#' @format A data frame with 2 rows and 5 variables:
#' \describe{
#'   \item{prodcode}{CPRD unique code for the treatment selected by the GP}
#'   \item{max_qty}{maximum possible quantity to be prescribed for the product}
#'   \item{min_qty}{minimum possible quantity to be prescribed for the product}
#'   \item{max_ndd}{maximum possible number of daily dose to be prescribed for the product}
#'   \item{min_ndd}{minimum possible number of daily dose  to be prescribed for the product}
#'   ...
#' }
"min_max_dat"

#' Example electronic prescription dataset
#'
#' Based on a hypothetical 'therapy' file from the Clinical Practical Research
#' Datalink (CPRD), a UK database of primary care records.
#'
#' @note This dataset is now generated deterministically, so it will not vary
#' between sessions.
#'
#' @export
example_therapy <- data.frame(
  patid = rep(1:10, each = 3),
  pracid = rep(c('x', 'y'), each = 3),
  prodcode = c("a", "b", "b", "a", "a", "a", "a", "a", "b", "a", "b", "a",
               "a", "a", "b", "b", "b", "a", "a", "a", "b", "b", "b", "a", "a",
               "b", "b", "a", "a", "a"),
  start_date = as.Date('2021-10-01') +
    c(20L, 65L, 46L, 3L, 33L, 10L, 28L, 33L, 47L, 10L, 56L, 19L,
      13L, 35L, 17L, 138L, 7L, 15L, 18L, 12L, 27L, 12L, 19L, 31L, 46L,
      32L, 39L, 73L, 17L, 2L),
  qty = c(50L, 49L, 56L, 37L, NA, 56L, 51L, 39L, 51L, 55L, 41L, 47L,
          46L, 43L, NA, 44L, 50L, 50L, NA, 47L, 46L, 62L, 52L, 40L, 49L,
          NA, 38L, 57L, 49L, NA),
  ndd = c(6, 7.5, 2, 7, 1.5, 7, 8, 4.5, 6.5, 3, 2, 3, 8, 5.5, 1.5, 6,
          NA, 5, 6.5, 6.5, 6, 7, 6.5, 3.5, 5.5, 5, 3.5, 0.5, 3, 2),
  stringsAsFactors = FALSE
)
