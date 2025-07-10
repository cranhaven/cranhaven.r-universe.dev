#' Basic information of virtual persons
#'
#' A dataset containing the personal basic information (name, sex, age, and death_age) of virtual persons.
#'
#' @format A data frame with 6 rows and 4 variables:
#' \describe{
#'   \item{..name}{name, chinese or foreigner, in carats}
#'   \item{..sex}{sex of the person, in carats}
#'   \item{..age}{living age in final record, in numbers}
#'   \item{..death..age}{final age when a person is dead, in numbers}
#'   ...
#'
#' }
#' @source "simulated dataset"
"people"

#' Grade records of virtual persons in high school
#'
#' A dataset containing the personal grade information (chinese, math, english, physics, biology, chemistry)
#'  of virtual persons.
#'
#' @format A data frame with 6 rows and 7 variables:
#' \describe{
#'   \item{name}{name, chinese or foreigner, in carats}
#'   \item{chinese}{grade of the chinese, in numbers}
#'   \item{math}{grade of the math, in numbers}
#'   \item{english}{grade of the english, in numbers}
#'   \item{physics}{grade of the physics, in numbers}
#'   \item{biology}{grade of the biology, in numbers}
#'   \item{chemistry}{grade of the chemistry, in numbers}
#'   ...
#'
#' }
#' @source "simulated dataset"
"grade"


#' The SNPV number within 1Mb bins at chromosome levels generated from transcriptome dataset of two dog populations
#' (including wild wolf and domesticated dogs).
#'
#' A dataset containing the SNV number within 1Mb bins called from transcriptome dataset of wild wolf and domesticated dogs.
#'
#' @format A data frame with 2544 rows and 4 variables:
#' \describe{
#'   \item{CHROM}{chrom id, reference genome of CanFam3.1, in numbers/carats}
#'   \item{BIN_START}{the start genomic coordinate for one bin at relevant chromosome, in numbers}
#'   \item{SNP_COUNT}{the end genomic coordinate for one bin at relevant chromosome, in numbers}
#'   \item{VARIANTS.KB}{SNV(variants) number within one bin per KB, in numbers}
#'   ...
#'
#' }
#' @source "real dataset"
"SNV_1MB_density_data"
