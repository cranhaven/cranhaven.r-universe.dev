#' exampleNprcgenekeeprConfig is a loadable version of the example
#' configuration file \code{example_nprcgenekeepr_config}
#'
#' It contains a working version of a \strong{nprcgenekeepr} configuration
#' file created the SNPRC.
#' Users of LabKey's EHR can adapt it to their systems and put it
#' in their home directory. Instructions are embedded as comments
#' within the file.
#' @examples
#' library(nprcgenekeepr)
#' data("exampleNprcgenekeeprConfig")
#' head(exampleNprcgenekeeprConfig)
"exampleNprcgenekeeprConfig"
#' examplePedigree is a pedigree object created by \code{qcStudbook}
#'
#' Represents pedigree from \emph{ExamplePedigree.csv}.
#' \describe{
#' \item{id}{-- character column of animal IDs}
#' \item{sire}{-- the male parent of the animal indicated by the \code{id}
#' column. Unknown sires are indicated with \code{NA}}
#' \item{dam}{-- the female parent of the animal indicated by the \code{id}
#' column.Unknown dams are indicated with \code{NA}}
#' \item{sex}{-- factor with levels: "M", "F", "U". Sex specifier for an
#' individual.}
#' \item{gen}{-- generation number (integers beginning with 0 for the founder
#' generation) of the animal indicated by the \code{id} column.}
#' \item{birth}{-- Date vector of birth dates}
#' \item{exit}{-- Date vector of exit dates}
#' \item{age}{-- numerical vector of age in years}
#' \item{ancestry}{-- character vector or NA with free-form text providing
#' information about the geographic population of origin.}
#' \item{origin}{-- character vector or \code{NA} (optional) that indicates
#' the name of the facility that the individual was imported from if other than
#' local.}
#' \item{status}{-- character vector or NA. Flag indicating an individual's
#' status as alive, dead, sold, etc. Transformed to
#'  factor \{levels: ALIVE, DECEASED, SHIPPED, UNKNOWN\}. Vector of
#' standardized status codes with the possible values
#' ALIVE, DECEASED, SHIPPED, or UNKNOWN}
#' \item{recordStats}{-- character vector with value of \code{"added"} or
#'  \code{"original"}.}
#' }
#' @examples
#' library(nprcgenekeepr)
#' data("examplePedigree")
#' exampleTree <- createPedTree(examplePedigree)
#' exampleLoops <- findLoops(exampleTree)
"examplePedigree"
#' finalRpt is a list object created from the list object \emph{rpt} prepared
#' by \code{reportGV}. It is created inside \code{orderReport}. This version
#' is at the state just prior to calling \code{rankSubjects} inside
#' \code{orderReport}.
#' @examples
#' library(nprcgenekeepr)
#' data("finalRpt")
#' finalRpt <- rankSubjects(finalRpt)
"finalRpt"
#' focalAnimals is a dataframe with one column (_id_) containing the of animal
#' Ids from the __examplePedigree__ pedigree.
#'
#' They can be used to illustrate the identification of a population of
#' interest as is shown in the example below.
#' @examples
#' library(nprcgenekeepr)
#' data("focalAnimals")
#' data("examplePedigree")
#' any(names(examplePedigree) == "population")
#' nrow(examplePedigree)
#' examplePedigree <- setPopulation(
#'   ped = examplePedigree,
#'   ids = focalAnimals$id
#' )
#' any(names(examplePedigree) == "population")
#' nrow(examplePedigree)
#' nrow(examplePedigree[examplePedigree$population, ])
"focalAnimals"
#' lacy1989Ped small hypothetical pedigree
#'
#' @source lacy1989Ped is a dataframe containing the small hypothetical
#' pedigree of three founders and four descendants used
#' by Robert C. Lacy in "Analysis of Founder Representation in Pedigrees:
#' Founder Equivalents and Founder Genome Equivalents" Zoo Biology 8:111-123
#' (1989).
#'
#' The founders (\code{A}, \code{B}, \code{E}) have unknown parentages and are
#' assumed to have independent ancestries.
#' \describe{
#' \item{id}{character column of animal IDs}
#' \item{sire}{the male parent of the animal indicated by the \code{id} column.
#' Unknown sires are indicated with \code{NA}}
#' \item{dam}{the female parent of the animal indicated by the \code{id}
#' column.Unknown dams are indicated with \code{NA}}
#' \item{gen}{generation number (integers beginning with 0 for the founder
#' generation) of the animal indicated by the \code{id} column.}
#' \item{population}{logical vector with all values set TRUE}
#' }
"lacy1989Ped"
#' lacy1989PedAlleles is a dataframe produced by \code{geneDrop} on
#' \code{lacy1989Ped} with 5000 iterations.
#'
#' @source lacy1989Ped is a dataframe containing the small example pedigree used
#' by Robert C. Lacy in "Analysis of Founder Representation in Pedigrees:
#' Founder Equivalents and Founder Genome Equivalents" Zoo Biology 8:111-123
#' (1989).
#'
#' \describe{
#' There are 5000 columns, one for each iteration in \code{geneDrop}
#' containing alleles randomly selected at each
#' generation of the pedigree using Mendelian rules.
#'
#' Column 5001 is the \code{id} column with two rows for each member of the
#' pedigree (2 * 7).
#'
#' Column 5002 is the \code{parent} column with values of \code{sire} and
#' \code{dam} alternating.
#' }
"lacy1989PedAlleles"
#' ped1Alleles is a dataframe created by the geneDrop function
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' @format A dataframe with 554 rows and 6 variables
#' \describe{
#' \item{V1}{alleles assigned to the parents of the animals identified in
#' the \code{id} column during iteration 1 of gene dropping performed by
#' \code{geneDrop}.}
#' \item{V2}{alleles assigned to the parents of the animals identified in
#' the \code{id} column during iteration 1 of gene dropping performed by
#' \code{geneDrop}.}
#' \item{V3}{alleles assigned to the parents of the animals identified in
#' the \code{id} column during iteration 1 of gene dropping performed by
#' \code{geneDrop}.}
#' \item{V4}{alleles assigned to the parents of the animals identified in
#' the \code{id} column during iteration 1 of gene dropping performed by
#' \code{geneDrop}.}
#' \item{id}{character vector of animal IDs provided to the gene dropping
#' function \code{geneDrop}.}
#' \item{parent}{the parent type ("sire" or "dam") of the parent who supplied
#' the alleles as assigned during each of the 4 gene dropping iterations
#' performed by \code{geneDrop}.}
#' }
#' @source example baboon pedigree file provided by Deborah Newman,
#' Southwest National Primate Center.
"ped1Alleles"
#' pedDuplicateIds is a dataframe with 9 rows and 5 columns (ego_id, sire,
#' dam_id, sex, birth_date) representing a full pedigree with a duplicated
#' record.
#'
#' It is one of six pedigrees (\code{pedDuplicateIds},
#' \code{pedFemaleSireMaleDam}, \code{pedgood},
#' \code{pedInvalidDates}, \code{pedMissingBirth},
#' \code{pedSameMaleIsSireAndDam}) used to
#' demonstrate error detection by the qcStudbook function.
"pedDuplicateIds"
#' pedFemaleSireMaleDam is a dataframe with 8 rows and 5 columns (ego_id, sire,
#' dam_id, sex, birth_date) representing a full pedigree with the errors of
#' having a sire labeled as female and a dam labeled as male.
#'
#' It is one of six pedigrees (\code{pedDuplicateIds},
#' \code{pedFemaleSireMaleDam}, \code{pedgood},
#' \code{pedInvalidDates}, \code{pedMissingBirth},
#' \code{pedSameMaleIsSireAndDam}) used to
#' demonstrate error detection by the qcStudbook function.
"pedFemaleSireMaleDam"
#' pedGood is a dataframe with 8 rows and 5 columns (ego_id, sire, dam_id,
#' sex, birth_date) representing a full pedigree with no errors.
#'
#' It is one of six pedigrees (\code{pedDuplicateIds},
#' \code{pedFemaleSireMaleDam}, \code{pedgood},
#' \code{pedInvalidDates}, \code{pedMissingBirth},
#' \code{pedSameMaleIsSireAndDam}) used to
#' demonstrate error detection by the qcStudbook function.
"pedGood"
#' pedInvalidDates is a dataframe with 8 rows and 5 columns (ego_id, sire,
#' dam_id, sex, birth_date) representing a full pedigree with values in the
#' \code{birth_date} column that are not valid dates.
#'
#' It is one of six pedigrees (\code{pedDuplicateIds},
#' \code{pedFemaleSireMaleDam}, \code{pedgood},
#' \code{pedInvalidDates}, \code{pedMissingBirth},
#' \code{pedSameMaleIsSireAndDam}) used to
#' demonstrate error detection by the qcStudbook function.
"pedInvalidDates"
#' pedMissingBirth is a dataframe with 8 rows and 5 columns (ego_id, sire,
#' dam_id, sex, birth_date) representing a full pedigree with no errors.
#'
#' It is one of six pedigrees (\code{pedDuplicateIds},
#' \code{pedFemaleSireMaleDam}, \code{pedgood},
#' \code{pedInvalidDates}, \code{pedMissingBirth},
#' \code{pedSameMaleIsSireAndDam}) used to
#' demonstrate error detection by the qcStudbook function.
"pedMissingBirth"
#' pedOne is a loadable version of a pedigree file fragment used for testing
#' and demonstration
#'
#' This is used for testing and demonstration.
#' @examples
#' library(nprcgenekeepr)
#' data("pedOne")
#' head(pedOne)
"pedOne"
#' pedSameMaleIsSireAndDam is a dataframe with 8 rows and 5 columns (ego_id,
#' sire, dam_id, sex, birth_date) representing a full pedigree with no errors.
#'
#' It is one of six pedigrees (\code{pedDuplicateIds},
#' \code{pedFemaleSireMaleDam}, \code{pedgood},
#' \code{pedInvalidDates}, \code{pedMissingBirth},
#' \code{pedSameMaleIsSireAndDam}) used to
#' demonstrate error detection by the qcStudbook function.
"pedSameMaleIsSireAndDam"
#' pedSix is a loadable version of a pedigree file fragment used for testing
#' and demonstration
#'
#' This is used for testing and demonstration.
#' @examples
#' library(nprcgenekeepr)
#' data("pedSix")
#' head(pedSix)
"pedSix"
#' pedWithGenotype is a dataframe produced from qcPed by adding made up
#' genotypes.
#'
#' \describe{
#' A dataframe containing 280 records with 12 columns: \code{id}, \code{sire},
#'  \code{dam}, \code{sex}, \code{gen}, \code{birth}, \code{exit}, \code{age},
#'  \code{first}, \code{second}, \code{first_name}, and \code{second_name}.
#' }
"pedWithGenotype"
#' pedWithGenotypeReport is a list containing the output of \code{reportGV}.
#'
#' @source pedWithGenotypeReport was made with pedWithGenotype as input into
#' reportGV with 10,000 iterations.
#'
#' pedWithGenotypeReport is a simple example report for use in
#' examples and unit tests.
#' It was created using the following commands.
#'   \itemize{
#'     \item set_seed(10)
#'     \item pedWithGenotypeReport <- reportGV(nprcgenekeepr::pedWithGenotype,
#'           guIter = 10000)
#'     \item save(pedWithGenotypeReport,
#'                file = "data/pedWithGenotypeReport.RData")
#'   }
#'
#' @examples
#' pedWithGenotypeReport <- nprcgenekeepr::pedWithGenotypeReport
"pedWithGenotypeReport"
#' qcBreeders is a list of 29 baboon IDs that are potential breeders
#'
#' @source qcBreeders is a list of 3 males and 26 females from
#' the \code{qcPed} data set.
#'
#' \describe{
#' These 29 animal IDs are used for examples and unit tests.
#' They were initially selected for having low kinship coefficients.
#' }
"qcBreeders"
#' qcPed is a dataframe with 277 rows and 6 columns
#'
#' \describe{
#' \item{id}{character column of animal IDs}
#' \item{sire}{the male parent of the animal indicated by the \code{id} column.}
#' \item{dam}{the female parent of the animal indicated by the \code{id}
#' column.}
#' \item{sex}{sex of the animal indicated by the \code{id} column.}
#' \item{gen}{generation number (integers beginning with 0 for the founder
#' generation) of the animal indicated by the \code{id} column.}
#' \item{birth}{birth date in \code{Date} format of the animal indicated by the
#'  \code{id} column.}
#' \item{exit}{exit date in \code{Date} format of the animal indicated by the
#'  \code{id} column.}
#' \item{age}{age in year (numeric) of the animal indicated by the \code{id}
#' column.}
#' }
"qcPed"
#' qcPedGvReport is a genetic value report
#'
#'
#' qcPedGvReport is a genetic value report for illustrative purposes only.
#' It is used in examples and unit tests with the nprcgenekeepr package.
#' It was created using the following commands.
#'   \itemize{
#'     \item set_seed(10)
#'     \item qcPedGvReport <- reportGV(nprcgenekeepr::qcPed, guIter = 10000)
#'     \item save(qcPedGvReport, file = "data/qcPedGvReport.RData")
#'   }
#'
#' @examples
#' qcPedGvReport <- nprcgenekeepr::qcPedGvReport
"qcPedGvReport"
#' smallPed is a hypothetical pedigree
#'
#' It has the following structure:
#' structure(list(id = c("A", "B", "C", "D", "E", "F", "G", "H",
#' "I", "J", "K", "L", "M", "N", "O", "P", "Q"), sire = c("Q", NA,
#' "A", "A", NA, "D", "D", "A", "A", NA, NA, "C", "A", NA, NA, "M", NA),
#' dam = c(NA, NA, "B", "B", NA, "E", "E", "B", "J", NA, NA,
#' "K", "N", NA, NA, "O", NA), sex = c("M", "F", "M", "M", "F",
#'  "F", "F", "M", "F", "F", "F", "F", "M", "F", "F", "F", "M"),
#'   gen = c(1, 1, 2, 2, 1, 3, 3, 2, 2, 1, 1, 2, 1, 1, 2, 3, 0),
#'   population = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
#'   TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
#'   .Names = c("id", "sire", "dam", "sex", "gen", "population"),
#'   row.names = c(NA, -17L), class = "data.frame")
"smallPed"
#' smallPedTree is a pedigree tree made from \code{smallPed}
#'
#' Access it using the following commands.
#' @examples
#' library(nprcgenekeepr)
#' data("smallPedTree")
"smallPedTree"
#' rhesusGenotypes is a dataframe with two haplotypes per animal
#'
#' There are  object.
#'
#' Represents 31 animals that are also in the obfuscated \code{rhesusPedigree}
#' pedigree from \emph{rhesusGenotypes.csv}.
#' \describe{
#' \item{id}{-- character column of animal IDs}
#' \item{first_name}{-- a generic name for the first haplotype}
#' \item{second_name}{-- a generic name for the second haplotype}
#' }
#' @examples
#' library(nprcgenekeepr)
#' data("rhesusGenotypes")
"rhesusGenotypes"
#' rhesusPedigree is a pedigree object
#'
#' Represents an obfuscated pedigree from \emph{rhesusPedigree.csv} where the
#' IDs and dates have been modified to de-identify the data.
#' \describe{
#' \item{id}{-- character column of animal IDs}
#' \item{sire}{-- the male parent of the animal indicated by the \code{id}
#' column. Unknown sires are indicated with \code{NA}}
#' \item{dam}{-- the female parent of the animal indicated by the \code{id}
#' column.Unknown dams are indicated with \code{NA}}
#' \item{sex}{-- factor with levels: "M", "F", "U". Sex specifier for an
#' individual.}
#' \item{gen}{-- generation number (integers beginning with 0 for the founder
#' generation) of the animal indicated by the \code{id} column.}
#' \item{birth}{-- Date vector of birth dates}
#' \item{exit}{-- Date vector of exit dates}
#' \item{age}{-- numerical vector of age in years}
#' }
#' @examples
#' library(nprcgenekeepr)
#' data("rhesusPedigree")
"rhesusPedigree"
