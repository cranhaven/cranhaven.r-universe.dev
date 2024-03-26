#' @docType data
#' @name metafile384
#' @aliases metafile384
#' @title Metadata of 384 Well Plate
#'
#' @format { A data frame with 384 rows and 8 variables.
#' \describe{
#'  \item{row}{Row number of multi well well plate.}
#'  \item{col}{Column number of multi well plate.}
#'  \item{position}{Well position address of multi well plate.}
#'  \item{cell}{Type of cells used for the assay.}
#'  \item{compound}{Different drugs (drug1,drug2,etc) used for the assay.}
#'  \item{concentration}{'C1','C2','C3' etc represent different concentration used for the same compound. 'B' represent blank wells}
#'  \item{type}{'treated' and 'untreated' shows if the wells had received pretreatment (example:inhibitors) or not. 'Blank1','Blank2','Blank3' etc represent separate blanks for different drugs.}
#'  \item{dilution}{dilution of samples used for the assay.}
#'  }
#'  }
#'
#'@source User generated metadata of the 384 well plate.
#'
#'@usage metafile384
#'
#'@description A dataset containing metadata.
#'
#'@keywords datasets
#'
NULL
