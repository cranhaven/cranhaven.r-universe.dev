#' Remove automatically generated IDs from pedigree
#'
#' Currently uses leading "U" to identify automatically generated IDs.
#' TODO change identification of automatically generated IDs from looking for
#' an initial "U" at the beginning of an ID to a function call so that actual
#' ID that start with a "U" are possible.
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information. The \code{id}, \code{sire}, and \code{dame} columns are
#' required.
#'
#' @return A pedigree with automatically generated IDs removed.
#' @export
#'
#' @examples
#' examplePedigree <- nprcgenekeepr::examplePedigree
#' length(examplePedigree$id)
#' ped <- removeAutoGenIds(examplePedigree)
#' length(ped$id)
#'
removeAutoGenIds <- function(ped) {
  ped <- ped[stri_sub(ped$id, 1L, 1L) != "U", ]
  ped$sire[stri_sub(ped$sire, 1L, 1L) == "U"] <- NA
  ped$dam[stri_sub(ped$dam, 1L, 1L) == "U"] <- NA
  ped
}
