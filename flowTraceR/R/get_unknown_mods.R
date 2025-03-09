#' Check of converted modifications
#'
#' Check if conversion to UniMod-format of identified modifications is successful.
#'
#' After conversion to standardized format by convert_precursor or convert_modified_peptides, entries with modifications are checked for a successful conversion. Conversion of modifications is currently only available for UniMod:35 and UniMod:4. Other modifications will not be converted to standardized format.
#'
#' @param input_string character column traceR_precursor as string.
#' @param pattern_start character of software-dependent beginning of representation of modifications.
#' @param pattern_end character of software-dependent end of representation of modifications.
#'
#' @author Oliver Kardell
#'
#' @import dplyr
#' @import stringr
#'
#' @return This function returns vector with logical values. This function is incorporated in the functions convert_precursor and convert_modified_peptides; used to generate the unknownMods column : if TRUE: a modification is detected, which is not converted to a standardized text.
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(dplyr)
#' library(stringr)
#' library(tibble)
#'
#' # Generate data
#' data <- tibble::tibble(
#'   "traceR_precursor" = c("AACLLPK",
#'    "ALTDM(UniMod:35)PQM(UniMod:35)R2",
#'    "ALTDM(DummyModification)PQMK3")
#' )
#'
#' # Unknown modifications present?
#' get_unknown_mods(input_string = data$traceR_precursor, pattern_start= "(", pattern_end = ")")


get_unknown_mods <- function(input_string,
                             pattern_start,
                             pattern_end) {

  mod_4 <- tibble::tibble("mod_4" = stringr::str_count(string = input_string, pattern = paste0("\\", pattern_start, "UniMod\\:4\\", pattern_end)))
  mod_35 <- tibble::tibble("mod_35" = stringr::str_count(string = input_string, pattern =  paste0("\\", pattern_start, "UniMod\\:35\\", pattern_end)))
  mod_all <- tibble::tibble("mod_all" = stringr::str_count(string = input_string, pattern =  paste0("\\", pattern_start))) #N-Term(Prot)(Met-loss+Acetyl) - not real count of mods!

  mods <- dplyr::bind_cols(mod_4, mod_35, mod_all) %>%
    dplyr::mutate(
      mod_known = mod_4 + mod_35,
      traceR_precursor_unknownMods = dplyr::case_when(
        mod_all == mod_known ~ FALSE,
        mod_all != mod_known ~ TRUE
      )
    )

 return(mods$traceR_precursor_unknownMods)
}
