#' Create generic template
#'
#' Generation of a template.csv file for generic input data. The template is provided in long-format.
#'
#' The generated template.csv file can be used to create a software-independent input file for mpwR. Example entries are provided. The template file - filename_Report.csv - is generated. The appendix "_Report" is required for importing with mpwR. Note that the template is in long-format, so each ProteinGroup.ID has possible multiple entries depending on the number of Precursor.IDs.
#'
#' @param path_filename Path to folder where template is generated and user-defined filename
#' @author Oliver Kardell
#'
#' @return This function returns a csv-file with the following columns:
#' \itemize{
#'  \item Run_mpwR - name of file(s).
#'  \item ProteinGroup.IDs_mpwR - ProteinGroup with identifier(s) of protein(s) contained in the protein group.
#'  \item Protein.IDs_mpwR - Protein identifier(s).
#'  \item Peptide.IDs_mpwR - Sequence representation plus possible post-translational modifications.
#'  \item Precursor.IDs_mpwR - Sequence representation plus possible post-translational modifications including charge state.
#'  \item Stripped.Sequence_mpwR - 	The amino acid sequence of the identified peptide without modifications.
#'  \item Precursor.Charge_mpwR - Charge state of the precursor.
#'  \item Missed.Cleavage_mpwR - Number of missed enzymatic cleavages.
#'  \item Retention.time_mpwR -  Retention time in minutes in the elution profile of the precursor ion.
#'  \item ProteinGroup_LFQ_mpwR - LFQ intensity column on proteingroup-level
#'  \item Peptide_LFQ_mpwR - LFQ intensity column on petide-level
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_generic_template(path = "DIRECTORY_WHERE_FILE_IS_GENERATED/filename")
#' }

write_generic_template <- function(path_filename) {

  template <- tibble::tibble(
          Run_mpwR = rep(c("R01", "R02"), each = 5),
          ProteinGroup.IDs_mpwR = c("A0A075B6I9;P04211", "A0A075B6H9", "A0A075B6K4;P01717", "A0A075B6K5", "A0A075B6K5",	"A0A075B6I9;P04211", "A0A075B6H9", "A0A075B6K4;P01717", "A0A075B6K5", "A0A075B6K5"),
          Protein.IDs_mpwR = c("A0A075B6I9;P04211", "A0A075B6H9", "A0A075B6K4;P01717", "A0A075B6K5", "A0A075B6K5",	"A0A075B6I9;P04211", "A0A075B6H9", "A0A075B6K4;P01717", "A0A075B6K5", "A0A075B6K5"),
          Peptide.IDs_mpwR = c("FSGSLLGGK", "FSGSRSSGAER", "ITC(UniMod:4)SGDALPK", "NVHWYQQK", "PGQAPVLVIYR", "FSGSLLGGK", "FSGSRSSGAER",	"ITC(UniMod:4)SGDALPK",	"NVHWYQQK",	"PGQAPVLVIYR"),
          Precursor.IDs_mpwR = c("FSGSLLGGK2", "FSGSRSSGAER2", "ITC(UniMod:4)SGDALPK2", "NVHWYQQK2", "PGQAPVLVIYR2", "FSGSLLGGK2", "FSGSRSSGAER2",	"ITC(UniMod:4)SGDALPK2",	"NVHWYQQK2",	"PGQAPVLVIYR2"),
          Stripped.Sequence_mpwR = c("FSGSLLGGK", "FSGSRSSGAER", "ITCSGDALPK", "NVHWYQQK", "PGQAPVLVIYR", "FSGSLLGGK", "FSGSRSSGAER",	"ITCSGDALPK",	"NVHWYQQK",	"PGQAPVLVIYR"),
          Precursor.Charge_mpwR = rep(2, each = 10),
          Missed.Cleavage_mpwR = c(0, 1, 0, 0, 0, 0, 1, 0, 0, 0),
          Retention.time_mpwR = c(50.1869, 16.8094, 32.4896, 30.1251, 71.7974, 50.2519, 16.8899, 32.393, 30.0295, 71.763),
          ProteinGroup_LFQ_mpwR = c(701407, 144256, 235109, 203237, 203237, 876683, 162304, 241585, 242318, 242318),
          Peptide_LFQ_mpwR = c(150000, 130000, 160000, 120000, 110000, 140000, 133000, 165000, 1220000, 111000)
        )

  utils::write.csv(template, file = paste0(path_filename, "_Report.csv"), row.names = FALSE)

}
