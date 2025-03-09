#' Report for identifications
#'
#' Generates a report for identifications
#'
#' For each submitted data a report with achieved number of identifications is generated on precursor-, peptide-, protein- and proteingroup-level.
#'
#' @param input_list A list with data frames and respective level information.
#'
#' @author Oliver Kardell
#'
#' @import stringr
#'
#' @return This function returns a list. For each analysis a respective data frame including number of identifications per run is stored in the generated list.
#' \itemize{
#'  \item Analysis - analysis name.
#'  \item Run - run information.
#'  \item Precursor.IDs - number of precursor identification.
#'  \item Peptide.IDs - number of peptide identification.
#'  \item Protein.IDs - number of protein identification.
#'  \item ProteinGroup.IDs - number of proteingroup identification.
#' }
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tibble)
#' library(stringr)
#'
#' # Example data
#' data <- list(
#'DIANN = list(
#'   filename = "B",
#'   software = "DIA-NN",
#'   data = list(
#'      "DIA-NN" = tibble::tibble(
#'         Run_mpwR = rep(c("A","B"), times = 10),
#'         Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
#'         Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
#'         Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 4),
#'         ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
#'      )
#'   )
#' )
#' )
#'
#' # Result
#' output <- get_ID_Report(
#'   input_list = data
#' )

get_ID_Report <- function(input_list) {

   #dependency ===
   cols_MQ_ev <- c("Run_mpwR", "Precursor.IDs_mpwR", "Peptide.IDs_mpwR", "Protein.IDs_mpwR")
   cols_MQ_pg <- c("ProteinGroup.IDs_mpwR")
   cols_MQ_pg_intensity <- c("Intensity ")

   cols_PD_prot <- c("Run_mpwR", "Protein.IDs_mpwR")
   cols_PD_pg <- c("Run_mpwR", "ProteinGroup.IDs_mpwR")
   cols_PD_psm <- c("Run_mpwR", "Precursor.IDs_mpwR", "Peptide.IDs_mpwR")

   cols_spec <- c("Run_mpwR", "Precursor.IDs_mpwR", "Peptide.IDs_mpwR", "ProteinGroup.IDs_mpwR")

   cols <- c("Run_mpwR", "Precursor.IDs_mpwR", "Peptide.IDs_mpwR", "Protein.IDs_mpwR", "ProteinGroup.IDs_mpwR")
   #===

 output_list <- list()

 for (i in seq_len(length(input_list))) {
   if (input_list[[i]][["software"]] == "DIA-NN") {
      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["DIA-NN"]]) %in% cols) != length(cols)) {
         stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==
     output_list[[i]] <- generate_ID_Report(input_df = input_list[[i]][["data"]][["DIA-NN"]], analysis_name = input_list[[i]][["filename"]], software = "DIA-NN")
     names(output_list)[i] <- input_list[[i]][["filename"]]
     next
   } else if (input_list[[i]][["software"]] == "Spectronaut") {
      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["Spectronaut"]]) %in% cols_spec) != length(cols_spec)) {
         stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==
     output_list[[i]] <- generate_ID_Report(input_df = input_list[[i]][["data"]][["Spectronaut"]], analysis_name = input_list[[i]][["filename"]], software = "Spectronaut")
     names(output_list)[i] <- input_list[[i]][["filename"]]
     next
   } else if (input_list[[i]][["software"]] == "MaxQuant") {
      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["ev"]]) %in% cols_MQ_ev) != length(cols_MQ_ev) | sum(stringr::str_detect(string = colnames(input_list[[i]][["data"]][["pg"]]), pattern = cols_MQ_pg)) == 0 | sum(stringr::str_detect(string = colnames(input_list[[i]][["data"]][["pg"]]), pattern = cols_MQ_pg_intensity)) == 0) {
         stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==
     output_list[[i]] <- generate_ID_Report(input_df = input_list[[i]][["data"]][["ev"]], input_MQ_proteingroup = input_list[[i]][["data"]][["pg"]], analysis_name = input_list[[i]][["filename"]], software = "MaxQuant")
     names(output_list)[i] <- input_list[[i]][["filename"]]
     next
   } else if (input_list[[i]][["software"]] == "PD") {
      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["psm"]]) %in% cols_PD_psm) != length(cols_PD_psm) | sum(colnames(input_list[[i]][["data"]][["prot"]]) %in% cols_PD_prot) != length(cols_PD_prot) | sum(colnames(input_list[[i]][["data"]][["pg"]]) %in% cols_PD_pg) != length(cols_PD_pg))  {
         stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==
     output_list[[i]] <- generate_ID_Report(input_df = input_list[[i]][["data"]][["psm"]], input_PD_protein = input_list[[i]][["data"]][["prot"]], input_PD_proteingroup = input_list[[i]][["data"]][["pg"]], analysis_name = input_list[[i]][["filename"]], software = "PD")
     names(output_list)[i] <- input_list[[i]][["filename"]]
     next
   } else if (input_list[[i]][["software"]] == "Generic") {
     #check cols
     if (sum(colnames(input_list[[i]][["data"]][["Generic"]]) %in% cols) != length(cols)) {
       stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
     }
     #==
     output_list[[i]] <- generate_ID_Report(input_df = input_list[[i]][["data"]][["Generic"]], analysis_name = input_list[[i]][["filename"]], software = "Generic")
     names(output_list)[i] <- input_list[[i]][["filename"]]
     next
   }
 }

 return(output_list)
}
