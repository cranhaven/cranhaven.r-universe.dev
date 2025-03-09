#' Peptide-level: Quantitative precision
#'
#' Calculate quantitative precision on peptide-level
#'
#' For each submitted data the coefficient of variation is calculated on peptide-level for LFQ intensities. Only full profiles are included.
#'
#' @param input_list A list with data frames and respective quantitative peptide information.
#'
#' @author Oliver Kardell
#'
#' @import stringr
#' @importFrom magrittr %>%
#'
#' @return This function returns the original submitted data of the \code{input_list} including a new output column:
#' \itemize{
#'  \item CV_Peptide_LFQ_mpwR - coefficient of variation in percentage.
#' }
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(stringr)
#' library(magrittr)
#' library(tibble)
#'
#' # Example data
#' set.seed(123)
#' data <- list(
#'   Spectronaut = list(
#'      filename = "C",
#'      software = "Spectronaut",
#'      data = list(
#'         "Spectronaut" = tibble::tibble(
#'            Run_mpwR = rep(c("A","B"), times = 5),
#'            Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
#'            Stripped.Sequence_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
#'            Peptide.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
#'            ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
#'            Retention.time_mpwR = sample(1:20, 10),
#'            Peptide_LFQ_mpwR = sample(1:30, 10),
#'            ProteinGroup_LFQ_mpwR = sample(1:30, 10))
#'      )
#'   )
#' )
#'
#' # Result
#' output <- get_CV_LFQ_pep(
#'   input_list = data
#' )

get_CV_LFQ_pep <- function(input_list) {

 #dependency ===
   cols_MQ_pep <- c("Stripped.Sequence_mpwR")
   cols_MQ_pepLFQ <- c("LFQ intensity")

   cols <- c("Run_mpwR", "Peptide_LFQ_mpwR", "Stripped.Sequence_mpwR")

 #handle global vars
 . <- NULL

 output_list <- list()

 for (i in seq_len(length(input_list))) {
   if (input_list[[i]][["software"]] == "Spectronaut") {
      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["Spectronaut"]]) %in% cols) != length(cols)) {
         stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==

     output_list[[i]] <- calculate_CV(input_df = input_list[[i]][["data"]][["Spectronaut"]], analysis_name = input_list[[i]][["filename"]], cv_col = "Peptide_LFQ")
     names(output_list)[i] <- input_list[[i]][["filename"]]
     next
   } else if (input_list[[i]][["software"]] == "MaxQuant") {
      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["pep"]]) %in% cols_MQ_pep) != length(cols_MQ_pep) | sum(stringr::str_detect(string = colnames(input_list[[i]][["data"]][["pep"]]), pattern = cols_MQ_pepLFQ)) == 0) {
         stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==

     output_list[[i]] <- tidy_MQ_LFQ(input_df = input_list[[i]][["data"]][["pep"]], cv_col = "Peptide_LFQ") %>%
        calculate_CV(input_df = ., analysis_name = input_list[[i]][["filename"]], cv_col = "Peptide_LFQ")
     names(output_list)[i] <- input_list[[i]][["filename"]]
     next
   } else if (input_list[[i]][["software"]] == "DIA-NN" | input_list[[i]][["software"]] == "PD") {
      if (input_list[[i]][["software"]] == "DIA-NN") {
         message("For DIA-NN no quantitative LFQ data on peptide-level.")
      } else if (input_list[[i]][["software"]] == "PD") {
         message("For PD no quantitative LFQ data on peptide-level.")
      }
      output_list[i] <- NA
      names(output_list)[i] <- NA
   } else if (input_list[[i]][["software"]] == "Generic") {
     #check cols
     if (sum(colnames(input_list[[i]][["data"]][["Generic"]]) %in% cols) != length(cols)) {
       stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
     }
     #==

     output_list[[i]] <- calculate_CV(input_df = input_list[[i]][["data"]][["Generic"]], analysis_name = input_list[[i]][["filename"]], cv_col = "Peptide_LFQ")
     names(output_list)[i] <- input_list[[i]][["filename"]]
     next
   }
 }

 output_list <- output_list[which(!is.na(names(output_list)))] #remove NAs - for each PD and DIA-NN entry

 return(output_list)
}
