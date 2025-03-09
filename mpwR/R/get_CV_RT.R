#' Retention time precision
#'
#' Calculate retention time precision
#'
#' For each submitted data the coefficient of variation is calculated on precursor-level for retention time. Only full profiles are included.
#'
#' @param input_list A list with data frames and respective retention time information.
#'
#' @author Oliver Kardell
#'
#' @return This function returns the original submitted data of the \code{input_list} including a new output column:
#' \itemize{
#'  \item CV_Retention.time_mpwR - coefficient of variation in percentage.
#' }
#' @export
#'
#' @examples
#' # Load libraries
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
#' output <- get_CV_RT(
#'   input_list = data
#' )

get_CV_RT <- function(input_list) {

   #dependency ===
   cols <- c("Run_mpwR", "Precursor.IDs_mpwR", "Retention.time_mpwR")
   #===

 output_list <- list()

 for (i in seq_len(length(input_list))) {
   if (input_list[[i]][["software"]] == "DIA-NN") {
      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["DIA-NN"]]) %in% cols) != length(cols)) {
         stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==
     output_list[[i]] <- calculate_CV(input_df = input_list[[i]][["data"]][["DIA-NN"]], analysis_name = input_list[[i]][["filename"]], cv_col = "Retention.time")
     names(output_list)[i] <- input_list[[i]][["filename"]]
     next
   } else if (input_list[[i]][["software"]] == "Spectronaut") {
      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["Spectronaut"]]) %in% cols) != length(cols)) {
         stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==
     output_list[[i]] <- calculate_CV(input_df = input_list[[i]][["data"]][["Spectronaut"]], analysis_name = input_list[[i]][["filename"]], cv_col = "Retention.time")
     names(output_list)[i] <- input_list[[i]][["filename"]]
     next
   } else if (input_list[[i]][["software"]] == "MaxQuant") {
      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["ev"]]) %in% cols) != length(cols)) {
         stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==
     output_list[[i]] <- calculate_CV(input_df = input_list[[i]][["data"]][["ev"]], analysis_name = input_list[[i]][["filename"]], cv_col = "Retention.time")
     names(output_list)[i] <- input_list[[i]][["filename"]]
     next
   } else if (input_list[[i]][["software"]] == "PD") {
      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["psm"]]) %in% cols) != length(cols)) {
         stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==
     output_list[[i]] <- calculate_CV(input_df = input_list[[i]][["data"]][["psm"]], analysis_name = input_list[[i]][["filename"]], cv_col = "Retention.time")
     names(output_list)[i] <- input_list[[i]][["filename"]]
     next
   } else if (input_list[[i]][["software"]] == "Generic") {
     #check cols
     if (sum(colnames(input_list[[i]][["data"]][["Generic"]]) %in% cols) != length(cols)) {
       stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
     }
     #==
     output_list[[i]] <- calculate_CV(input_df = input_list[[i]][["data"]][["Generic"]], analysis_name = input_list[[i]][["filename"]], cv_col = "Retention.time")
     names(output_list)[i] <- input_list[[i]][["filename"]]
     next
   }
 }

 return(output_list)
}
