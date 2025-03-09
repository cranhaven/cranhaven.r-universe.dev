#' Report about Missed Cleavages
#'
#' Generates report with information about number of missed cleavages
#'
#' For each submitted data a report is generated with information about the number of missed cleavages.
#'
#' @param input_list A list with data frames and respective missed cleavage information.
#' @param metric \code{"absolute"} for absolute numbers or \code{"percentage"} for displaying percentages. Default is absolute.
#'
#' @author Oliver Kardell
#'
#' @import stringr
#' @importFrom magrittr %>%
#'
#' @return This function returns a list. For each analysis a respective data frame including information of missed cleavages is stored in the generated list.
#' \itemize{
#'  \item Analysis - analysis name.
#'  \item Missed.Cleavage - categorical entry with number of missed cleavages.
#'  \item mc_count - number of missed cleavages per categorical missed cleavage entry - absolute or in percentage.
#' }
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tibble)
#' library(magrittr)
#' library(stringr)
#'
#' # Example data
#' data <- list(
#' Spectronaut = list(
#'   filename = "C",
#'   software = "Spectronaut",
#'   data = list(
#'      "Spectronaut" = tibble::tibble(
#'         Stripped.Sequence_mpwR = c("A", "B", "C", "D", "E"),
#'         Missed.Cleavage_mpwR = c(0, 1, 1, 2, 2)
#'      )
#'   )
#' )
#' )
#'
#' # Result
#' output <- get_MC_Report(
#'   input_list = data,
#'   metric = "absolute"
#' )

get_MC_Report <- function(input_list,
                           metric = c("absolute", "percentage")) {

   #dependency ===
   if (metric[1] %in% c("absolute", "percentage") == FALSE) {
      stop("Please check your metric entry - only use absolute or percentage")
   }

   cols <- c("Stripped.Sequence_mpwR", "Missed.Cleavage_mpwR")

   cols_DIANN <- c("Stripped.Sequence_mpwR")
   cols_MQ_pep <- c("Intensity")
   #===

   #handle global vars
   . <- NULL

 output_list <- list()

if (metric[1] == "percentage") {

   for (i in seq_len(length(input_list))) {
      if (input_list[[i]][["software"]] == "DIA-NN" | input_list[[i]][["software"]] == "Spectronaut") {
         #check cols
         if (sum(colnames(input_list[[i]][["data"]][[1]]) %in% cols_DIANN) != length(cols_DIANN)) {
            stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
         }
         #==
         output_list[[i]] <- generate_MC_Report(input_df = input_list[[i]][["data"]][[1]], analysis_name = input_list[[i]][["filename"]], software = input_list[[i]][["software"]], metric = "percentage")
         names(output_list)[i] <- input_list[[i]][["filename"]]
         next
      } else if (input_list[[i]][["software"]] == "MaxQuant") {
         #check cols
         if (sum(colnames(input_list[[i]][["data"]][["pep"]]) %in% cols) != length(cols) | sum(stringr::str_detect(string = colnames(input_list[[i]][["data"]][["pep"]]), pattern = cols_MQ_pep)) == 0) {
            stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
         }
         #==
         output_list[[i]] <- tidy_MQ_pep_pg(input_df = input_list[[i]][["data"]][["pep"]]) %>%
            generate_MC_Report(input_df = ., analysis_name = input_list[[i]][["filename"]], software = input_list[[i]][["software"]], metric = "percentage")
         names(output_list)[i] <- input_list[[i]][["filename"]]
         next
      } else if (input_list[[i]][["software"]] == "PD") {
         #check cols
         if (sum(colnames(input_list[[i]][["data"]][["pep"]]) %in% cols) != length(cols)) {
            stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
         }
         #==
         output_list[[i]] <-  generate_MC_Report(input_df = input_list[[i]][["data"]][["pep"]], analysis_name = input_list[[i]][["filename"]], software = input_list[[i]][["software"]], metric = "percentage")
         names(output_list)[i] <- input_list[[i]][["filename"]]
         next
      } else if (input_list[[i]][["software"]] == "Generic") {
        #check cols
        if (sum(colnames(input_list[[i]][["data"]][[1]]) %in% cols) != length(cols)) {
          stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
        }
        #==
        output_list[[i]] <- generate_MC_Report(input_df = input_list[[i]][["data"]][[1]], analysis_name = input_list[[i]][["filename"]], software = input_list[[i]][["software"]], metric = "percentage")
        names(output_list)[i] <- input_list[[i]][["filename"]]
        next
      }
   }

} else if (metric[1] == "absolute") {

   for (i in seq_len(length(input_list))) {
      if (input_list[[i]][["software"]] == "DIA-NN" | input_list[[i]][["software"]] == "Spectronaut") {
         #check cols
         if (sum(colnames(input_list[[i]][["data"]][[1]]) %in% cols_DIANN) != length(cols_DIANN)) {
            stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
         }
         #==
         output_list[[i]] <- generate_MC_Report(input_df = input_list[[i]][["data"]][[1]], analysis_name = input_list[[i]][["filename"]], software = input_list[[i]][["software"]], metric = "absolute")
         names(output_list)[i] <- input_list[[i]][["filename"]]
         next
      } else if (input_list[[i]][["software"]] == "MaxQuant") {
         #check cols
         if (sum(colnames(input_list[[i]][["data"]][["pep"]]) %in% cols) != length(cols) | sum(stringr::str_detect(string = colnames(input_list[[i]][["data"]][["pep"]]), pattern = cols_MQ_pep)) == 0) {
            stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
         }
         #==
         output_list[[i]] <- tidy_MQ_pep_pg(input_df = input_list[[i]][["data"]][["pep"]]) %>%
            generate_MC_Report(input_df = ., analysis_name = input_list[[i]][["filename"]], software = input_list[[i]][["software"]], metric = "absolute")
         names(output_list)[i] <- input_list[[i]][["filename"]]
         next
      } else if (input_list[[i]][["software"]] == "PD") {
         #check cols
         if (sum(colnames(input_list[[i]][["data"]][["pep"]]) %in% cols) != length(cols)) {
            stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
         }
         #==
         output_list[[i]] <- generate_MC_Report(input_df = input_list[[i]][["data"]][["pep"]], analysis_name = input_list[[i]][["filename"]], software = input_list[[i]][["software"]], metric = "absolute")
         names(output_list)[i] <- input_list[[i]][["filename"]]
         next
      } else if (input_list[[i]][["software"]] == "Generic") {
        #check cols
        if (sum(colnames(input_list[[i]][["data"]][[1]]) %in% cols) != length(cols)) {
          stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
        }
        #==
        output_list[[i]] <- generate_MC_Report(input_df = input_list[[i]][["data"]][[1]], analysis_name = input_list[[i]][["filename"]], software = input_list[[i]][["software"]], metric = "absolute")
        names(output_list)[i] <- input_list[[i]][["filename"]]
        next
      }
   }

}
 return(output_list)
}
