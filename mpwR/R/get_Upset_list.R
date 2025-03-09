#' Generate Upset list
#'
#' Generate a list as input for Upset plot
#'
#' An input is generated for Upset plotting for either precursor-, peptide-, protein- or proteingroup-level. For inter-software comparisons flowTraceR is integrated.
#'
#' @param input_list A list with data frames and respective level information.
#' @param level Character string. Choose between "Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs". Default is "Precursor.IDs".
#' @param percentage_runs Number. Percentage of appearance in runs. 100 means: Identification is present in 100% of runs. Default is 100.
#' @param flowTraceR Logical. If FALSE no level conversion is applied. Useful for inter-software comparisons. Default is FALSE.
#' @param remove_traceR_unknownMods Logical. If FALSE no unknown Modifications are filtered out. Only applies if flowTraceR is set to TRUE. Default is FALSE.
#'
#' @author Oliver Kardell
#'
#' @import stringr
#' @importFrom magrittr %>%
#'
#' @return This function returns a list for each analysis with respective level information.
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
#' DIANN = list(
#'  filename = "B",
#'  software = "DIA-NN",
#'  data = list(
#'    "DIA-NN" = tibble::tibble(
#'      Run_mpwR = rep(c("A","B"), times = 10),
#'      Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
#'      Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
#'      Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 4),
#'      ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
#'    )
#'  )
#' ),
#' Spectronaut = list(
#'  filename = "C",
#'  software = "Spectronaut",
#'  data = list(
#'    "Spectronaut" = tibble::tibble(
#'      Run_mpwR = rep(c("A","B"), times = 15),
#'      Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 6),
#'      Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 6),
#'      ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 6)
#'    )
#'  )
#' )
#' )
#'
#' # Result
#' output <- get_Upset_list(
#'   input_list = data,
#'   level = "Precursor.IDs"
#' )

get_Upset_list <- function(input_list,
                           level = c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs"),
                           percentage_runs = 100,
                           flowTraceR = FALSE,
                           remove_traceR_unknownMods = FALSE) {

  #dependency ===
  if (level[1] %in% c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs") == FALSE) {
    stop("Please check your level entry - only use Precursor.IDs, Peptide.IDs, Protein.IDs or ProteinGroup.IDs")
  }

  #dependency ===
  cols_MQ_ev <- c("Run_mpwR", "Precursor.IDs_mpwR", "Peptide.IDs_mpwR", "Protein.IDs_mpwR")
  cols_MQ_pg <- c("ProteinGroup.IDs_mpwR")
  cols_MQ_pg_intensity <- c("Intensity")

  cols_PD_prot <- c("Run_mpwR", "Protein.IDs_mpwR")
  cols_PD_psm <- c("Run_mpwR", "Precursor.IDs_mpwR", "Peptide.IDs_mpwR")

  cols_spec <- c("Run_mpwR", "Precursor.IDs_mpwR", "Peptide.IDs_mpwR", "ProteinGroup.IDs_mpwR")

  cols <- c("Run_mpwR", "Precursor.IDs_mpwR", "Peptide.IDs_mpwR", "Protein.IDs_mpwR", "ProteinGroup.IDs_mpwR")
  #===

  #handle global vars
  . <- NULL

  output_list <- list()

  for (i in seq_len(length(input_list))) {
    if (input_list[[i]][["software"]] == "DIA-NN") {
      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["DIA-NN"]]) %in% cols) != length(cols)) {
        stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==

      output_list[[i]] <- prepare_Upset(input_df = input_list[[i]][["data"]][["DIA-NN"]], software = "DIA-NN", level = level[1], percentage_runs = percentage_runs, flowTraceR = flowTraceR, remove_traceR_unknownMods = remove_traceR_unknownMods)
      names(output_list)[i] <- input_list[[i]][["filename"]]
      next
    } else if (input_list[[i]][["software"]] == "Spectronaut") {

      if (level[1] == "Protein.IDs") {
        message("Protein-level not available for Spectronaut.")
        next
      }

      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["Spectronaut"]]) %in% cols_spec) != length(cols_spec)) {
        stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==
      output_list[[i]] <- prepare_Upset(input_df = input_list[[i]][["data"]][["Spectronaut"]], software = "Spectronaut", level = level[1], percentage_runs = percentage_runs, flowTraceR = flowTraceR, remove_traceR_unknownMods = remove_traceR_unknownMods)
      names(output_list)[i] <- input_list[[i]][["filename"]]
      next
    } else if (input_list[[i]][["software"]] == "MaxQuant") {
      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["ev"]]) %in% cols_MQ_ev) != length(cols_MQ_ev) | sum(stringr::str_detect(string = colnames(input_list[[i]][["data"]][["pg"]]), pattern = cols_MQ_pg)) == 0 | sum(stringr::str_detect(string = colnames(input_list[[i]][["data"]][["pg"]]), pattern = cols_MQ_pg_intensity)) == 0) {
        stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==

      if (level[1] == "ProteinGroup.IDs") {
        output_list[[i]] <- tidy_MQ_pep_pg(input_list[[i]][["data"]][["pg"]]) %>%
          prepare_Upset(input_df = ., software = "MaxQuant", level = level[1], percentage_runs = percentage_runs, flowTraceR = flowTraceR, remove_traceR_unknownMods = remove_traceR_unknownMods)
        names(output_list)[i] <- input_list[[i]][["filename"]]
        next
      } else if (level[1] == "Precursor.IDs" | level[1] == "Peptide.IDs" | level[1] == "Protein.IDs") {
        output_list[[i]] <- prepare_Upset(input_df = input_list[[i]][["data"]][["ev"]], software = "MaxQuant", level = level[1], percentage_runs = percentage_runs, flowTraceR = flowTraceR, remove_traceR_unknownMods = remove_traceR_unknownMods)
        names(output_list)[i] <- input_list[[i]][["filename"]]
        next
      }

    } else if (input_list[[i]][["software"]] == "PD") {

      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["psm"]]) %in% cols_PD_psm) != length(cols_PD_psm) | sum(colnames(input_list[[i]][["data"]][["prot"]]) %in% cols_PD_prot) != length(cols_PD_prot))  {
        stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==

      if (level[1] == "Protein.IDs") {
        output_list[[i]] <- prepare_Upset(input_df = input_list[[i]][["data"]][["prot"]], software = "PD", level = level[1], percentage_runs = percentage_runs, flowTraceR = flowTraceR, remove_traceR_unknownMods = remove_traceR_unknownMods)
        names(output_list)[i] <- input_list[[i]][["filename"]]

      } else if (level[1] == "Precursor.IDs" | level[1] == "Peptide.IDs" | level[1] == "ProteinGroup.IDs") {
        output_list[[i]] <- prepare_Upset(input_df = input_list[[i]][["data"]][["psm"]], software = "PD", level = level[1], percentage_runs = percentage_runs, flowTraceR = flowTraceR, remove_traceR_unknownMods = remove_traceR_unknownMods)
        names(output_list)[i] <- input_list[[i]][["filename"]]
        next
      }
    } else if (input_list[[i]][["software"]] == "Generic") {
      #check cols
      if (sum(colnames(input_list[[i]][["data"]][["Generic"]]) %in% cols) != length(cols)) {
        stop(paste0("Not all required columns - wrong input_list? Check position ", i, " in input_list."))
      }
      #==

      output_list[[i]] <- prepare_Upset(input_df = input_list[[i]][["data"]][["Generic"]], software = "Generic", level = level[1], percentage_runs = percentage_runs, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE)
      names(output_list)[i] <- input_list[[i]][["filename"]]

      if (flowTraceR == TRUE) {
        message(paste0("flowTraceR not available for generic input. No conversion applied for position ", i, " in input_list."))
      }

      next
    }
  }

  output_list <- output_list[names(output_list) != ""] #NULL entries for Spectronaut - Protein.IDs - in names: ""

  return(output_list)

}
