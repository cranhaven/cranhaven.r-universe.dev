#' Load and Prepare the input data
#'
#' Input data will be imported, renamed and default filtering will be applied
#'
#' Function for easily importing the default software outputs and preparing for downstream analysis with mpwR within one folder. As default for MaxQuant "Reverse", "Potential contaminants" and "Only identified by site" are filtered out. As default for PD only "High" confidence identifications are included and for Found in Sample column(s) also only "High" identifications. Contaminants are filtered out. As default for Spectronaut only EG.Identified equals TRUE are included.
#'
#' @param path Path to folder where the input data is stored - only input data. No subfolders or other files. Analysis name as prefix + for MaxQuant: _evidence, _peptides, _proteinGroups; for PD - R-friendly headers enabled: _PSMs, _Proteins, _PeptideGroups, _ProteinGroups; for DIA-NN, Spectronaut and Generic: _Report
#' @param diann_addon_pg_qval Numeric between 0 and 1. Applied only to DIA-NN data: `diann_addon_pg_qval` <= PG.Q.Value.
#' @param diann_addon_prec_qval Numeric between 0 and 1. Applied only to DIA-NN data: `diann_addon_prec_qval` <= Q.Value.
#'
#' @author Oliver Kardell
#'
#' @importFrom data.table fread
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import purrr
#'
#' @return A list - each list entry has filename and software info as well as stored data.
#' @export
#'
#' @examples
#' \dontrun{
#' prepare_mpwR(path = "DIRECTORY_TO_FILES")
#' }

prepare_mpwR <- function(path,
                         diann_addon_pg_qval = 0.01,
                         diann_addon_prec_qval = 0.01) {

  #**Here software columns to ID correct software**
  software <- list(
    Generic = c("Run_mpwR", "ProteinGroup.IDs_mpwR", "Peptide.IDs_mpwR", "Precursor.IDs_mpwR", "Stripped.Sequence_mpwR", "Precursor.Charge_mpwR", "Missed.Cleavage_mpwR", "Retention.time_mpwR", "ProteinGroup_LFQ_mpwR", "Peptide_LFQ_mpwR"),
    MaxQuant_ev = c("Protein group IDs", "Peptide ID"),
    MaxQuant_pep = c("Protein group IDs", "Evidence IDs"),
    MaxQuant_pg = c("Peptide IDs", "Evidence IDs"),
    `DIA-NN` = c("File.Name", "Run", "Protein.Group", "Protein.Ids"),
    Spectronaut = c("R.FileName", "PG.ProteinGroups", "EG.PrecursorId"),
    PD_psm = c("Confidence", "Spectrum File", "Annotated Sequence", "Modifications", "Number of Missed Cleavages", "Charge", "RT in min"),  #Apex RT in min
    PD_pep = c("Number of Protein Groups", "Number of Proteins", "Number of PSMs", "Confidence", "Sequence", "Modifications", "Number of Missed Cleavages"), #Annotated Sequence
    PD_prot = c("Proteins Unique Sequence ID", "Accession", "Description"), #Modifications
    PD_pg =  c("Protein Groups Protein Group ID", "Group Description", "Number of Proteins", "Number of Unique Peptides")
  )
  #****

  #handle global vars
  . <- NULL

  #get file names and path
  file_names <- list.files(path = path) %>%
    stringr::str_remove(string = ., pattern = "\\.[a-z]+") #remove file extensions like .txt
  file_paths <- list.files(path = path, full.names = TRUE)

  #import files
  files <- comprehenr::to_list(for (i in file_paths) import_mpwR(i))
  names(files) <- file_names

  #categorize - filename - data - software
  files_categorized <- list()
  appendix <- c("_evidence", "_peptides", "_proteinGroups", "_PSMs", "_Proteins", "_PeptideGroups", "_ProteinGroups", "_Report")

  #dependency - appendix ===
  sum_appendix <- 0
  for (i in seq_len(length(appendix))) {
    add <- sum(stringr::str_detect(string = file_names, pattern = paste0(appendix[i], "$")))
    sum_appendix <- sum_appendix + add
  }

  if (sum_appendix != length(file_names)) {
     stop("Unknown file appendix detected! Please use: _evidence, _peptides, _proteinGroups, _PSMs, _Proteins, _PeptideGroups, _ProteinGroups, _Report")
    }
  #===

  for (i in seq_len(length(files))) {
   for (x in seq_len(length(software))) {

     if (sum(software[[x]] %in% colnames(files[[i]])) == length(software[[x]])) {

       #remove software appendix of filename
       filename <- names(files[i])
       for (y in seq_len(length(appendix))) {
        filename <- stringr::str_remove(string = filename, pattern = appendix[y])
       }

       #generate categorized list
       files_categorized[[i]] <- list(
         filename = filename,
         data = files[[i]],
         type = stringr::str_remove(string = names(software[x]), pattern = "[A-z]+_"), #remove prefix of software entries
         software = stringr::str_remove(string = names(software[x]), pattern = "_[a-z]+") #remove appendix of software entries
         )
       next
     }
   }
  }

  #dependency - empty===
  #remove files with NULL (if ID cols are not detected)
  null_index <- sapply(files_categorized, is.null)
  files_categorized <- files_categorized %>%
    purrr::discard(is.null)

  file_names <- file_names[null_index]
  #===

  #get overview of software and filenames
  list_filename <- list()
  list_software <- list()
  for (i in seq_len(length(files_categorized))) {

    list_filename[[i]] <- files_categorized[[i]][["filename"]]
    list_software[[i]] <- files_categorized[[i]][["software"]]
  }

  overview <- bind_cols(
    tibble::tibble("filename" = unlist(list_filename)),
    tibble::tibble("software" = unlist(list_software))) %>%
    dplyr::group_by(filename, software) %>%
    dplyr::count(filename) %>%
    dplyr::rename("count" = n) %>%
    dplyr::ungroup()

  #dependency - correct number of files per analysis ===
  for (i in seq_len(nrow(overview))) {

    if (overview[i, "software", drop = TRUE] == "DIA-NN") {
      if (overview[i, "count", drop = TRUE] != 1) {
        stop_name <- overview[i, "filename", drop = TRUE]
        stop(paste0("Wrong number of analyses detected for ", stop_name, "! DIA-NN requires 1 input file. Remember: Use unique name for each analysis."))
      }
    } else if (overview[i, "software", drop = TRUE] == "MaxQuant") {
      if (overview[i, "count", drop = TRUE] != 3) {
        stop_name <- overview[i, "filename", drop = TRUE]
        stop(paste0("Wrong number of analyses detected for ", stop_name, "! MaxQuant requires 3 input files. Remember: Use unique name for each analysis."))
      }
    } else if (overview[i, "software", drop = TRUE] == "Spectronaut") {
      if (overview[i, "count", drop = TRUE] != 1) {
      stop_name <- overview[i, "filename", drop = TRUE]
      stop(paste0("Wrong number of analyses detected for ", stop_name, "! Spectronaut requires 1 input file. Remember: Use unique name for each analysis."))
      }
    } else if (overview[i, "software", drop = TRUE] == "PD") {
      if (overview[i, "count", drop = TRUE] != 4) {
      stop_name <- overview[i, "filename", drop = TRUE]
      stop(paste0("Wrong number of analyses detected for ", stop_name, "! PD requires 4 input files. Remember: Use unique name for each analysis."))
      }
    } else if (overview[i, "software", drop = TRUE] == "Generic") {
      if (overview[i, "count", drop = TRUE] != 1) {
        stop_name <- overview[i, "filename", drop = TRUE]
        stop(paste0("Wrong number of analyses detected for ", stop_name, "! Generic input requires 1 input file. Remember: Use unique name for each analysis."))
      }
    }
  }
  #===

  #order files
  ordered_files <- list()

  for (i in seq_len(nrow(overview))) {

    ordered_files[[i]] <- list(
      filename = overview[i, "filename", drop = TRUE],
      software = overview[i, "software", drop = TRUE],
      data = list()
    )

    for (x in seq_len(length(files_categorized))) {
#**** DIA-NN/ Spectronaut / Generic
     if (overview[i, "count", drop = TRUE] == 1) {

       if (overview[i, "filename", drop = TRUE] == files_categorized[[x]][["filename"]] & overview[i, "software", drop = TRUE] == files_categorized[[x]][["software"]]) {
         ordered_files[[i]][["data"]][[1]] <- files_categorized[[x]][["data"]]
         names(ordered_files[[i]][["data"]])[1] <- files_categorized[[x]][["type"]]
         next
       }
#**** MaxQuant
     } else if (overview[i, "software", drop = TRUE] == "MaxQuant") {

       if (overview[i, "filename", drop = TRUE] == files_categorized[[x]][["filename"]] & overview[i, "software", drop = TRUE] == files_categorized[[x]][["software"]] & files_categorized[[x]][["type"]] == "ev") {

         ordered_files[[i]][["data"]][[1]] <- files_categorized[[x]][["data"]]
         names(ordered_files[[i]][["data"]])[1] <- files_categorized[[x]][["type"]]
         next

       } else if (overview[i, "filename", drop = TRUE] == files_categorized[[x]][["filename"]] & overview[i, "software", drop = TRUE] == files_categorized[[x]][["software"]] & files_categorized[[x]][["type"]] == "pep") {

         ordered_files[[i]][["data"]][[2]] <- files_categorized[[x]][["data"]]
         names(ordered_files[[i]][["data"]])[2] <- files_categorized[[x]][["type"]]
         next

       } else if (overview[i, "filename", drop = TRUE] == files_categorized[[x]][["filename"]] & overview[i, "software", drop = TRUE] == files_categorized[[x]][["software"]] & files_categorized[[x]][["type"]] == "pg") {

         ordered_files[[i]][["data"]][[3]] <- files_categorized[[x]][["data"]]
         names(ordered_files[[i]][["data"]])[3] <- files_categorized[[x]][["type"]]
         next

       }
#**** PD
     } else if (overview[i, "software", drop = TRUE] == "PD") {
         if (overview[i, "filename", drop = TRUE] == files_categorized[[x]][["filename"]] & overview[i, "software", drop = TRUE] == files_categorized[[x]][["software"]] & files_categorized[[x]][["type"]] == "psm") {

           ordered_files[[i]][["data"]][[1]] <- files_categorized[[x]][["data"]]
           names(ordered_files[[i]][["data"]])[1] <- files_categorized[[x]][["type"]]
           next

         } else if (overview[i, "filename", drop = TRUE] == files_categorized[[x]][["filename"]] & overview[i, "software", drop = TRUE] == files_categorized[[x]][["software"]] & files_categorized[[x]][["type"]] == "pep") {

           ordered_files[[i]][["data"]][[2]] <- files_categorized[[x]][["data"]]
           names(ordered_files[[i]][["data"]])[2] <- files_categorized[[x]][["type"]]
           next

         } else if (overview[i, "filename", drop = TRUE] == files_categorized[[x]][["filename"]] & overview[i, "software", drop = TRUE] == files_categorized[[x]][["software"]] & files_categorized[[x]][["type"]] == "prot") {

           ordered_files[[i]][["data"]][[3]] <- files_categorized[[x]][["data"]]
           names(ordered_files[[i]][["data"]])[3] <- files_categorized[[x]][["type"]]
           next

         } else if (overview[i, "filename", drop = TRUE] == files_categorized[[x]][["filename"]] & overview[i, "software", drop = TRUE] == files_categorized[[x]][["software"]] & files_categorized[[x]][["type"]] == "pg") {

           ordered_files[[i]][["data"]][[4]] <- files_categorized[[x]][["data"]]
           names(ordered_files[[i]][["data"]])[4] <- files_categorized[[x]][["type"]]
           next

         }
       }
    }
 }

  #prepare files - rename + filtering
  for (i in seq_len(length(ordered_files))) {

    if (ordered_files[[i]][["software"]] == "DIA-NN") {
      ordered_files[[i]][["data"]][["DIA-NN"]] <- prepare_input(ordered_files[[i]][["data"]][["DIA-NN"]], software = "DIA-NN",
                                                                diann_addon_pg_qval = diann_addon_pg_qval,
                                                                diann_addon_prec_qval = diann_addon_prec_qval)
      next
    } else if (ordered_files[[i]][["software"]] == "Spectronaut") {
      ordered_files[[i]][["data"]][["Spectronaut"]] <- prepare_input(ordered_files[[i]][["data"]][["Spectronaut"]], software = "Spectronaut")
      next
    } else if (ordered_files[[i]][["software"]] == "MaxQuant") {
      ordered_files[[i]][["data"]][["ev"]] <- prepare_input(ordered_files[[i]][["data"]][["ev"]], software = "MaxQuant", MaxQuant_addon = "evidence")
      ordered_files[[i]][["data"]][["pep"]] <- prepare_input(ordered_files[[i]][["data"]][["pep"]], software = "MaxQuant", MaxQuant_addon = "peptide")
      ordered_files[[i]][["data"]][["pg"]] <- prepare_input(ordered_files[[i]][["data"]][["pg"]], software = "MaxQuant", MaxQuant_addon = "proteingroup")
      next
    } else if (ordered_files[[i]][["software"]] == "PD") {
      ordered_files[[i]][["data"]][["psm"]] <- prepare_input(ordered_files[[i]][["data"]][["psm"]], software = "PD", PD_addon = "psm")
      ordered_files[[i]][["data"]][["pep"]] <- prepare_input(ordered_files[[i]][["data"]][["pep"]], software = "PD", PD_addon = "peptide")
      ordered_files[[i]][["data"]][["prot"]] <- prepare_input(ordered_files[[i]][["data"]][["prot"]], software = "PD", PD_addon = "protein")
      ordered_files[[i]][["data"]][["pg"]] <- prepare_input(ordered_files[[i]][["data"]][["pg"]], software = "PD", PD_addon = "proteingroup")
      next
    } else if (ordered_files[[i]][["software"]] == "Generic") {
      ordered_files[[i]][["data"]][["Generic"]] <- prepare_input(ordered_files[[i]][["data"]][["Generic"]], software = "Generic")
      next
    }
  }

  #add names in list
  ordered_files_names <- vector(mode = "character")
  for (i in seq_len(length(ordered_files))){
    ordered_files_names[i] <- ordered_files[[i]][["filename"]]
  }
  names(ordered_files) <- ordered_files_names

  #dependency - same name for different software theoretically possible ===
  #eg. SAME_Report.tsv and SAME_peptides.txt
  #results in WRONG ASSIGNMENT

  if(length(unique(ordered_files_names)) != length(ordered_files)) {
    stop("Please use a unique filename for each analysis.")
  }
  #===

  return(ordered_files)
}
