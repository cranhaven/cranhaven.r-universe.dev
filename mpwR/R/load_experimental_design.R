#' Load and Prepare the input data with experimental design file
#'
#' Based on submitted experimental design file the input data will be imported, renamed and default filtering will be applied. An experimental design template is available with write_experimental_design.
#'
#' Function for easily importing the default software outputs and preparing for downstream analysis with mpwR from multiple analysis folders. As default for MaxQuant "Reverse", "Potential contaminants" and "Only identified by site" are filtered out. As default for PD only "High" confidence identifications are included and for Found in Sample column(s) also only "High" identifications. Contaminants are filtered out. As default for Spectronaut only EG.Identified equals TRUE are included.
#'
#' @param path Path to folder with experimental design file. Within exp_design.csv the user needs to specify the analysis name, software and path to analysis folder. Also specific default suffixes are required: for MaxQuant: _evidence, _peptides, _proteinGroups; for PD - R-friendly headers enabled: _PSMs, _Proteins, _PeptideGroups, _ProteinGroups; for DIA-NN, Spectronaut and Generic: _Report
#' @author Oliver Kardell
#'
#' @importFrom data.table fread
#' @importFrom magrittr %>%
#' @import stringr
#'
#' @return A list - each list entry has filename and software info as well as stored data.
#' @export
#'
#' @examples
#' \dontrun{
#' #get template with write_experimental_design and adjust inputs
#' write_experimental_design("DIRECTORY_TO_FILE")
#'
#' #load in data
#' files <- load_experimental_design(path = "DIRECTORY_TO_FILE/your_exp_design.csv")
#' }

load_experimental_design <- function(path) {
  #handle global vars
  . <- NULL

  #Load experiment design file
  if (file.exists(path)) {
    exp_design <- import_mpwR(path)
  } else {
    stop("Experimental_design not found under specified path.")
  }

  #check dependencies
  if(ncol(exp_design) != 3 & sum(colnames(exp_design) != c("analysis_name", "software", "path_to_folder")) != 3) {
    stop("Wrong experimental file submitted? You require an analysis_name, software and path_to_folder column. You can use the write_experimental_design() to generate a template.")
  }

  if(nrow(exp_design) == 0) {
    stop("No entries detected in submitted file.")
  }

  #check for allowed software
  mq_soft_check <- sum(str_detect(exp_design$software, "MaxQuant"))
  diann_soft_check <- sum(str_detect(exp_design$software, "DIA-NN"))
  spec_soft_check <- sum(str_detect(exp_design$software, "Spectronaut"))
  pd_soft_check <- sum(str_detect(exp_design$software, "PD"))
  generic_soft_check <- sum(str_detect(exp_design$software, "Generic"))

  if (mq_soft_check + diann_soft_check + spec_soft_check + pd_soft_check + generic_soft_check != length(exp_design$software)) {
    stop("Unknown software detected or misspelled? - Please use MaxQuant, DIA-NN, Spectronaut, PD or Generic as input for software.")
  }

  #Check if all submitted folder paths exist
  for (i in seq_len(nrow(exp_design))) {
    if (!file.exists(exp_design$path_to_folder[i])) {
      stop(paste("Specified path:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - is not found. Please submit valid path."))
    }
  }

  #Check if files exist in folder per specific software
  for (i in seq_len(nrow(exp_design))) {
    if (stringr::str_detect(exp_design$software[i],"DIA-NN|Spectronaut") == TRUE) {
         output_files <- list.files(exp_design$path_to_folder[i], pattern = ".*Report.tsv$")
         if (length(output_files) == 0) {
           stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - no ", exp_design$software[i], " Report.tsv output is found. Please check."))
         }
    } else if (exp_design$software[i] == "Generic") {
      output_files <- list.files(exp_design$path_to_folder[i], pattern = ".*Report.csv$")
      if (length(output_files) == 0) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - no ", exp_design$software[i], " Report.csv output is found. Please check."))
      }

    } else if (exp_design$software[i] == "MaxQuant") {
      output_files <- list.files(exp_design$path_to_folder[i], pattern = "evidence.txt$")
      if (length(output_files) == 0) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - no ", exp_design$software[i], " evidence.txt output is found. Please check."))
      }

      output_files <- list.files(exp_design$path_to_folder[i], pattern = "peptides.txt$")
      if (length(output_files) == 0) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - no ", exp_design$software[i], " peptides.txt output is found. Please check."))
      }

      output_files <- list.files(exp_design$path_to_folder[i], pattern = "proteinGroups.txt$")
      if (length(output_files) == 0) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - no ", exp_design$software[i], " proteinGroups.txt output is found. Please check."))
      }

    } else if (exp_design$software[i] == "PD") {
      output_files <- list.files(exp_design$path_to_folder[i], pattern = "PSMs.txt$")
      if (length(output_files) == 0) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - no ", exp_design$software[i], " PSMs.txt output is found. Please check."))
      }

      output_files <- list.files(exp_design$path_to_folder[i], pattern = "PeptideGroups.txt$")
      if (length(output_files) == 0) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - no ", exp_design$software[i], " PeptideGroups.txt output is found. Please check."))
      }

      output_files <- list.files(exp_design$path_to_folder[i], pattern = "Proteins.txt$")
      if (length(output_files) == 0) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - no ", exp_design$software[i], " Proteins.txt output is found. Please check."))
      }

      output_files <- list.files(exp_design$path_to_folder[i], pattern = "ProteinGroups.txt$")
      if (length(output_files) == 0) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - no ", exp_design$software[i], " ProteinGroups.txt output is found. Please check."))
      }
    }
  }

  #check if multiple files are present
  for (i in seq_len(nrow(exp_design))) {
    if (stringr::str_detect(exp_design$software[i],"DIA-NN|Spectronaut") == TRUE)  {
      output_files <- list.files(exp_design$path_to_folder[i], pattern = ".*Report.tsv")
      if (length(output_files) > 1) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - for software ", exp_design$software[i], " more than one Report.tsv output is found. Please check."))
      }
    } else if (exp_design$software[i] == "Generic") {
      output_files <- list.files(exp_design$path_to_folder[i], pattern = ".*Report.csv$")
      if (length(output_files) > 1) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - for software ", exp_design$software[i], " more than one Report.csv output is found. Please check."))
      }

    } else if (exp_design$software[i] == "MaxQuant") {
      output_files <- list.files(exp_design$path_to_folder[i], pattern = "evidence.txt$")
      if (length(output_files) > 1) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - for software ", exp_design$software[i], " more than one evidence.txt output is found. Please check."))
      }

      output_files <- list.files(exp_design$path_to_folder[i], pattern = "peptides.txt$")
      if (length(output_files) > 1) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - for software ", exp_design$software[i], " more than one peptides.txt output is found. Please check."))
      }

      output_files <- list.files(exp_design$path_to_folder[i], pattern = "proteinGroups.txt$")
      if (length(output_files) > 1) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - for software ", exp_design$software[i], " more than one proteinGroups.txt output is found. Please check."))
      }

    } else if (exp_design$software[i] == "PD") {
      output_files <- list.files(exp_design$path_to_folder[i], pattern = "PSMs.txt$")
      if (length(output_files) > 1) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - for software ", exp_design$software[i], " more than one PSMs.txt output is found. Please check."))
      }

      output_files <- list.files(exp_design$path_to_folder[i], pattern = "PeptideGroups.txt$")
      if (length(output_files) > 1) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - for software ", exp_design$software[i], " more than one PeptideGroups.txt output is found. Please check."))
      }

      output_files <- list.files(exp_design$path_to_folder[i], pattern = "Proteins.txt$")
      if (length(output_files) > 1) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - for software ", exp_design$software[i], " more than one Proteins.txt output is found. Please check."))
      }

      output_files <- list.files(exp_design$path_to_folder[i], pattern = "ProteinGroups.txt$")
      if (length(output_files) > 1) {
        stop(paste0("In folder:",exp_design$path_to_folder[i] ," for analysis - ", exp_design$analysis_name[i], " - for software ", exp_design$software[i], " more than one ProteinGroups.txt output is found. Please check."))
      }
    }
  }

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

  #software dependend number of files
  file_names <- vector(mode = "character")
  ordered_files <- list()

  #automatic import - DIA-NN, Spectronaut, Generic
  for (i in seq_len(nrow(exp_design))) {
    if (exp_design[i, "software", drop = TRUE] == "DIA-NN" | exp_design[i, "software", drop = TRUE] == "Spectronaut" | exp_design[i, "software", drop = TRUE] == "Generic") {
      #import
      report_path <- list.files(exp_design$path_to_folder[i], full.names = TRUE)[stringr::str_detect(
        string = list.files(exp_design$path_to_folder[i], full.names = TRUE),
        pattern = ".*Report.tsv$|.*Report.csv$")]
      file <- import_mpwR(report_path)

      #file_name
      file_name <- exp_design$analysis_name[i]
      file_names[i] <- exp_design$analysis_name[i]

      ordered_files[[i]] <- list(
        filename = exp_design$analysis_name[i],
        software = exp_design$software[i]
      )
      ordered_files[[i]][["data"]][[exp_design$software[i]]] <- file

    } else if (exp_design[i, "software", drop = TRUE] == "MaxQuant") {
      #import
      report_path <- list.files(exp_design$path_to_folder[i], full.names = TRUE)[stringr::str_detect(
        string = list.files(exp_design$path_to_folder[i], full.names = TRUE),
        pattern = "evidence.txt$")]
      ev <- import_mpwR(report_path)

      report_path <- list.files(exp_design$path_to_folder[i], full.names = TRUE)[stringr::str_detect(
        string = list.files(exp_design$path_to_folder[i], full.names = TRUE),
        pattern = "peptides.txt$")]
      pep <- import_mpwR(report_path)

      report_path <- list.files(exp_design$path_to_folder[i], full.names = TRUE)[stringr::str_detect(
        string = list.files(exp_design$path_to_folder[i], full.names = TRUE),
        pattern = "proteinGroups.txt$")]
      pg <- import_mpwR(report_path)

      #file_name
      file_name <- exp_design$analysis_name[i]
      file_names[i] <- exp_design$analysis_name[i]

      ordered_files[[i]] <- list(
        filename = exp_design$analysis_name[i],
        software = exp_design$software[i]
      )
      ordered_files[[i]][["data"]][[1]] <- ev
      ordered_files[[i]][["data"]][[2]] <- pep
      ordered_files[[i]][["data"]][[3]] <- pg
      names(ordered_files[[i]][["data"]]) <- c("ev", "pep", "pg")

    } else if (exp_design[i, "software", drop = TRUE] == "PD") {
      #import
      report_path <- list.files(exp_design$path_to_folder[i], full.names = TRUE)[stringr::str_detect(
        string = list.files(exp_design$path_to_folder[i], full.names = TRUE),
        pattern = "PSMs.txt$")]
      psm <- import_mpwR(report_path)

      report_path <- list.files(exp_design$path_to_folder[i], full.names = TRUE)[stringr::str_detect(
        string = list.files(exp_design$path_to_folder[i], full.names = TRUE),
        pattern = "PeptideGroups.txt$")]
      pep <- import_mpwR(report_path)

      report_path <- list.files(exp_design$path_to_folder[i], full.names = TRUE)[stringr::str_detect(
        string = list.files(exp_design$path_to_folder[i], full.names = TRUE),
        pattern = "Proteins.txt$")]
      prot <- import_mpwR(report_path)

      report_path <- list.files(exp_design$path_to_folder[i], full.names = TRUE)[stringr::str_detect(
        string = list.files(exp_design$path_to_folder[i], full.names = TRUE),
        pattern = "ProteinGroups.txt$")]
      pg <- import_mpwR(report_path)

      #file_name
      file_name <- exp_design$analysis_name[i]
      file_names[i] <- exp_design$analysis_name[i]

      ordered_files[[i]] <- list(
        filename = exp_design$analysis_name[i],
        software = exp_design$software[i]
      )
      ordered_files[[i]][["data"]][[1]] <- psm
      ordered_files[[i]][["data"]][[2]] <- pep
      ordered_files[[i]][["data"]][[3]] <- prot
      ordered_files[[i]][["data"]][[4]] <- pg
      names(ordered_files[[i]][["data"]]) <- c("psm", "pep", "prot", "pg")

    }
    names(ordered_files) <- file_names
  }

  #prepare files - rename + filtering
  for (i in seq_len(length(ordered_files))) {

    if (ordered_files[[i]][["software"]] == "DIA-NN") {
      ordered_files[[i]][["data"]][["DIA-NN"]] <- prepare_input(ordered_files[[i]][["data"]][["DIA-NN"]], software = "DIA-NN")
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
  return(ordered_files)
}

