#' @title
#' Launch the program
#'
#' @description
#' This function takes a path string identifying the input file path. The
#' function internally runs a series of tests that assess the input dataset. If
#' any of these tests fails, the user gets information allowing them to correct
#' the input dataset and rerun the process. Once all tests pass, the dataset
#' is given as an output with a diagnosis for each observation (using the
#' function [add_diagnoses()] internally). The output dataset, along with its
#' associated labels (`label:en` by default) are provided to the user in an Excel
#' format file accessible in the output_folder specified. The output dataset
#' comes with a report that summarizes information about variable distributions
#' and descriptive statistics.
#'
#' @param input_file A character string identifying the path of the input file
#' (must be a CSV or a one sheet Excel file)
#' to be processed.
#' @param output_folder A character string identifying the folder path where
#' the output files will be saved.
#' @param language Optional argument allowing the user to get the diagnoses in a
#' specific language. Options are "label:en" (default), "label:fr", "label:de",
#' "label:sp", "label:nl", "label:jp", "label:in".
#' @inheritParams get_banff_version
#' @param option_filter Optional argument allowing the user to filter the
#' dataset using [dplyr::filter()] syntax. The variable used to filter must
#' exist.
#' @param detail Optional argument indicating whether the output should
#' include temporary variables generated in the process or not. FALSE by default.
#'
#' @return
#' Nothing to be returned. The function generates a folder (if not already
#' existing) where Excel files are stored.
#'
#' @examples
#' {
#'
#' version <- get_banff_version()
#' input_file <- system.file("extdata",
#'    paste0(version,"/banff_template.xlsx"), package = "banffIT")
#' banff_launcher(input_file, output_folder = tempdir())
#'
#' }
#'
#' @import dplyr tidyr stringr fs fabR madshapR
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom lubridate is.POSIXt
#' @importFrom tools file_ext
#' @importFrom crayon bold
#' @export
banff_launcher <- function(
    input_file,
    output_folder,
    language = "label:en",
    version = NULL,
    option_filter,
    detail = FALSE){

  # Function to read csv or xlsx file
  read_file <- function(file_path = input_file) {
    # Check the file extension
    file_extension <- file_ext(file_path)

    # Read the file based on its extension
    if (file_extension == "csv") {
      data <- read_csv_any_formats(file_path)
    } else if (file_extension == "xlsx") {
      data <- read_excel_allsheets(file_path, keep_as_list = FALSE)
    } else {
      stop("Unsupported file format. Only CSV and XLSX formats are supported.")
    }

    return(data)
  }

  ##### 1 - CHECK ARGUMENTS ####

  # get the version
  version <- get_banff_version(version)

  # list of arguments provided by user in the function.
  # fargs <- list()
  fargs <- as.list(match.call(expand.dots = TRUE))

  # check on arguments : detail
  if(!is.logical(detail))
    stop(call. = FALSE,'`detail` must be TRUE or FALSE.')

  # message to user to begin with the process.
  message_on_prompt(paste0(
"Welcome to the Banff diagnosis helper.

Press [enter] to continue or [esc] to quit.

"))

  ##### 2 - GENERATE FOLDER ####

  # check the folder (to be created if does not exists).
  if(path_ext(output_folder) != "")
    stop(call. = FALSE,"Please specify a valid directory name (in quote).")
  if(!dir.exists(output_folder)) dir_create(output_folder)

  # names of objects to be generated in the folder. construct name and check previous existence.
  input_file_name  <- make.names(path_ext_remove(basename(input_file)))

  timestamp <-
    str_replace_all(str_replace_all(
      toString(trunc(Sys.time(), units = "secs")),":","")," ","-")

  banff_diagnoses_path  <- paste0(output_folder,'/',input_file_name,'-', timestamp,'-diagnoses.xlsx')
  banff_assessment_path <- paste0(output_folder,'/',input_file_name,'-', timestamp,'-assessment.xlsx')
  banff_report_path     <- paste0(output_folder,'/',input_file_name,'-', timestamp,'-report.xlsx')
  banff_dictionary_path <- paste0(output_folder,'/banff_dictionary.xlsx')

  if(file.exists(banff_diagnoses_path))
    stop(call. = FALSE,"

Diagnoses for this file already exists in '",basename(output_folder),"'")

  ##### 3 - CHECK INPUT DATASET ####

  message("\n[1/6] - Import data")

  # creation of the dataset
  banff_dataset <- read_file(input_file)

  # creation of the Banff dictionary
  banff_dict        <- suppressMessages(get_banff_dictionary(version,NULL    , language, detail))
  banff_dict_input  <- suppressMessages(get_banff_dictionary(version,"input" , language, detail))
  banff_dict_output <- suppressMessages(get_banff_dictionary(version,"output", language, detail))

  ## creation of adequacy calculated
  adequacy_input_copy <- calculate_adequacy(banff_dataset)
  banff_dataset <-
    banff_dataset %>%
    mutate(
      adequacy = adequacy_input_copy$`adequacy_calculated`)

  # filter the dataset if the user specified filter
  if(!is.null(fargs$`option_filter`)){

    query_expr <- substitute(option_filter)
    banff_dataset <-
      banff_dataset %>%
      dplyr::filter(!! query_expr)

    }

  message("\n[2/6] - Evaluation of '",bold(input_file_name),"'")

  ##### evaluation of input dataset.
  banff_assessment <-
    suppressMessages(banff_dataset_evaluate(banff_dataset,version))

  ##### IF EMPTY #####
  if(!is.null(attributes(banff_assessment)$empty)){
    message(attributes(banff_assessment)$empty)
    return(invisible(NULL))
  }

  ##### IF ERROR DETECTED #####

  if(!is.null(attributes(banff_assessment)$error)){

    message(attributes(banff_assessment)$error)

message('\n',rep('-',80))

    message("
An assessment report has been generated in your output folder to help you correcting
your dataset. You can open it as an Excel file and check the variables that contain
errors, and correct them before reprocessing.

The data dictionary that is used in the process has been downloaded in your
output folder. You can open it as an Excel file and check the names provided.
Each of the variable listed is mandatory.

For further information please refer to documentation.")

    message("Export Banff assessment Excel files in \n'",bold(banff_assessment_path),"'\n")
    message("Export Banff dictionary Excel files in \n'",bold(banff_dictionary_path),"'\n")

    write_excel_allsheets(banff_assessment, banff_assessment_path)
    write_excel_allsheets(banff_dict, banff_dictionary_path)

    return(invisible(NULL))

  }

  message("\n[3/6] - Add diagnosis to each observation of '",
          bold(input_file_name),"'")

  banff_diagnoses <- suppressMessages(add_diagnoses(banff_dataset,version))

  message("\n[4/6] - Generate labels for each variable of '",
          bold(input_file_name),"'")

  banff_diagnoses_codeset <-
    suppressWarnings({
      data_dict_match_dataset(
        banff_diagnoses,
        as_data_dict_mlstr(banff_dict_output),
        output = 'dataset',data_dict_apply = TRUE)})

  banff_diagnoses_dataset <-
    dataset_zap_data_dict(banff_diagnoses_codeset) %>%
    dataset_cat_as_labels(data_dict = banff_dict_output)

  banff_diagnoses <- list(
    dataset = banff_diagnoses_dataset,
    codeset = banff_diagnoses_codeset)

  message("\n[5/6] - Assessment of the the annotated '",
          bold(input_file_name),"'\n")

  banff_report <- list()

  banff_report <-
    dataset_summarize(
      dataset = banff_diagnoses$codeset,
      data_dict = banff_dict,
      dataset_name = input_file_name)

  banff_report$`Dataset assessment - input`     <- banff_assessment$`Dataset assessment`
  banff_report$`Dataset assessment - diagnoses` <- banff_report$`Dataset assessment`
  banff_report$`Dataset assessment`             <- NULL
  banff_report$`Variables summary (all)`        <- banff_assessment$`Data dictionary summary`

  banff_report$`Dataset assessment - diagnoses` <-
    banff_report$`Dataset assessment - diagnoses` %>%
    select("variable" = "Variable name","condition" = "Dataset assessment", "value" = "Value") %>%
    dplyr::filter(!.data$`variable` %in% banff_report$`Dataset assessment - input`$variable)

  banff_report <- banff_report[unique(c(
    "Overview","Dataset assessment - input",
    "Dataset assessment - diagnoses", names(banff_report)))]

  message("\n[6/6] - Export XLSX files in '",bold(basename(output_folder)),"'")
  message("\n")

  message("

The assessment and addition of diagnoses on your dataset is now finished.
In the output folder, you will find an Excel file containing both labels and codes
of your dataset and for each participant, additional variables corresponding to the
diagnoses.

An assessment and summary report has been generated in your output folder to help you
assess and interpret your dataset. You can open it as an Excel file.

For further information please refer to documentation.")

  message("Export Banff diagnoses Excel files in \n'",bold(banff_diagnoses_path),"'\n")
  message("Export Banff report Excel files in \n'",bold(banff_report_path),"'\n")
  message("Export Banff dictionary Excel files in \n'",bold(banff_dictionary_path),"'\n")

  write_excel_allsheets(banff_diagnoses , banff_diagnoses_path)
  write_excel_allsheets(banff_report, banff_report_path)
  write_excel_allsheets(banff_dict, banff_dictionary_path)

  return(invisible(NULL))
}
