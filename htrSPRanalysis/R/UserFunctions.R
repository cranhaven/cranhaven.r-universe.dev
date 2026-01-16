#' Process user input files and obtain options for fitting.
#'
#' Performs all functions selected in sample information, such as
#' automated dissociation window detection, automated concentration range, automated bulk shift detection and
#' returns a list object with the titration time series, processed sample information, all user inputs directing
#' file outputs and fitting options
#'
#' @importFrom rlang .data

#'
#' @param sample_sheet_path The full path to the sample information file.
#' @param data_file_path The full path to the titration data file.
#' @param output_file_path The full path where output should be stored. This directory needs to exist.
#' @param output_pdf The name of the file for the pdf output.
#' @param output_csv The name of the file for the csv output.
#' @param error_pdf The name of the file for error output.
#' @param num_cores The number of cores to use for parallel processing. The default is( the number of cores detected by `parallel::detectCores()`.
#' @param min_allowed_kd The minimum value for the dissociation constant. The default is 10^(-5).
#' @param max_iterations The maximum number of iterations for curve fitting. The default is 1000.
#' @param ptol Curve fitting parameter. If the proposed changes in parameters is smaller than this value, the optimization is considered converged. The default is 10^(-10)
#' @param ftol Curve fitting parameter. If the squared error between observed and predicted values is smaller than ftol, the optimization is considered converged. The default is 10^(-10)
#' @param min_RU_tol Minimum RU required for dissociation window detection
#' @param max_RU_tol Maximum RU required for dissociation window detection. Also used in curve fitting.
#'
#' @return A list object containing the following
#' \item{expanded_sample_sheet}{The sample sheet expanded to include all spots that are represented, expanding the short-hand entries for Position/Block/Channel}
#' \item{sample_info}{The expanded sample sheet with only the rows that are to be fit}
#' \item{sample_info_fits}{The sample_info without rows that have encountered errors in initial processing}
#' \item{Time}{The dataframe whose columns are the Time values for the input titration data. This only includes columns selected for analysis.}
#' \item{RU}{The dataframe whose columns are the RU values for the input titration data. Only the columns for the samples to be analyzed are included}
#' \item{correctedRU}{The `RU` dataframe after baseline correction}
#' \item{keep_concentrations}{A vector containing the indices of the columns from `Time` and `correctedRU` to be used in curve fitting}
#' \item{all_concentrations_values}{A vector containing the concentration values corresponding to the columns of the `Time` and `RU` dataframes}
#' \item{incl_concentrations_values}{A vector containing the concentration values corresponding to the `Time` and `correctedRU` columns chosen for curve fitting}
#' \item{n_time_points}{The maximum length of titration time series}
#' \item{max_RU_tol}{The maximum RU for dissociation window trimming to be automated}
#' \item{min_RU_tol}{The minimum RU for dissociation window trimming to be automated}
#' \item{min_RU_tol}{The minimum RU for dissociation window trimming to be automated}
#' \item{nwells}{The number of rows in the `sample_info` dataframe}
#' \item{n_fit_wells}{The number of rows in the `sample_info_fits` dataframe}
#' \item{ftol}{The ftol parameter passed to the `nls.lm` function}
#' \item{ptol}{The ptol parameter passed to the `nls.lm` function}
#' \item{ptol}{The ptol parameter passed to the `nls.lm` function}
#' \item{output_pdf}{The full pathname for the output pdf file}
#' \item{output_csv}{The full pathname for the output csv file}
#' \item{error_pdf}{The full pathname for the pdf error file. This is where errors in processing can be found.}
#' \item{error_idx_concentrations}{If there is an issue in determining the concentration window for some spots, they will be logged here}

#' @examples
#' # set up file paths for example
#'
#'\donttest{ sample_sheet_path <- system.file("extdata",
#'  "sample_sheet.xlsx", package="htrSPRanalysis")
#'
#'fn <- paste0("https://gitlab.oit.duke.edu/janice/htrspranalysis/",
#'         "-/raw/master/inst/extdata/titration_data.xlsx?ref_type=heads")
#'
#' download.file(fn,
#'       destfile = paste0(tempdir(),"/titration_data.xlsx"),
#'       mode = "wb")
#'
#' data_file_path <- paste0(tempdir(), "/titration_data.xlsx")
#'
#' # process the input
#' processed_input <- process_input(sample_sheet_path = sample_sheet_path,
#'   data_file_path = data_file_path,
#'    num_cores = 2)}
#' @export process_input

process_input <- function(sample_sheet_path = NULL,
                          data_file_path = NULL,
                          output_file_path = NULL,
                          output_pdf = NULL,
                          output_csv = NULL,
                          error_pdf = NULL,
                          num_cores = NULL,
                          min_allowed_kd = 10^(-5),
                          max_iterations = 1000,
                          ptol = 10^(-10),
                          ftol = 10^(-10),
                          min_RU_tol = 20,
                          max_RU_tol = 300){

  sample_sheet <- readxl::read_excel(sample_sheet_path)
  #identify if any flags present in the sample sheet
  flagspresent <- check_sample_sheet(sample_sheet, sample_sheet_path)

  if (flagspresent){
     usr_msg <- "Error in sample sheet file. See Error_note_sample_sheet.csv for detailed description, then select updated file"
     stop(usr_msg)
  }

  # Keep track of the original ordering. Sample sheet will be re-ordered to be in order of ROI for processing

  sample_sheet$OriginalIdx <- 1:dim(sample_sheet)[1]
  #formatting sample_sheet data
  sample_info <- process_sample_sheet(sample_sheet)

  n_incl_samples <- sum(sample_info$Incl. == "Y")
  if (n_incl_samples == 0)
    stop("No samples chosen for analysis in sample sheet")

  #import filter

  # readxl is *horribly* slow right now. Switching to openxlsx for the moment, may switch if readxl gets fixed.
  # check if this is new or old file format
  titration_data <- openxlsx::read.xlsx(data_file_path, colNames = TRUE, sheet = 1, startRow = 1, check.names = TRUE)

  ligand_and_ROI <- NULL

  if (titration_data[1,1] == "X"){
    # file may be in new format. We should skip first line
    titration_data <- openxlsx::read.xlsx(data_file_path,
                                          colNames = TRUE,
                                          sheet = 1,
                                          startRow = 2,
                                          check.names = TRUE)
    ligand_and_ROI <- openxlsx::read.xlsx(data_file_path,
                                          colNames = FALSE,
                                          sheet = 1,
                                          rows = 1,
                                          startRow = 1,
                                          check.names = TRUE)
    ligand_and_ROI <- tibble::tibble(ligand_and_ROI)

    ligand_and_ROI %>% tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "ROI") %>%
      tidyr::separate("ROI", into = c("Name", "ROI"), sep = " \\(") %>%
      tidyr::separate("ROI", into = c("ROI", "Rest"), sep = "\\)") %>%
        dplyr::mutate(ROI = as.numeric(.data$ROI)) %>%
        tidyr::separate(col = "Name", into = c("bracket", "Name")) %>%
        tidyr::separate(col = "Rest", into = c("Dash1","Dash2", "Analyte", "Conc String", "Conc"), sep = " ") %>%
        tidyr::separate("Conc", into = c("Conc.", "Cycle"), sep = "\\(") %>%
        dplyr::select("Name", "ROI", "Analyte", "Conc.", "Cycle") %>%
        dplyr::mutate(Conc. = as.numeric(.data$Conc.)) %>%
        dplyr::mutate(Cycle = as.numeric(.data$Cycle)) -> ligand_and_ROI
  } else
      titration_data <- openxlsx::read.xlsx(data_file_path,
                                          colNames = TRUE,
                                          sheet = 1,
                                          startRow = 1,
                                          check.names = TRUE)

  titration_data <- tibble::tibble(titration_data)

  #identify if any flags present in the sample sheet
  flagspresent <- check_titration_data(titration_data, data_file_path)

  if (flagspresent){
    usr_msg <- "Error in titration data file. See Error_note_titration_data.csv for detailed description, then select corrected file"
    stop(usr_msg)
  }


  usr_msg <- check_sample_and_data_match(sample_info, ligand_and_ROI)

  if (!is.null(usr_msg))
    stop(usr_msg)

  ################################ Set fitting options - not allowing user to change ptol or ftol at the moment ###############################

  if (is.na(min_allowed_kd) | min_allowed_kd < 10^(-7) | min_allowed_kd > 10^(-3)){
      usr_msg <- "Invalid min_allowed_kd. Please use a number in the form 1e-n, where n is between 3 and 7"
      stop(usr_msg)
  }
  if (is.na(max_iterations) | max_iterations < 500 | max_iterations > 100000){
     usr_msg <- "Invalid max_iterations. Please use a number between 500 and 100000"
     stop(usr_msg)
  }

  if (is.na(min_RU_tol) | min_RU_tol < 0 | min_RU_tol > 300){
      usr_msg <- "Invalid min_RU_tol. Please use a number between 0 and 300"
      stop(usr_msg)
  }

  if (is.na(max_RU_tol) | max_RU_tol < 50 | max_RU_tol > 2000){
      usr_msg <- "Invalid max_RU_tol. Please use a number between 50 and 2000"
      stop(usr_msg)
  }

  detected_num_cores <- parallel::detectCores()

  if (detected_num_cores < 1)
    detected_num_cores <- 1

  if (is.null(num_cores))
     num_cores <- detected_num_cores

  if (is.na(num_cores) | num_cores > detected_num_cores | num_cores < 1){
      usr_msg <- paste("Invalid value for num_cores. Please use a number between 1 and", detected_num_cores)
      stop(usr_msg)
  }

  ########### Set output file names #######################################
  output_file_path_default <-
    stringr::str_split(sample_sheet_path, "-", n = 2)[[1]][1]

  if (is.null(output_file_path))
    output_file_path <- output_file_path_default

  date_time <- date()
  date_time <- gsub(x = date_time, ":", "_")

  output_pdf_default <- file.path(output_file_path, paste0(date_time, "_output.pdf"))
  if (is.null(output_pdf))
    output_pdf <- output_pdf_default
  else
    output_pdf <- file.path(output_file_path, output_pdf)

  output_csv_default <- file.path(output_file_path, paste0(date_time,"_output.csv"))
  if (is.null(output_csv))
    output_csv <- output_csv_default
  else
    output_csv <- file.path(output_file_path, output_csv)

  error_pdf_default <- file.path(output_file_path, paste0(date_time,"_error.pdf"))
  if (is.null(error_pdf))
    error_pdf <- error_pdf_default
  else
    error_pdf <- file.path(output_file_path, error_pdf)

  ####### Process sample sheet #############################################
  # There are different numbers of concentrations exported for each well.
  # Here, we delete the observations from the time series and from the ligand_conc data frame
  # that are chosen by the user as not to be included.
  # For some analyses, we will restrict to the chosen concentrations.

  # keep track of ROI
  #n_ROI <- dim(sample_info)[1]
  #sample_info$ROI <- 1:n_ROI

  #remove wells from ligand each time series.
  if (!is.null(ligand_and_ROI)){
    selected_samples <- select_samples_with_ROI(sample_info,
                                                titration_data,
                                                ligand_and_ROI)
    ligand_and_ROI <- selected_samples$ligand_and_ROI
  } else
    selected_samples <- select_samples(sample_info, titration_data)
  #selected_samples

  expanded_sample_sheet <- sample_info
  sample_info <- selected_samples$sample_info
#  keep_concentrations <- selected_samples$keep_concentrations
  Time <- selected_samples$Time
  RU <- selected_samples$RU
  all_concentrations_values <- selected_samples$all_concentrations_values
  n_time_points <- selected_samples$n_time_points

  nwells <- dim(sample_info)[1]
  # get index of first concentration (selected for analysis) for each well

  if (is.null(ligand_and_ROI))
     first_conc_idx_list <- purrr::map(.x = 1:nwells, .f = first_conc_indices,
                             sample_info$NumConc) else {
                               first_conc_idx_list <- purrr::map(.x = 1:nwells,
                                                                 .f = first_conc_indices_from_titration_data,
                                                                 sample_info$ROI,
                                                                 ligand_and_ROI)
                             }


  sample_info$FirstConcIdx <- purrr::as_vector(purrr::flatten(first_conc_idx_list))

  # get baseline averages for each well (first time point)
  sample_info$BaselineAverage <- rep(0, nwells)

  baseline_info_list <- purrr::map_dfr(.x = 1:nwells, .f = get_baseline_indices,
                                sample_info,
                                Time, RU)

  sample_info$BaselineAverage <- baseline_info_list$min_baseline
  sample_info$BaselineIdx <- baseline_info_list$baseline_idx
  sample_info$BaselineNegative <- baseline_info_list$baseline_negative
  rm(baseline_info_list)

  sample_info$WellIdx <- 1:nwells

  corrected_RU <- purrr::map_dfc(.x = 1:nwells, .f = baseline_correction,
                          Time,
                          RU,
                          sample_info)


  ############################  Concentrations for fits ##############################################

  selected_concentrations <- select_concentrations(sample_info, Time, corrected_RU)

  keep_concentrations <- selected_concentrations$keep_concentrations
  sample_info <- selected_concentrations$sample_info

  incl_concentrations_values <- selected_concentrations$incl_concentrations_values
  incl_concentrations_ligand <- selected_concentrations$incl_concentrations_ligand

  error_idx_concentrations <- selected_concentrations$error_idx

  # Delete NumConc corresponding to error_idx from all FirstConcIdx after the error_idx
  # Also need to delete corresponding rows in Time and RU data frames
  # and fix all_concentrations_values

  wells <- which(!(1:nwells %in% error_idx_concentrations))
  n_fit_wells <- length(wells)

  # now the list doesn't match with sample info
  # we'll create a new sample sheet, because we need the full sheet later to display error information
  # any errors now make the first concentration index off.

  sample_info_fits <- sample_info[wells, ]

  sample_info_fits$NumInclConc <- incl_concentrations_ligand

  first_incl_conc_idx_list <- purrr::map(.x = 1:n_fit_wells, .f = first_conc_indices,
                                         sample_info_fits$NumInclConc)

  sample_info_fits$FirstInclConcIdx <- purrr::as_vector(purrr::flatten(first_incl_conc_idx_list))

  bulkshift <- purrr::map(.x = 1:n_fit_wells, .f = get_auto_bulkshift,
                              sample_info_fits,
                              Time[, keep_concentrations],
                              corrected_RU[, keep_concentrations])

  sample_info_fits$Bulkshift <- as.vector(bulkshift)
  cl <- parallel::makeCluster(getOption("cl.cores", num_cores))
  parallel::clusterEvalQ(cl, library("htrSPRanalysis"))

  end_dissoc_list_biphasic <- parallel::parLapply(cl, X = 1:n_fit_wells,
                                   fun = purrr::safely(find_dissociation_window_biphasic),
                              sample_info_fits,
                              Time[, keep_concentrations],
                              corrected_RU[, keep_concentrations],
                              incl_concentrations_values,
                              max_RU_tol,
                              min_RU_tol)

  end_dissoc_list_flat <- parallel::parLapply(cl, X = 1:n_fit_wells,
                                         fun = purrr::safely(find_dissociation_window_flat),
                                         sample_info_fits,
                                         Time[, keep_concentrations],
                                         corrected_RU[, keep_concentrations],
                                         incl_concentrations_values,
                                         max_RU_tol,
                                         min_RU_tol)
  parallel::stopCluster(cl)

  sample_info_fits$DissocEndBiphasic <- purrr::map_dbl(.x = end_dissoc_list_biphasic,
                                        .f = function(x){
                                          ifelse(is.null(x$error) & !is.null(x$result),
                                                 x$result, NA)})

  sample_info_fits$DissocEndFlat <- purrr::map_dbl(.x = end_dissoc_list_flat,
                                                       .f = function(x){
                                                         ifelse(is.null(x$error) & !is.null(x$result),
                                                                x$result, NA)})
  sample_info_fits$DissocEnd <- pmin(sample_info_fits$DissocEndFlat,
                                     sample_info_fits$DissocEndBiphasic,
                                     na.rm = TRUE)

  list(expanded_sample_sheet = expanded_sample_sheet,
       sample_info = sample_info,
       sample_info_fits = sample_info_fits,
       Time = Time, RU = RU, corrected_RU = corrected_RU,
       keep_concentrations = keep_concentrations,
       all_concentrations_values = all_concentrations_values,
       incl_concentrations_values = incl_concentrations_values,
       n_time_points = n_time_points, max_RU_tol = max_RU_tol, min_RU_tol = min_RU_tol, nwells = nwells,
       n_fit_wells = n_fit_wells, num_cores = num_cores, min_allowed_kd = min_allowed_kd,
       max_iterations = max_iterations, ptol = ptol, ftol = ftol, output_pdf = output_pdf,
       output_csv = output_csv, error_pdf = error_pdf, error_idx_concentrations = error_idx_concentrations)
}

#' Plot all raw data that has been selected to be processed (via the `Incl.` column in the sample information). No
#' adjustments are made to the data.
#'
#' @param processed_input The list file that is output from the `process_input` function.
#' @return A list of all plots that have been selected via the `Incl.` column in sample information
#' @export get_plots_before_baseline



get_plots_before_baseline <- function(processed_input){
  sample_info <- processed_input$sample_info
  Time <- processed_input$Time
  RU <- processed_input$RU
  incl_concentrations_values <- processed_input$incl_concentrations_values
  all_concentrations_values <- processed_input$all_concentrations_values
  n_time_points <- processed_input$n_time_points
  num_cores <- processed_input$num_cores
  nwells <- processed_input$nwells

  cl <- parallel::makeCluster(getOption("cl.cores", num_cores))
  parallel::clusterEvalQ(cl, library("htrSPRanalysis"))

  plot_result <- parallel::parLapply(cl, X = 1:nwells, fun = plot_sensorgrams, sample_info,
           Time, RU,
           incl_concentrations_values,
           all_concentrations_values,
           n_time_points, all_concentrations = TRUE)
  parallel::stopCluster(cl)
  plot_result
}

#' Get fits of all selected sensorgrams as indicated in the sample information.
#' @param processed_input The processed_input object returned by the function `process_input`.
#' @return A list of all fits. The fits are performed using the `safely` function, so that the list has a `$result` entry
#' and a `$error` entry for each item. If `$error` is `NULL`, the sensorgram was fit succesfully.
#' @export get_fits

get_fits <- function(processed_input){

  n_fit_wells <- processed_input$n_fit_wells
  n_time_points <- processed_input$n_time_points
  sample_info_fits <- processed_input$sample_info_fits
  num_cores <- processed_input$num_cores
  Time <- processed_input$Time
  RU <- processed_input$corrected_RU
  keep_concentrations <- processed_input$keep_concentrations
  incl_concentrations_values <- processed_input$incl_concentrations_values
  min_allowed_kd <- processed_input$min_allowed_kd
  max_iterations <- processed_input$max_iterations
  max_RU_tol <- processed_input$max_RU_tol
  ptol <- processed_input$ptol
  ftol <- processed_input$ftol


  cl <- parallel::makeCluster(getOption("cl.cores", num_cores))
  parallel::clusterEvalQ(cl, library("htrSPRanalysis"))

  fit_result <- parallel::parLapply(cl, X = 1:n_fit_wells, fun = purrr::safely(fit_association_dissociation), sample_info_fits,
           Time[, keep_concentrations],
           RU[, keep_concentrations],
           incl_concentrations_values,
           n_time_points,
           min_allowed_kd,
           max_iterations,
           ptol,
           ftol,
           max_RU_tol)

  parallel::stopCluster(cl)
  fit_result


}


#' Plot fitted sensorgras and raw data.
#' @param processed_input processed_input as returned by `process_input`
#' @param fits_list List of fits as returned by `get_fits`
#' @return list of plots of sensorgrams and fits
#' @export get_fitted_plots

get_fitted_plots <- function(processed_input, fits_list){
  n_fit_wells <- processed_input$n_fit_wells
  n_time_points <- processed_input$n_time_points
  num_cores <- processed_input$num_cores
  sample_info_fits <- processed_input$sample_info_fits
  keep_concentrations <- processed_input$keep_concentrations
  incl_concentrations_values <- processed_input$incl_concentrations_values
  RU <- processed_input$corrected_RU
  Time <- processed_input$Time


  cl <- parallel::makeCluster(getOption("cl.cores", num_cores))
  parallel::clusterEvalQ(cl, library("htrSPRanalysis"))

  plot_result <- parallel::parLapply(cl, X = 1:n_fit_wells, fun = plot_sensorgrams_with_fits,
           sample_info_fits, fits_list,
           Time[, keep_concentrations], RU[, keep_concentrations],
           incl_concentrations_values,
           n_time_points)

  parallel::stopCluster(cl)
  plot_result
}

#' Plot response curve. Average RU versus log10 of concentration. Color coded for concentrations selected for fitting.
#' @param processed_input Processed input object as returned from `process_input` function.
#' @return list of plots of response curves, indicating the concentrations chosen for fitting
#' @export get_rc_plots

get_rc_plots <- function(processed_input){

  n_fit_wells <- processed_input$n_fit_wells
  n_time_points <- processed_input$n_time_points
  num_cores <- processed_input$num_cores
  sample_info_fits <- processed_input$sample_info_fits
  sample_info <- processed_input$sample_info
  incl_concentrations_values <- processed_input$incl_concentrations_values
  all_concentrations_values <- processed_input$all_concentrations_values
  RU <- processed_input$corrected_RU
  Time <- processed_input$Time

# Need to fix first concentration index in sample_info_fits. If we've skipped any because of errors,
# the first index is off.
  cl <- parallel::makeCluster(getOption("cl.cores", num_cores))
  parallel::clusterEvalQ(cl, library("htrSPRanalysis"))

  rc_result <- parallel::parLapply(cl, X = 1:n_fit_wells, fun = get_response_curve, sample_info_fits,
           Time, RU,
           all_concentrations_values,
           incl_concentrations_values, n_time_points)
  parallel::stopCluster(cl)
  rc_result

}

#' Create pdf file with sensorgrams with fitted curves, residuals, table of fit parameters, and response curves.
#'
#' @param processed_input Processed_input as returned by `process_input`
#' @param fits_list List of fits as returned by `get_fits`
#' @param rc_list List of response curves as returned by `get_rc_plots`
#' @param plot_list List of plots as returned by `get_fitted_plots`
#' @param ... Arguments passed to the `pdf` function.
#' @return `NULL` A pdf file is created using the path name supplied to `process_input`


#' @export create_pdf
create_pdf <- function(processed_input, fits_list, rc_list, plot_list, ...){
  nwells <- processed_input$nwells
  n_fit_wells <- processed_input$n_fit_wells
  sample_info <- processed_input$sample_info
  sample_info_fits <- processed_input$sample_info_fits
  output_pdf <- processed_input$output_pdf
  error_pdf <- processed_input$error_pdf
  error_idx_concentrations <- processed_input$error_idx_concentrations

  num_cores <- processed_input$num_cores
  #nwells <- processed_input$nwells

  # cl <- parallel::makeCluster(getOption("cl.cores", num_cores))
  # parallel::clusterEvalQ(cl, library("htrSPRanalysis"))
  #
  # pages_list <- parallel::parLapply(cl, X = 1:n_fit_wells, fun = purrr::safely(combine_output),
  #                                    fits_list,
  #                                    plot_list,
  #                                    sample_info_fits,
  #                                    rc_list,
  #                                    sample_info)
  # parallel::stopCluster(cl)
  #

  # sort in order original to sample sheet

  sample_info_fits %>%
    dplyr::select("OriginalIdx", "WellIdx") %>%
    dplyr::arrange(.data$OriginalIdx) %>%
    dplyr::select("WellIdx") %>%
    purrr::as_vector() -> sort_idx


  pages_list <-lapply(1:n_fit_wells, purrr::safely(combine_output),
                      fits_list, plot_list, rc_list, sample_info_fits)

  grDevices::pdf(file = output_pdf, ...)
  for (well_idx in sort_idx){
    if (!is.null(pages_list[[well_idx]]$result)){
      gridExtra::grid.arrange(pages_list[[well_idx]]$result)

    } else {
      error_msg <- paste("An error occurred when producing final output", well_idx,
                         "\n\n The sample info is: \n")
      sample_info_fits[well_idx,] %>%
        dplyr::select("Block",
                      "Row",
                      "Column",
                      "Ligand",
                      "Analyte") -> sample_info_error
      gridExtra::grid.arrange(grid::textGrob(error_msg), gridExtra::tableGrob(sample_info_error))
    }
  }
  grDevices::dev.off()

  if (!is.null(error_idx_concentrations)){
    grDevices::pdf(file = error_pdf)
    for (well_idx in 1:nwells){
      if (well_idx %in% error_idx_concentrations){
        error_msg <- paste("Too few concentrations selected/found for well", well_idx,
                           "\n\n This could be addressed by explicitly choosing concentrations to analyze. \n The sample info is: \n")
        sample_info[well_idx,] %>%
          dplyr::select("Block",
                        "Row",
                        "Column",
                        "Ligand",
                        "Analyte") -> sample_info_error
        gridExtra::grid.arrange(grid::textGrob(error_msg), gridExtra::tableGrob(sample_info_error))
      }
    }
    grDevices::dev.off()

  }

}

#' Create csv file with all fit parameters.
#'
#' @param processed_input Processed_input as returned by `process_input`
#' @param fits_list List of fits as returned by `get_fits`
#' @return a data frame with the fit parameters and errors.  A csv file is also created using the path name supplied to `process_input`


#' @export create_csv
create_csv <- function(processed_input, fits_list){

  sample_info_fits <- processed_input$sample_info_fits
  n_fit_wells <- processed_input$n_fit_wells
  output_csv <- processed_input$output_csv

  csv_data <- purrr::map_dfr(.x = 1:n_fit_wells, .f = get_csv, fits_list, sample_info_fits)

  csv_data$Ligand <- sample_info_fits$Ligand
  csv_data$Analyte <- sample_info_fits$Analyte
  csv_data$Block <- sample_info_fits$Block
  csv_data$Row <- sample_info_fits$Row
  csv_data$Column <- sample_info_fits$Column

  csv_data %>% dplyr::relocate(.data$Ligand,
                               .data$Analyte,
                               .data$Block,
                               .data$Row,
                               .data$Column) -> csv_data

  readr::write_csv(csv_data, file = output_csv)

  csv_data
}
