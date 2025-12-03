#' Run Quality Control Checks
#' @description This function takes a `gimap_dataset` and creates a QC report
#' @param gimap_dataset A special dataset structure that is setup using the
#' `setup_data()` function.
#' @param plots_dir directory to save plots created with this function,
#' if it doesn't exist already it will be created
#' @param overwrite default is FALSE; whether to overwrite the QC Report file
#' @param output_file Needs to be a string that ends with ".Rmd" What the
#' name of the output QC report file should be.
#' @param filter_zerocount_target_col default is NULL; Which sample column(s)
#' should be
#'  used to check for counts of 0?  If NULL and not specified, downstream
#'  analysis will select all sample columns
#' @param filter_plasmid_target_col default is NULL; Which sample columns(s)
#' should be used to look at log2
#' CPM expression for plasmid pgRNA constructs? If NULL and not specified,
#' downstream analysis
#' will select the first sample column only
#' @param filter_replicates_target_col default is NULL; Which sample columns
#' are replicates whose variation you'd like to analyze; If NULL, the last
#' 3 sample columns are used
#' @param open_results default is TRUE but if you don't want the report
#' automatically opened, choose FALSE.
#' @param ... additional parameters are sent to `rmarkdown::render()`
#' @returns a QC report saved locally
#' @export
#' @importFrom utils browseURL
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @examples \dontrun{
#'
#' gimap_dataset <- get_example_data("gimap")
#'
#' run_qc(
#'   gimap_dataset,
#'   plots_dir = tempdir(),
#'   output_file = paste0(tempfile(), "_QC_Report.Rmd")
#' )
#' }
run_qc <- function(gimap_dataset,
                   output_file,
                   plots_dir,
                   overwrite = FALSE,
                   filter_zerocount_target_col = NULL,
                   filter_plasmid_target_col = NULL,
                   filter_replicates_target_col = NULL,
                   open_results = TRUE,
                   ...) {
  if (!("gimap_dataset" %in% class(gimap_dataset))) {
    stop("This function only works with gimap_dataset objects
       which can be made with the setup_data() function.")
  }

  # Determine the template
  templateFile <- system.file("extdata/gimapQCTemplate.Rmd", package = "gimap")

  # Make sure that the directory exists!
  directory <- dirname(output_file)

  # If not, make the directory
  if (directory != ".") {
    if (!dir.exists(directory)) {
      dir.create(directory, showWarnings = TRUE, recursive = TRUE)
    }
  }

  # Now if the file exists,
  if (file.exists(templateFile)) {
    if (file.exists(output_file) & !overwrite) {
      stop("there is already an output .Rmd file", output_file,
        ". Please remove or rename this file,",
        "or choose another output_file name.",
        call. = FALSE
      )
    } else {
      file.copy(from = templateFile, to = output_file, overwrite = overwrite)
    }
  } else {
    stop("The Rmd template file ", templateFile,
      " does not exist -- did you move it from the package files?",
      call. = FALSE
    )
  }

  # Make a plots directory if it doesn't exist
  if (!dir.exists(plots_dir)) dir.create(plots_dir, showWarnings = TRUE)

  # Send the data to render it!
  rmarkdown::render(output_file,
    params = list(
      dataset = gimap_dataset,
      plots_dir = plots_dir,
      filter_zerocount_target_col = filter_zerocount_target_col,
      filter_plasmid_target_col = filter_plasmid_target_col,
      filter_replicates_target_col = filter_replicates_target_col
    ),
    ...
  )

  # Tell where the output is
  results_file <- gsub("\\.Rmd$", "\\.html", output_file)
  message("Results in: ", results_file)

  if (interactive()) {
    if (open_results) {
      if (results_file != "") browseURL(results_file)
    }
  }
  results_file
}
