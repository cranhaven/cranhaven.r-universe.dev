#' Import data into an mpactr object.
#'
#' @description
#' `import_data()` takes two file paths, one for the pre-processed feature
#' table and one for sample metadata. Both files should be .csv.
#'
#' @details
#' mpactr requires a peak table and meta data as input. Files are expected to be
#' comma separated files (*.csv*).
#'
#' 1. `peak_table`: a peak table where rows are expected to be compounds. mpactr
#' supports import of feature table files from multiple tools through the
#' `format` argument. Currently supported value for `format` are "Progenesis",
#' "Metaboscape", or "None".
#'
#' `format` = "Progenesis." allows users to provide a feature table exported by
#' Progenesis. To export a compatible peak table in Progenesis, navigate to the
#' *Review Compounds* tab then File -> Export Compound Measurements. Select
#' the following properties: Compound, m/z, Retention time (min), and Raw
#' abundance and click ok.
#'
#' `format` = "Metaboscape" allows users to provide a feature table exported by
#' Metaboscape with default settings. The import function will save the raw peak
#' table in the `mpactr_object` and store a formatted peak table for filtering.
#' Reformatting includes selecting "FEATURE_ID", "RT", "PEPMASS", and sample
#' columns. Sample columns are determined from the "Injection" column in
#' `meta_data` (see below). "PEPMASS" is converted to m/z using the "ADDUCT"
#' column and compound metadata columns are renamed for mpactr.
#'
#' `format` = "None" allows users to provide a feature table file in the
#' expected format. This can be useful if you have a file from another tool and
#' want to manually format it in R. The table rows are expected to be individual
#' features, while columns are compound metadata and samples. The feature table
#' must have the compound metadata columns "Compound", "mz", and "rt". Where
#' "Compound" is the compound id, and can be `numeric` or `character`. "mz" is
#' the compound m/z, and should be `numeric`. "rt" is the retention time, in
#' minutes, and should be `numeric`. The remaining columns should be samples,
#' and match the names in the "Injection" column of the `meta_data` file.

#' 2. `meta_data`: a table with sample information. Either a file path or
#' `data.frame` can be supplied. At minimum the following columns are expected:
#' "Injection", "Sample_Code", and "Biological_Group". "Injection" is the sample
#' name and is expected to match sample column names in the `peak_table`.
#' "Sample_Code" is the id for technical replicate groups. "Biological_Group"
#' is the id for biological replicate groups. Other sample metadata can be
#' added, and is encouraged for downstream analysis following filtering with
#' mpactr.
#'
#' @param peak_table The file path or valid `https` url to your feature table
#' file.
#' @param meta_data The file path to your meta_data file or `data.frame`.
#' @param format The expected exported type of your peak table, can be
#' one of "Progenesis", "Metaboscape", "None".
#'
#' @return an `mpactr_object`.
#' @export
#'
#' @examples
#' data <- import_data(
#'   example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' meta_data <- read.csv(example_path("metadata.csv"))
#' data <- import_data(example_path("coculture_peak_table.csv"),
#'   meta_data,
#'   format = "Progenesis"
#' )
#'
import_data <- function(peak_table, meta_data, format = "none") {
  if (!any(class(meta_data) %in% c("data.table", "data.frame"))) {
    meta_data <- data.table(readr::read_csv(meta_data, show_col_types = FALSE))
  }

  #*** check for Injection, Sample_Code, Biological_Group
  cols <- c("Injection", "Sample_Code", "Biological_Group")
  if (any(cols %in% colnames(meta_data) == FALSE)) {
    cli::cli_abort("{.cls {cols[which(!(cols %in% colnames(meta_data)))]}}
                    are not columns in the provided metadata. Please see
                     function documentation for more details.")
  }

  df <- format_by_type(
    peak_table_path = peak_table,
    type_of_peak_table = format,
    sample_names = meta_data$Injection
  )

  mpactr_object <- mpactr$new(
    peak_table = unique_compounds(df),
    meta_data = data.table(meta_data)
  )
  mpactr_object$setup()
  filter_object <- filter_pactr$new(mpactr_object)
  return(filter_object)
}

unique_compounds <- function(peak_table_list, show_message = TRUE) {
  peak_table <- peak_table_list$peak_table
  duplicates <- names(which(table(peak_table$Compound) > 1))
  if (any(is.na(peak_table$Compound))) {
    stop("Found NA values inside your compounds names,
    please remove them.")
  }
  if (length(duplicates) > 0 && show_message) {
    cli::cli_inform("Found duplicate compound values, will add a suffix to
    unique the value.")
    peak_table$Compound <- UniqueDuplicates(as.character(peak_table$Compound))
  }
  return(list(
    "peak_table" = peak_table,
    "raw_table" = peak_table
  ))
}
