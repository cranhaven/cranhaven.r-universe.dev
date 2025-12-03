utils::globalVariables(c(
  "pg_ids", "plot_theme()", "negative_control", "positive_control", "mean_observed_cs", "timepoints", "value", "timepoint_avg", "target_type",
  "unexpressed_ctrl_flag", "median", "lfc_adj", "median", "gRNA1_seq", "gRNA2_seq",
  "control_gRNA_seq", "crispr_score", "pgRNA_target", "mean_double_control_crispr",
  "pgRNA_target", "targeting_gRNA_seq", "mean_single_crispr", "double_crispr_score",
  "single_crispr_score_1", "single_crispr_score_2", "pgRNA_target_double", "mean_single_crispr_1",
  "mean_single_crispr_2", "mean_double_control_crispr_2",
  "expected_crispr", "term", "estimate", "mean_expected_crispr", "intercept", "slope",
  "p_val_ttest", "p_val_wil", "fde_vals_ttest", "fdr_vals_wil", "double_gi_score",
  "single_gi_score_1", "single_gi_score_2", "gene", "DepMap_ID",
  "gene1_symbol", "gene2_symbol", "expressed_flag", "norm_ctrl_flag", "bool_vals",
  "filter_name", "counts", "numzero", "name", "value", "lfc_plasmid_vs_late", "lfc_adj",
  "double_gi_score", "count_normalized", "construct",
  "filterFlag", "plasmid_log2_cpm", "log2_cpm", "gene_symbol", "gene_symbol_1", "gene_symbol_2",
  "mean_double_control_crispr_1", "expected_crispr_double", "expected_crispr_single_1",
  "expected_crispr_single_2", "fdr_vals_ttest", "read_table", "stripped_cell_line_name",
  "comparison", ".", "col_names", "lfc_adj1", "t.test", "wilcox.test", "p.adjust",
  "cor", "quantile", "var", "browseURL", "single_crispr", "mean_single_crispr_2",
  "mean_single_crispr_1", "expected_single_crispr", "double_crispr", "double_gi_score",
  "fdr", "lfc", "mean_expected_cs", "mean_gi_score", "mean_single_crispr",
  "expected_double_crispr", "p_val", "single_gi_score", "Rank", "broad_target_type",
  "logfdr", "pointColor", "both", "mean_score", "gi_score", "Day05_RepA"
))


#' Returns example data for package
#' @description This function loads and returns example data for the package.
#' Which dataset is returned must be specified. Data will be downloaded from Figshare
#' the first time it is used.
#' @param which_data options are "count" or "meta"; specifies which example dataset should be returned
#' @param data_dir Where should the data be saved if applicable?
#' @param refresh_data should the example data that's been downloaded be deleted
#' and redownloaded?
#' @export
#' @returns the respective example data either as a data frame or a specialized
#' gimap_dataset depending on what was requested.
#'
#' @examples \dontrun{
#'
#' counts_timepoint <- get_example_data("count")
#' counts_treatment <- get_example_data("count_treatment")
#' gimap_timepoint_dataset <- get_example_data("gimap")
#' gimap_treatment_dataset <- get_example_data("gimap_treatment")
#' metadata <- get_example_data("meta")
#' annotation <- get_example_data("annotation")
#' }
get_example_data <- function(which_data,
                             data_dir = system.file("extdata", package = "gimap"),
                             refresh_data = FALSE) {
  file_name <- switch(which_data,
    "count" = "PP_pgPEN_HeLa_counts.txt",
    "count_treatment" = "counts_pgPEN_PC9_example.tsv",
    "meta" = "pgRNA_ID_pgPEN_library_comp.csv",
    "gimap" = "gimap_dataset_timepoint.RDS",
    "gimap_treatment" = "gimap_dataset_treatment.RDS",
    "annotation" = "pgPEN_annotations.txt"
  )

  # If data is to be refreshed delete old data
  if (refresh_data) {
    delete_example_data()
  }

  if (!grepl("RDS$", file_name)) {
    file_path <- file.path(data_dir, file_name)

    # Save file path in the options
    file_path_list <- list(file_path)
    names(file_path_list) <- which_data
    options(file_path_list)

    if (!file.exists(file_path)) {
      get_figshare(
        file_name = file_name,
        item = "28264271",
        output_dir = data_dir
      )
    }
  } else {
    file_path <- file.path(data_dir, file_name)

    if (!file.exists(file_path)) {
      download.file(
        paste0("https://github.com/FredHutch/gimap/",
               "raw/refs/heads/main/inst/extdata/", file_name),
        destfile = file_path
        )
    }
  }
  dataset <- switch(which_data,
    "count" = readr::read_tsv(file_path,
      show_col_types = FALSE
    ),
    "count_treatment" = readr::read_tsv(file_path,
      show_col_types = FALSE
    ),
    "meta" = readr::read_csv(file_path,
      skip = 1,
      show_col_types = FALSE
    ),
    "gimap" = readr::read_rds(file_path),
    "gimap_treatment" = readr::read_rds(file_path),
    "annotation" = readr::read_tsv(file_path, show_col_types = FALSE)
  )
  return(dataset)
}


#' Get file path to an default credentials RDS
#' @export
#' @return Returns the file path to folder where the example data is stored
example_data_folder <- function() {
  file <- list.files(
    pattern = "example_data.md",
    recursive = TRUE,
    system.file("extdata", package = "gimap"),
    full.names = TRUE
  )
  dirname(file)
}

#' Set up example data set for timepoints
#' @export
# This function sets up the RDS file for timepoint data
save_example_data_timepoint <- function() {
  example_data <- get_example_data("count") %>%
    dplyr::select(!Day05_RepA)

  example_pg_metadata <- get_example_data("meta")

  example_counts <- example_data %>%
    dplyr::select(c("Day00_RepA", "Day22_RepA", "Day22_RepB", "Day22_RepC")) %>%
    as.matrix()

  example_pg_id <- example_data %>%
    dplyr::select("id")

  example_pg_metadata <- example_data %>%
    dplyr::select(c("id", "seq_1", "seq_2"))

  example_sample_metadata <- data.frame(
    col_names = c("Day00_RepA", "Day22_RepA", "Day22_RepB", "Day22_RepC"),
    day = as.numeric(c("0", "22", "22", "22")),
    rep = as.factor(c("RepA", "RepA", "RepB", "RepC"))
  )

  gimap_dataset <- setup_data(
    counts = example_counts,
    pg_ids = example_pg_id,
    sample_metadata = example_sample_metadata
  )

  example_folder <- list.files(
    pattern = "PP_pgPEN_HeLa_counts.txt",
    recursive = TRUE,
    system.file("extdata", package = "gimap"),
    full.names = TRUE
  )

  saveRDS(gimap_dataset, file.path(dirname(example_folder), "gimap_dataset.RDS"))
}

#' Set up example data set for treatments
#' @export
# This function sets up the RDS file for treatment data
save_example_data_treatment <- function() {
  example_data <- get_example_data("count_treatment")

  example_pg_metadata <- get_example_data("meta")

  example_counts <- example_data %>%
    select(c("pretreatment", "dmsoA", "dmsoB", "drug1A", "drug1B")) %>%
    as.matrix()

  example_pg_id <- example_data %>%
    dplyr::select("id")

  example_pg_metadata <- example_data %>%
    dplyr::select(c("id", "seq_1", "seq_2"))

  example_sample_metadata <- data.frame(
    col_names = c("pretreatment", "dmsoA", "dmsoB", "drug1A", "drug1B"),
    drug_treatment = as.factor(c("pretreatment", "dmso", "dmso", "drug", "drug"))
  )

  gimap_dataset <- setup_data(
    counts = example_counts,
    pg_ids = example_pg_id,
    sample_metadata = example_sample_metadata
  )

  example_folder <- list.files(
    pattern = "PP_pgPEN_HeLa_counts.txt",
    recursive = TRUE,
    system.file("extdata", package = "gimap"),
    full.names = TRUE
  )

  saveRDS(gimap_dataset, file.path(dirname(example_folder), "gimap_dataset_treatment.RDS"))
}

plot_options <- function() {
  list(theme_bw(base_size = 12))
}

#' Default creds path
#' @param app_name What app set up are you looking for? Supported apps are 'google' 'calendly' and 'github'
encrypt_creds_path <- function(app_name) {
  list.files(
    pattern = paste0("figshare_encrypt.rds"),
    recursive = TRUE,
    system.file("extdata", package = "gimap"),
    full.names = TRUE
  )
}
#' Get file path to an key encryption RDS
key_encrypt_creds_path <- function() {
  list.files(
    pattern = "encrypt_pass.rds",
    recursive = TRUE,
    system.file("extdata", package = "gimap"),
    full.names = TRUE
  )
}
#' Handler function for GET requests from Figshare
#' @param item What is the item we are retrieving?
#' @param file_name Which item are we downloading?
#' @param output_dir Where should the file be saved?
#' @param return_list Should the list of files be returned instead of the file
#' @importFrom utils menu installed.packages
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom openssl aes_cbc_decrypt
#' @returns Downloads necessary annotation files from Figshare and reads them
#' in as data frames.
#' @export
#'
#' @examples \donttest{
#'
#' get_figshare(
#'   return_list = TRUE,
#'   output_dir = tempdir()
#' )
#'
#' get_figshare(
#'   file_name = "Achilles_common_essentials.csv",
#'   output_dir = tempdir()
#' )
#' }
get_figshare <- function(file_name = NA,
                         item = "19700056",
                         output_dir = tempdir(),
                         return_list = FALSE) {
  if (is.null(output_dir)) output_dir <- system.file("extdata", package = "gimap")

  decrypted <- openssl::aes_cbc_decrypt(
    readRDS(encrypt_creds_path()),
    key = readRDS(key_encrypt_creds_path())
  )

  url <- file.path("https://api.figshare.com/v2/articles", item)

  # Github api get
  result <- httr::GET(
    url,
    httr::progress(),
    httr::add_headers(
      Authorization = paste0("Bearer ", unserialize(decrypted)$client_secret)
    ),
    httr::accept_json()
  )

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text",
    encoding = "UTF-8"
  )
  result_list <- jsonlite::fromJSON(result_content)

  if (return_list) {
    return(result_list$files)
  }
  file_id <- result_list$files %>%
    dplyr::filter(name == file_name) %>%
    dplyr::pull(id)

  message("Downloading: ", file_name)
  result <- httr::GET(
    file.path("https://api.figshare.com/v2/file/download/", file_id),
    httr::progress(),
    httr::add_headers(
      Authorization = paste0("Bearer ", unserialize(decrypted)$client_secret)
    ),
    httr::accept_json()
  )

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  result_content <- httr::content(result, "text",
    encoding = "UTF-8"
  )

  writeLines(result_content, file.path(output_dir, file_name))

  return(file.path(output_dir, file_name))
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Refresh the example data files by redownloading them
#' @description This function will set example data file options to NULL so files
#' will be re-downloaded
#' @export
#' @return options for example data are are set to NULL.
#' @examples
#'
#' delete_example_data()
#'
delete_example_data <- function() {
  data_list <- list(
    "count" = NULL,
    "count_treatment" = NULL,
    "meta" = NULL,
    "gimap" = NULL,
    "gimap_treatment" = NULL,
    "annotation" = NULL
  )

  message("Deleting the example data files listed in options")
  unlink(options(names(data_list)))

  # Set options as NULL
  options(data_list)
}
