#' Process a Tibble of UniProt Data
#'
#' This function processes a tibble containing accession and accession_source columns.
#' It retrieves data from the UniProt API for rows with accession_source == "UniProt" and
#' overwrites or creates the entry_name, protein_name, and gene_name columns only if the parsed values are not NULL or NA.
#'
#' @param data A tibble containing at least accession and accession_source columns.
#' @param accession_col The column name for accession numbers (default: "accession").
#' @param accession_source_col The column name for accession sources (default: "accession_source").
#' @param entry_name_col The column name for entry names (default: "entry_name").
#' @param protein_name_col The column name for protein names (default: "protein_name").
#' @param gene_name_col The column name for gene names (default: "gene_name").
#' @return A tibble with UniProt data processed.
#' @export
#'
#' @examples
#' # Example usage:
#' \donttest{
#' # Load necessary library
#' library(tibble)
#'
#' # Reduced example data as an R tibble
#' test_data <- tibble::tibble(
#'   id = c(1, 78, 83, 87),
#'   species = c("mouse", "mouse", "rat", "mouse"),
#'   sample_type = c("brain", "brain", "brain", "brain"),
#'   accession = c("O88737", "O35927", "Q9R064", "P51611"),
#'   accession_source = c("OtherDB", "UniProt", "UniProt", "UniProt"),
#'   entry_name = c("BSN_MOUSE", NA, "GORS2_RAT", NA),
#'   protein_name = c("Protein bassoon", NA, "Golgi reassembly-stacking protein2", NA),
#'   gene_name = c("Bsn", NA, "Gorasp2", NA)
#' )
#'
#' # Process the tibble
#' result_data <- process_tibble_uniprot(test_data)
#'
#' # Compare the original and processed tibbles
#' compare_tibbles_uniprot(test_data, result_data)
#' }
process_tibble_uniprot <- function(data,
                                   accession_col = "accession",
                                   accession_source_col = "accession_source",
                                   entry_name_col = "entry_name",
                                   protein_name_col = "protein_name",
                                   gene_name_col = "gene_name") {
  # Ensure required columns exist
  required_cols <- c(accession_col, accession_source_col)
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing required columns: {missing_cols}")
  }

  # Add columns if they don't exist
  if (!entry_name_col %in% colnames(data)) {
    data[[entry_name_col]] <- NA
  }
  if (!protein_name_col %in% colnames(data)) {
    data[[protein_name_col]] <- NA
  }
  if (!gene_name_col %in% colnames(data)) {
    data[[gene_name_col]] <- NA
  }

  # Iterate through each row
  for (i in seq_len(nrow(data))) {
    # Check if the accession_source is UniProt
    if (data[[accession_source_col]][i] == "UniProt") {
      cli::cli_inform("Processing accession: {data[[accession_col]][i]}")

      # Retrieve UniProt data
      uniprot_data <- retrieve_uniprot_data(data[[accession_col]][i])

      # Parse UniProt data
      parsed_data <- parse_uniprot_data(uniprot_data)

      # Only update the columns if the parsed value is not NULL or NA
      if (!is.null(parsed_data$entry_name) && !is.na(parsed_data$entry_name)) {
        data[[entry_name_col]][i] <- parsed_data$entry_name
      }

      if (!is.null(parsed_data$protein_name) && !is.na(parsed_data$protein_name)) {
        data[[protein_name_col]][i] <- parsed_data$protein_name
      }

      if (!is.null(parsed_data$gene_name) && !is.na(parsed_data$gene_name)) {
        data[[gene_name_col]][i] <- parsed_data$gene_name
      }

      cli::cli_inform("Data updated for row {i}")
    } else {
      cli::cli_inform("Skipping non-UniProt row {i}")
    }
  }

  cli::cli_alert_success("Processing completed for all rows.")
  return(data)
}
