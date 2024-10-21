#' Compare Input and Updated Tibbles
#'
#' This function compares the original input tibble and the updated tibble,
#' identifying and reporting any changes in the specified columns (`entry_name`, `protein_name`, `gene_name`).
#'
#' @param original_tibble The original tibble before processing.
#' @param updated_tibble The tibble returned after processing.
#' @param entry_name_col The column name for entry names (default: "entry_name").
#' @param protein_name_col The column name for protein names (default: "protein_name").
#' @param gene_name_col The column name for gene names (default: "gene_name").
#' @return None. Prints the differences between the tibbles.
#' @import glue cli
#' @export
#'
#' @examples
#' # Example usage:
#' \donttest{
#' # Original input tibble
#' input_data <- tibble::tibble(
#'   id = c(1, 2),
#'   species = c("mouse", "rat"),
#'   sample_type = c("brain", "liver"),
#'   accession = c("O88737", "Q9R064"),
#'   accession_source = c("UniProt", "UniProt")
#' )
#'
#' # Process the tibble (this will add the entry_name, protein_name, and gene_name)
#' processed_data <- process_tibble_uniprot(input_data)
#'
#' # Compare the original and processed tibbles
#' compare_tibbles_uniprot(input_data, processed_data)
#' }
compare_tibbles_uniprot <- function(original_tibble,
                                    updated_tibble,
                                    entry_name_col = "entry_name",
                                    protein_name_col = "protein_name",
                                    gene_name_col = "gene_name") {
  # Initialize a vector to collect messages
  message_log <- c()

  # Add missing columns to the original tibble (with NA values)
  if (!entry_name_col %in% colnames(original_tibble)) {
    original_tibble[[entry_name_col]] <- NA
  }
  if (!protein_name_col %in% colnames(original_tibble)) {
    original_tibble[[protein_name_col]] <- NA
  }
  if (!gene_name_col %in% colnames(original_tibble)) {
    original_tibble[[gene_name_col]] <- NA
  }

  # Loop through each row and compare the columns
  for (i in seq_len(nrow(original_tibble))) {
    # Track if any changes were made
    changes_made <- FALSE

    # Compare entry_name, handling NA values properly
    if (!isTRUE(all.equal(original_tibble[[entry_name_col]][i], updated_tibble[[entry_name_col]][i], check.attributes = FALSE))) {
      message_log <- c(message_log, glue::glue("Row {i}: entry_name updated from {original_tibble[[entry_name_col]][i]} to {updated_tibble[[entry_name_col]][i]}"))
      changes_made <- TRUE
    }

    # Compare protein_name, handling NA values properly
    if (!isTRUE(all.equal(original_tibble[[protein_name_col]][i], updated_tibble[[protein_name_col]][i], check.attributes = FALSE))) {
      message_log <- c(message_log, glue::glue("Row {i}: protein_name updated from {original_tibble[[protein_name_col]][i]} to {updated_tibble[[protein_name_col]][i]}"))
      changes_made <- TRUE
    }

    # Compare gene_name, handling NA values properly
    if (!isTRUE(all.equal(original_tibble[[gene_name_col]][i], updated_tibble[[gene_name_col]][i], check.attributes = FALSE))) {
      message_log <- c(message_log, glue::glue("Row {i}: gene_name updated from {original_tibble[[gene_name_col]][i]} to {updated_tibble[[gene_name_col]][i]}"))
      changes_made <- TRUE
    }

    if (!changes_made) {
      message_log <- c(message_log, glue::glue("Row {i}: No changes detected."))
    }
  }

  message_log <- c(message_log, "Comparison completed.")

  # Print the log to the console (you can still have console output)
  cli::cli_inform(message_log)

  # Return the message log for testing purposes
  return(message_log)
}
