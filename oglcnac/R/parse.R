#' Parse UniProt Data
#'
#' This function parses the data retrieved from the UniProt API to extract
#' the entry name, protein name, and gene name.
#'
#' @param uniprot_data A list returned by the UniProt API query.
#' @return A list containing `entry_name`, `protein_name`, and `gene_name`.
#' @import cli
#' @export
#'
#' @examples
#' # Example usage:
#' \donttest{
#' # Retrieve UniProt data
#' test_result <- retrieve_uniprot_data("O88737")
#'
#' # Parse the UniProt data
#' parsed_result <- parse_uniprot_data(test_result)
#'
#' # Print the parsed result
#' print(parsed_result)
#' }
parse_uniprot_data <- function(uniprot_data) {
  # Use cli to indicate the start of parsing
  cli::cli_alert_info("Parsing UniProt data...")

  # Extract the UniProt ID (entry_name)
  entry_name <- uniprot_data$uniProtkbId
  cli::cli_alert_success("Entry name retrieved: {entry_name}")

  # Extract the recommended protein name
  protein_name <- uniprot_data$proteinDescription$recommendedName$fullName$value
  cli::cli_alert_success("Protein name retrieved: {protein_name}")

  # Extract the gene name (check if there are multiple gene names)
  if (!is.null(uniprot_data$genes)) {
    gene_name <- uniprot_data$genes$geneName$value
    cli::cli_alert_success("Gene name retrieved: {gene_name}")
  } else {
    gene_name <- NA # If gene name is not available, set as NA
    cli::cli_alert_warning("Gene name not available")
  }

  # Return the parsed data as a list
  result <- list(
    entry_name = entry_name,
    protein_name = protein_name,
    gene_name = gene_name
  )

  cli::cli_alert_info("Parsing completed.")

  return(result)
}
