#' @title Cite a Finna collection
#'
#' @description
#' Automatically generates a citation for a Finna collection result.
#'
#' @param result The Finna collection result as a tibble.
#' @param index The index of the collection to cite (numeric).
#' @param style The citation style to use (default: "citation"). See \code{\link[utils]{bibentry}}.
#' @return A bibliographic entry (\code{bibentry}) printed in the specified style.
#'
#' @export
finna_cite <- function(result, index, style = "citation") {
  # Validate the input structure
  if (!"value" %in% names(result) || !"translated" %in% names(result) ||
      !"count" %in% names(result) || !"href" %in% names(result)) {
    stop("Invalid result format: expected a tibble with columns 'value', 'translated', 'count', and 'href'.")
  }

  if (index < 1 || index > nrow(result)) {
    stop("Invalid index: out of bounds.")
  }

  # Extract the specific row corresponding to the index
  collection <- result[index, ]

  # Generate citation using bibentry
  citation <- utils::bibentry(
    bibtype = "Misc",
    title = paste("Finna Collection -", collection$translated),
    author = utils::person("Finna API"),
    organization = collection$translated,
    year = format(Sys.Date(), "%Y"),
    url = paste0("https://finna.fi", collection$href),
    note = sprintf(
      "This citation was generated using the Finna R package on %s.",
      Sys.Date()
    )
  )

  # Print citation in the specified style
  print(citation, style = style)
}

# Example usage
# finna_result <- fetch_finna(query = "oai_dc", lng = "en")
# finna_cite(finna_result, 4)  # Cite the Jyväskylän yliopisto collection
