plot_multi_histogram <- function(df, feature, label_column, hist = FALSE) { # function to create nice multi histograms, taken somewhere from the web
  plt <- ggplot2::ggplot(df, ggplot2::aes(x = eval(parse(text = feature)), fill = eval(parse(text = label_column)))) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(eval(parse(text = feature)))), color = "black", linetype = "dashed", linewidth = 1) + ## this makes a vertical line of the mean
    ggplot2::labs(x = feature, y = "Density") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = label_column))

  if (hist == TRUE) plt + ggplot2::geom_histogram(alpha = 0.7, position = "identity", ggplot2::aes(y = ..density..), color = "black")

  return(plt)
}


## The function below is intended to find a dataframe called $coef in the output
## regardless of the output data structure, which can vary
find_dataframe <- function(list_object, dataframe_name) {
  # Check if the current object is a list
  if (is.list(list_object)) {
    # Check if the dataframe_name exists in the names of the current list object
    if (dataframe_name %in% names(list_object)) {
      # Check if the object corresponding to dataframe_name is a data frame
      if (is.data.frame(list_object[[dataframe_name]])) {
        return(list_object[[dataframe_name]]) # Return the data frame if found
      }
    }

    # Recursively search through each element of the current list object
    for (element in list_object) {
      result <- find_dataframe(element, dataframe_name)
      if (!is.null(result)) {
        return(result) # Return the data frame if found in any nested list
      }
    }
  }

  return(NULL) # Return NULL if dataframe_name not found
}



make_md <- function(f = file) {
  rmarkdown::render("simulation_output.rmd",
    output_file = paste0(
      stringr::str_remove_all(
        file, "parameters_|.R$"
      ), ".html"
    ),
    params = list(file = file)
  )
}



#' Modify bcoeff Names
#'
#' This function modifies the names of beta coefficients (`bcoeff`) by removing
#' dots and underscores to ensure consistent naming. If the names are already modified,
#' the function skips processing.
#'
#' @param bcoeff A named list of beta coefficients. The names of this list are
#'   the coefficients used in the utility function.
#'
#' @return A list containing:
#' \describe{
#'   \item{bcoeff}{The modified `bcoeff` list with updated names.}
#'   \item{bcoeff_lookup}{A tibble mapping the original names to the modified names.}
#' }
#'
#' @keywords internal
modify_bcoeff_names <- function(bcoeff) {
  # Check if bcoeff already has a lookup table attribute
  if (!is.null(attr(bcoeff, "bcoeff_lookup"))) {
    message("bcoeff_lookup already exists; skipping modification.")
    # Retrieve the existing lookup table
    bcoeff_lookup <- attr(bcoeff, "bcoeff_lookup")
  } else {
    # Check if bcoeff names need modification
    if (any(grepl("[._]", names(bcoeff)))) {
      # Create a lookup table
      bcoeff_lookup <- tibble::tibble(
        original = names(bcoeff),
        modified = stringr::str_replace_all(names(bcoeff), "[._]", "")
      )
      # Modify the names in the original bcoeff
      names(bcoeff) <- bcoeff_lookup$modified
    } else {
      # No modification needed; create a trivial lookup table
      bcoeff_lookup <- tibble::tibble(
        original = names(bcoeff),
        modified = names(bcoeff)
      )
    }
    # Attach the lookup table as an attribute to bcoeff
    attr(bcoeff, "bcoeff_lookup") <- bcoeff_lookup
  }

  # Return both modified bcoeff and lookup table
  list(
    bcoeff = bcoeff,
    bcoeff_lookup = bcoeff_lookup
  )
}
