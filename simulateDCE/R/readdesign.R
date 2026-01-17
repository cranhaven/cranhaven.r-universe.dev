#'  Creates a dataframe with the design.
#'
#' @param design The path to a design file
#' @param designtype Is it a design created with ngene, spdesign or idefix. use 'ngene', 'spdesign' or 'idefix. Ngene designs should be stored as the standard .ngd output. spdesign should be the spdesign object stored as an RDS file. Idefix objects should also be stored as an RDS file. If designtype is not specified, I try to guess what it is. This is especially helpful if you want to carry out a simulation for both spdesign designs and ngene designs at the same time.
#' @param destype Deprecated. Use designtype instead.
#' @return a dataframe
#' @export
#'
#' @examples library(simulateDCE)
#' mydesign <- readdesign(
#'   system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE"),
#'   "ngene"
#' )
#'
#' print(mydesign)
#'
readdesign <- function(design = designfile, designtype = NULL, destype = NULL) {
  # Ensure both designtype and destype are not set simultaneously
  if (!is.null(designtype) && !is.null(destype)) {
    stop("Both 'designtype' and 'destype' cannot be specified at the same time. Please use only 'designtype'.")
  }

  # Handle deprecated 'destype' argument
  if (!is.null(destype)) {
    message("'destype' is deprecated. Please use 'designtype' instead.")
    designtype <- destype
  }



  if (is.null(designtype)) {
    # Check if the string ends with ".ngd"
    if (grepl("\\.ngd$", design)) {
      # Code to execute if condition is true
      designtype <- "ngene"
      message("I guessed it is an ngene file")
    } else if ("spdesign" %in% class(readRDS(design))) {
      designtype <- "spdesign"
      message("I assume it is a spdesign.")
    } else {
      designtype <- "idefix"
      message("I assume it is an idefix design.")
    }
  }


  read_test <- function() {
    designf <- readRDS(design)
    if (is.list(designf) & !is.data.frame(designf)) {
      if (!"design" %in% names(designf)) {
        stop("The 'design' list element is missing. Make sure to provide a proper spdesign object.")
      }
      designf <- designf[["design"]] %>%
        as.data.frame()
    }
    as.data.frame(designf)
  }
  idefix <- function() {
    # Process the data
    read_test() %>%
      tibble::rownames_to_column(var = "row_id") %>%
      dplyr::filter(!grepl("no.choice", row_id)) %>% # Exclude no.choice rows
      dplyr::select(!dplyr::contains("cte")) %>% # Drop unnecessary columns
      dplyr::mutate(
        # Extract Choice.situation as number after 'set'
        Choice.situation = as.integer(sub("^set(\\d+).*", "\\1", row_id)),
        # Extract alt as the alternative identifier
        alt = sub(".*\\.", "", row_id)
      ) %>%
      dplyr::select(-"row_id") %>% # Drop the original row_id
      tidyr::pivot_wider(
        id_cols = "Choice.situation", # Group by Choice.situation
        names_from = "alt", # Use alt to create column suffixes
        values_from = -c("Choice.situation", "alt"), # Values from other columns
        names_glue = "{alt}.{.value}" # Custom naming convention
      )
  }

  design <- switch(designtype,
    "ngene" = suppressWarnings(readr::read_delim(design,
      delim = "\t",
      escape_double = FALSE,
      trim_ws = TRUE,
      col_select = c(-Design, -tidyr::starts_with("...")),
      name_repair = "universal", show_col_types = FALSE, guess_max = Inf
    )) %>%
      dplyr::filter(!is.na(Choice.situation)),
    "spdesign" = {
      read_test() %>%
        dplyr::mutate(Choice.situation = 1:dplyr::n()) %>%
        dplyr::rename_with(~ stringr::str_replace(., pattern = "_", "\\."), tidyr::everything()) %>%
        dplyr::rename_with(~ dplyr::case_when(
          . == "block" ~ "Block",
          TRUE ~ .
        ), tidyr::everything())
    },
    "idefix" = idefix(),
    stop("Invalid value for design. Please provide either NULL, 'ngene', 'spdesign'or 'idefix',  or do not use the argument 'designtype'. NULL lets us to guess the design.")
  )
}
