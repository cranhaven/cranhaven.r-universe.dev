#' Aggregate Simulation Results
#'
#' Processes the simulation results to extract summaries, coefficients, and graphs.
#'
#' @param all_designs A list of simulation results from sim_choice. Can contain different designs but need to have the common structure returned by simchoice
#' @param fromfolder A folder from where to read simulations. If provided, the function will read all .qs files from the folder and process them. The files are usually saved by your earlier work and should be qs files as they are more efficient that rds files.
#' @return A list with aggregated results including summary, coefficients, graphs, and power.
#' @export
aggregateResults <- function(all_designs, fromfolder = NULL) {
  if (!is.null(fromfolder)) {
    if (!dir.exists(fromfolder)) stop("Folder from where to read simulations does not exist.")

    designs <- list.files(path = fromfolder, pattern = "*.qs", full.names = TRUE)

    purrr::map(designs, qs::qread) %>% stats::setNames(basename(designs))
  }


  designname <- all_designs[["arguements"]][["designname"]]
  reshape_type <- all_designs[["arguements"]][["Reshape Type"]]
  bcoeff <- all_designs[["arguements"]][["Beta values"]]

  powa <- all_designs |>
    purrr::map(~ .x$power) |>
    purrr::compact()

  summaryall <- as.data.frame(purrr::compact(purrr::map(all_designs, ~ .x$summary))) %>% ## purrr::compact to remove all NULL
    dplyr::select(!dplyr::ends_with("vars")) %>%
    tibble::rownames_to_column("parname") %>%
    dplyr::mutate(parname = stringr::str_remove(parname, "^est_")) %>%
    dplyr::left_join(
      data.frame(truepar = unlist(bcoeff)) %>% tibble::rownames_to_column("parname") %>%
        dplyr::mutate(parname = stringr::str_replace_all(parname, "\\.", "_")), ## because parameters have been renamed for mixl, we have to make sure we substitute all . with _
      by = "parname"
    ) %>%
    dplyr::relocate(parname, dplyr::ends_with(c(
      ".n", "truepar", "mean", "sd", "min", "max", "range", "se"
    )))

  coefall <- purrr::compact(purrr::map(all_designs, ~ .x$coefs))

  pat <- paste0("(", paste(designname, collapse = "|"), ").") # needed to identify pattern to be replaced

  preprocessed <- as.data.frame(coefall) %>%
    dplyr::select(!dplyr::matches("pval|run")) %>%
    dplyr::rename_with(~ sub("est_", "", .x), dplyr::everything()) %>%
    dplyr::rename_with(
      ~ paste0(., "_", stringr::str_extract(., pat)),
      dplyr::everything()
    ) %>% # rename attributes for reshape part 1
    dplyr::rename_with(
      ~ stringr::str_replace(.,
        pattern = pat, replacement =
          ""
      ),
      dplyr::everything()
    )


  s <- switch(reshape_type,
    "stats" = {
      message("Using stats::reshape for reshaping...")
      stats::reshape(
        preprocessed,
        varying = 1:ncol(preprocessed),
        sep = "_",
        direction = "long",
        timevar = "design"
      ) %>%
        dplyr::select(-id)
    },
    "tidyr" = {
      message("Using tidyr::pivot_longer for reshaping...")
      tidyr::pivot_longer(
        preprocessed,
        cols = dplyr::everything(),
        names_to = c(".value", "design"),
        names_sep = "_",
        values_drop_na = TRUE
      )
    },
    "auto" = {
      tryCatch(
        {
          message("Trying tidyr::pivot_longer for reshaping...")
          tidyr::pivot_longer(
            preprocessed,
            cols = dplyr::everything(),
            names_to = c(".value", "design"),
            names_sep = "_",
            values_drop_na = TRUE
          )
        },
        error = function(e) {
          message("tidyr::pivot_longer failed, falling back to stats::reshape...")
          stats::reshape(
            preprocessed,
            varying = 1:ncol(preprocessed),
            sep = "_",
            direction = "long",
            timevar = "design"
          ) %>%
            dplyr::select(-id)
        }
      )
    },
    stop("You need to specify either 'tidyr', 'stats', or 'auto' as the reshape_type")
  )


  p <- list()

  for (att in names(dplyr::select(s, -c("design")))) {
    p[[att]] <- plot_multi_histogram(s, att, "design")

    print(p[[att]])
  }

  all_designs[["summaryall"]] <- summaryall
  all_designs[["graphs"]] <- p
  all_designs[["powa"]] <- powa


  return(all_designs)
}
