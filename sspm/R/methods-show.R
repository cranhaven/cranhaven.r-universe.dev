# Methods -----------------------------------------------------------------

custom_h1 <- function(text) {
  cli::cat_line(
    cli::symbol$figure_dash, cli::symbol$figure_dash,
    " ", cli::style_bold(text), " ",
    cli::symbol$figure_dash, cli::symbol$figure_dash)
}

setMethod("show",
          "sspm_boundary",
          function(object) {
            cli::cat_line()
            custom_h1(paste0("Boundaries"))
            cat_boundaries(object)
            cli::cat_line()
          }
)

setMethod("show",
          "sspm_discrete_boundary",
          function(object) {
            cli::cat_line()
            if (!is.null(dim(object@boundaries))) {
              custom_h1(paste0("Boundaries ", cli::col_green("(Discrete)")))
            }
            cat_boundaries(object)
            cli::cat_line()
          }
)

setMethod("show",
          "sspm_dataset",
          function(object) {
            cli::cat_line()

            if (object@is_mapped) {
              custom_h1(paste0("Dataset ", cli::col_blue(object@name), cli::col_green(" (Mapped)")))
            } else {
              custom_h1(paste0("Dataset ", cli::col_blue(object@name)))
            }
            cat_data(object)
            cat_smoothed_data(object)
            cli::cat_line()
          }
)

setMethod("show",
          "discretization_method",
          function(object) {
            cli::cat_line()
            custom_h1("Discretization method")
            cli::cat_bullet(" Name : '", object@name, "'",
                            bullet = "arrow_right")
            cli::cat_line()
          }
)

setMethod("show",
          "sspm_formula",
          function(object) {
            cli::cat_line()
            custom_h1("Formula")
            cli::cat_bullet(" Response : ",
                            object@response,
                            bullet = "arrow_right")
            cli::cat_bullet(" Raw : ",
                            format_formula(object@raw_formula),
                            bullet = "arrow_right")
            cli::cat_bullet(" Translated : ",
                            format_formula(object@translated_formula),
                            bullet = "arrow_right")
            cli::cat_bullet(" Variables : ",
                            paste0(names(object@vars), collapse = ", "),
                            bullet = "arrow_right")
            cli::cat_line()
          }
)

setMethod("show",
          "sspm",
          function(object) {
            cli::cat_line()
            n_datasets <- length(object@datasets)
            custom_h1(paste0("Model ",
                             cli::pluralize(cli::col_green("({n_datasets} dataset{?s})"))))
            cat_smoothed_data(object)
            cli::cat_line()
          }
)

setMethod("show",
          "sspm_fit",
          function(object) {
            cli::cat_line()
            custom_h1("Model fit")
            cat_smoothed_data(object, prints = FALSE)
            cli::cat_line()
          }
)

# Helpers -----------------------------------------------------------------

cat_boundaries <- function(object) {

  print_not_init <- FALSE

  if (checkmate::test_class(object, "sspm_boundary")) {
    if (!is.null(dim(object@boundaries))) {
      ok_to_print <- TRUE
    } else {
      ok_to_print <- FALSE
      print_not_init <- TRUE
    }
  } else if (checkmate::test_class(object, "sspm_dataset")) {
    if (!is.null(dim(object@boundaries@boundaries))) {
      ok_to_print <- TRUE
    } else {
      ok_to_print <- FALSE
    }
  } else {
    ok_to_print <- FALSE
  }

  if (print_not_init){
    cli::cli_alert_info(" Boundaries not initialized")
  } else if (ok_to_print) {
    cli::cat_bullet(" ",
                    pluralize_data_info(object@boundaries),
                    bullet = "arrow_right")

    if (checkmate::test_class(object, "sspm_discrete_boundary")) {
      if (!is.null(dim(object@boundaries))) {
        cat_discretization_info(object)
      }
    }

    cli::cat_bullet(" Column : ",
                    cli::col_blue(object@boundary),
                    bullet = "arrow_right")

    cli::cat_bullet(" Area : ",
                    cli::col_blue(object@boundary_area),
                    bullet = "arrow_right")

    # TODO add " Patches area col. :
  }

}

cat_discretization_info <- function(object) {

  if(!is.null(nrow(object@points))){
    cli::cat_line("  ", paste(cli::symbol$star, cli::col_green("Points"),
                              cli::symbol$em_dash,
                              pluralize_data_info(object@points, dim_1_name = "feature")))
  }
  if(!is.null(object@patches)){
    cli::cat_line("  ", paste(cli::symbol$star, cli::col_green("Patches"),
                              cli::symbol$em_dash,
                              pluralize_data_info(object@patches, dim_1_name = "feature")))
  }

}

cat_data <- function(object) {

  header <- " "

  cli::cat_bullet(header,
                  pluralize_data_info(object@data),
                  bullet = "arrow_right")

  # cli::cat_bullet(" Unique ID : ",
  #                 cli::col_blue(object@uniqueID),
  #                 bullet = "arrow_right")

  # if (!is.null(object@coords)) {
  #   cli::cat_bullet(" Coords : ",
  #                   paste(cli::col_green(object@coords),
  #                         collapse = ", "),
  #                   bullet = "arrow_right")
  # }

  if (!is.null(object@biomass)) {
    cli::cat_bullet(" Biomass : ",
                    paste(cli::col_green(object@biomass),
                          collapse =  paste0(" ", cli::symbol$em_dash, " ")),
                    bullet = "arrow_right")
  }

  if (!is.null(object@density)) {
    cli::cat_bullet(" Density : ",
                    paste(cli::col_green(object@density),
                          collapse =  paste0(" ", cli::symbol$em_dash, " ")),
                    bullet = "arrow_right")
  }

  cli::cat_bullet(" Time : ",
                  cli::col_green(object@time),
                  bullet = "arrow_right")

}

cat_smoothed_data <- function(object, prints = TRUE) {

  if (!is.null(object@smoothed_data)) {

    if (("train_test" %in% names(object@smoothed_data))) {

      n_train <- sum(object@smoothed_data$train_test)
      n_test <- sum(!object@smoothed_data$train_test)

      split_info <- paste0(" [", cli::col_blue(n_train),
                           cli::col_yellow(" train, "),
                           cli::col_blue(n_test),
                           cli::col_yellow(" test"), "]")

      cli::cat_bullet(" Smoothed data : ",
                      pluralize_data_info(object@smoothed_data),
                      " /", split_info,
                      bullet = "arrow_right")


    } else {
      cli::cat_bullet(" Smoothed data : ",
                      pluralize_data_info(object@smoothed_data),
                      bullet = "arrow_right")
    }

    if (prints) {

      columns_with_smooth <- object@smoothed_vars

      columns_with_catch <-
        names(which(sapply(colnames(object@smoothed_data),
                           grepl, pattern = "_with_catch", fixed = TRUE)))

      columns_with_lag <-
        names(which(sapply(colnames(object@smoothed_data),
                           grepl, pattern = "_lag", fixed = TRUE)))

      columns_with_smooth <- columns_with_smooth[!(columns_with_smooth %in%
                                                     c(columns_with_catch,
                                                       columns_with_lag))]

      if (length(columns_with_smooth) > 0) {
        the_line <-
          paste(cli::symbol$star, "Smoothed vars:",
                paste(cli::col_green(sort(columns_with_smooth)),
                      collapse = paste0(" ", cli::symbol$em_dash, " ")))

        cli::cat_line("   ", the_line)
      }

      if (length(columns_with_catch) > 0) {
        the_line <-
          paste(cli::symbol$star, "Vars with catch:",
                paste(cli::col_green(sort(columns_with_catch)),
                      collapse = paste0(" ", cli::symbol$em_dash, " ")))

        cli::cat_line("   ", the_line)
      }

      if (length(columns_with_lag) > 0) {
        the_line <-
          paste(cli::symbol$star, "lagged vars:",
                paste(cli::col_green(sort(columns_with_lag)),
                      collapse = paste0(" ", cli::symbol$em_dash, " ")))

        cli::cat_line("   ", the_line)
      }
    }
  }

}

# -------------------------------------------------------------------------

pluralize_data_info <- function(object,
                                dim_1_name = "row",
                                dim_2_name = "column") {

  dim_1 <- dim(object)[1]
  dim_2 <- dim(object)[2]

  info <-
    cli::pluralize("[", cli::col_blue("{dim_1}"), " ", dim_1_name, "{?s}, ",
                   cli::col_blue("{dim_2}"), " ", dim_2_name, "{?s}]")

  return(info)

}

format_formula <- function(form) {
  gsub(format(
    paste0(trimws(format(form)), collapse = " ")
  ), pattern = "\\\"", replacement = "'")
}
