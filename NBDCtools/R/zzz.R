.onLoad <- function(libname, pkgname) {
  # if data pkg is installed, load it first
  if (
    !is_on_cran() &&
    length(find.package("NBDCtoolsData", quiet = TRUE)) > 0
  ) {
    utils::data(
      list = c(
        "lst_dds",
        "lst_levels",
        "lst_sessions"
      ),
      package = "NBDCtoolsData",
      envir = asNamespace(pkgname)
    )
  }
  if (is_on_cran()) {
    purrr::walk(
      c("lst_dds", "lst_levels", "lst_sessions"),
      ~ readRDS(system.file(
        "extdata", "meta_internal", glue::glue("{.x}.rds"),
        package = pkgname
      )) |>
        assign(.x, value = _, envir = asNamespace(pkgname))
    )
  }
}

.onAttach <- function(libname, pkgname) {
  if (!getOption("nbdctools_start_msg_displayed", FALSE)) {
    packageStartupMessage(glue::glue(
      "Welcome to the `NBDCtools` package! For more information, ",
      "visit: https://software.nbdc-datahub.org/NBDCtools/"
    ))
    packageStartupMessage(glue::glue(
      "This package is developed by the ABCD Data Analysis, Informatics & ",
      "Resource Center (DAIRC) at the J. Craig Venter Institute (JCVI)"
    ))
    # TODO uncomment once we have a paper
    # packageStartupMessage(glue::glue(
    #   "If `NBDCtools` is helpful to your research, ",
    #   "please kindly cite it as:\n",
    #   "L Zhang, xxx & J Linkersdörfer. NBDCtools: xxx. 2025. xxx",
    # ))
    options(nbdctools_start_msg_displayed = TRUE)
  }

  if (!is_on_cran() && length(find.package("NBDCtoolsData", quiet = TRUE)) < 1) {
    cli::cli_warn(
      c(
        "x" = glue::glue(
          "The `NBDCtoolsData` package cannot be found. Please install it ",
          "using: remotes::install_github('nbdc-datahub/NBDCtoolsData')"
        )
      )
    )
  }
}
