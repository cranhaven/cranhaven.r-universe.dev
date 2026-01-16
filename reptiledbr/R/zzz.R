utils::globalVariables(c(
  ".", "V1", "V2", "attribute", "attribute_value", "comment_detail",
  "comments_info", "common_name", "common_names_list", "data diagnosis_detail",
  "diagnosis_info", "distribution", "distribution_list", "etymology_detail",
  "etymology_info", "format_external_links", "formatted_data", "genus",
  "input_name", "reference", "references_list", "reproduction_detail",
  "reproduction_info", "species", "status", "subspecies",  "subspecies_list",
  "synonym", "synonym_list", "taxa_list", "taxon", "type_detail", "types_info", "value",
  "data", "diagnosis_detail", "x1", "x2", "author", "ephitetho", "epithet",
  "family", "fuzzy_match", "species_author", "species_match",
  "species_name_year", "has_subspecies",  "id", "original_order", "query",
  "subspecies_name", "subspecies_name_author", "subspecies_year",
  "change", "download.file", "rdb_sp_id", "subspecie_author_info",
  "clean_namesedbr"
))

# ---------------------------------------------------------------
# Environment for package-level variables
.pkgenv <- new.env(parent = emptyenv())

#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # Display welcome message
  packageStartupMessage(
    sprintf(
      "Welcome to reptiledbr (%s)\n%s\n%s\n%s",
      utils::packageDescription("reptiledbr", fields = "Version"),
      "This package provides tools to access and query data from the Reptile Database:",
      "  https://reptile-database.reptarium.cz/",
      "Type ?reptiledbr to get started or visit the documentation for examples and guidance."
    )
  )

  # Check if reptiledb.data is available
  if (!requireNamespace("reptiledb.data", quietly = TRUE)) {
    packageStartupMessage(
      paste0(
        "\nNOTE: The 'reptiledb.data' package is **recommended** for full functionality.\n",
        "It provides the necessary data for the following functions:\n",
        "  - `reptiledbr_exact()`\n",
        "  - `reptiledbr_partial()`\n",
        "  - `search_reptiledbr()`\n",
        "  - `list_subspecies_reptiledbr()`\n\n",
        "To install 'reptiledb.data', please use: pak::pak('PaulESantos/reptiledb.data')\n",
        "Alternatively, if you prefer not to use pak: remotes::install_github('PaulESantos/reptiledb.data')"
      )
    )
  }
}

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Silent initialization when the package is loaded
  invisible()
}

#' Check if data package is required and available
#' @keywords internal
check_data_required <- function() {
  if (!requireNamespace("reptiledb.data", quietly = TRUE)) {
    stop(
      "This function requires the 'reptiledb.data' package to be installed.\n",
      "Please install it using: pak::pak('PaulESantos/reptiledb.data')\n",
      "Alternatively: remotes::install_github('PaulESantos/reptiledb.data')",
      call. = FALSE
    )
  }
}
