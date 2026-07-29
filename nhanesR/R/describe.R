#' Attach CDC variable descriptions as Hmisc-style labels
#'
#' Looks up the plain-language CDC description for each column in a NHANES
#' data frame and stores it as an \code{"label"} attribute on the column.
#' Hmisc reads these attributes automatically in \code{\link[Hmisc]{describe}},
#' \code{Hmisc::summary()}, \code{Hmisc::html()}, and other label-aware
#' functions, so labelling once makes descriptions available everywhere.
#'
#' @param x A data frame of NHANES data, typically from
#'   \code{\link{nhanes_download_analyte}}.
#' @param descriptions Optional lookup for variable descriptions. May be:
#'   \itemize{
#'     \item \code{NULL} (default): descriptions are loaded from the locally cached
#'       variable catalog. Run any \code{\link{nhanes_search_variables}} call first
#'       to populate the cache.
#'     \item A \code{data.frame} with columns \code{variable_name} and
#'       \code{variable_desc}, such as the output of
#'       \code{\link{nhanes_search_variables}}.
#'     \item A named character vector mapping variable names to descriptions.
#'   }
#'
#' @return \code{x} with \code{"label"} attributes set on each column that
#'   could be matched to a CDC description. Columns with no catalog match are
#'   returned unchanged.
#'
#' @seealso \code{\link{NH_describe}} for a one-step labelled describe;
#'   \code{\link{nhanes_search_variables}} to browse the variable catalog;
#'   \code{\link[Hmisc]{label}} for the Hmisc label convention.
#' @export
#' @examples
#' \donttest{
#' tc <- nhanes_download_analyte("total cholesterol", "2015-2016")
#' tc <- NH_label(tc)
#'
#' # CDC descriptions now appear in all Hmisc label-aware output
#' Hmisc::describe(tc)
#' Hmisc::html(Hmisc::describe(tc))
#' }
NH_label <- function(x, descriptions = NULL) {
  desc_map <- .nhanes_build_desc_map(descriptions)
  if (is.null(desc_map)) return(x)

  for (vn in names(x)) {
    if (vn %in% names(desc_map)) {
      lbl <- trimws(desc_map[[vn]])
      if (!is.na(lbl) && nzchar(lbl)) {
        attr(x[[vn]], "label") <- lbl
      }
    }
  }
  x
}

#' NHANES-annotated variable descriptions
#'
#' Attaches CDC plain-language descriptions as column labels via
#' \code{\link{NH_label}}, then calls \code{\link[Hmisc]{describe}}. This is a
#' convenience wrapper; for repeated use prefer \code{NH_label()} once so that
#' labels persist across all subsequent Hmisc operations.
#'
#' Replicate weights (variables matching \code{REP[0-9]+$}, e.g.
#' \code{WTMREP01}--\code{WTMREP52} and \code{WTIREP01}--\code{WTIREP52}) are
#' suppressed by default because they appear in NHANES DEMO files but are not
#' needed for Taylor-series linearization variance estimation, which is the
#' standard approach for NHANES analysis. Set \code{all_weights = TRUE} to
#' include them.
#'
#' @param x A data frame of NHANES data, typically from
#'   \code{\link{nhanes_download_analyte}}.
#' @param descriptions Optional lookup passed through to \code{\link{NH_label}}.
#'   See that function for accepted forms.
#' @param all_weights Logical. If \code{FALSE} (default), columns whose names
#'   match \code{REP[0-9]+$} (balanced repeated replication weights such as
#'   \code{WTMREP01}--\code{WTMREP52}) are excluded from the output. Set to
#'   \code{TRUE} to include all weight columns.
#' @param ... Additional arguments passed to \code{\link[Hmisc]{describe}}.
#'
#' @return An object of class \code{"describe"} with CDC descriptions embedded
#'   as variable labels.
#'
#' @seealso \code{\link{NH_label}} to attach labels to a data frame for
#'   persistent use; \code{\link[Hmisc]{describe}} for the underlying engine.
#' @export
#' @examples
#' \donttest{
#' tc <- nhanes_download_analyte("total cholesterol", "2015-2016")
#' NH_describe(tc)
#'
#' # Include replicate weights in the output
#' demo_list <- nhanes_download("DEMO", nhanes_cycles()[1:10, "cycle"])
#' demo <- nhanes_stack(demo_list)
#' NH_describe(demo, all_weights = TRUE)
#'
#' # Supply descriptions from a prior nhanes_search_variables() call
#' vars <- nhanes_search_variables("cholesterol")
#' NH_describe(tc, descriptions = vars)
#' }
NH_describe <- function(x, descriptions = NULL, all_weights = FALSE, ...) {
  if (!requireNamespace("Hmisc", quietly = TRUE)) {
    stop(
      "The 'Hmisc' package is required for NH_describe(). ",
      "Install it with: install.packages('Hmisc')",
      call. = FALSE
    )
  }
  if (!all_weights) {
    drop <- grep("REP[0-9]+$", names(x), ignore.case = TRUE)
    if (length(drop)) {
      cli::cli_inform(
        "Hiding {length(drop)} replicate weight column{?s} \\
         ({.val {names(x)[drop[1]]}} \u2013 {.val {names(x)[drop[length(drop)]]}}).  \\
         Use {.code all_weights = TRUE} to include them."
      )
      x <- x[, -drop, drop = FALSE]
    }
  }
  Hmisc::describe(NH_label(x, descriptions = descriptions), ...)
}

# -- Internal helpers ----------------------------------------------------------

#' Build a variable-name -> description lookup from cache or user input
#' @keywords internal
#' @importFrom stats setNames
.nhanes_build_desc_map <- function(descriptions) {
  if (!is.null(descriptions)) {
    if (is.data.frame(descriptions)) {
      if (!all(c("variable_name", "variable_desc") %in% names(descriptions))) {
        stop(
          "'descriptions' data.frame must have columns ",
          "'variable_name' and 'variable_desc'",
          call. = FALSE
        )
      }
      return(setNames(descriptions$variable_desc, descriptions$variable_name))
    }
    if (is.character(descriptions) && !is.null(names(descriptions))) {
      return(descriptions)
    }
    stop(
      "'descriptions' must be a named character vector or a data.frame with ",
      "columns 'variable_name' and 'variable_desc'",
      call. = FALSE
    )
  }

  # Load from the per-component cache files written by nhanes_search_variables()
  components  <- c("demographics", "dietary", "examination",
                   "laboratory", "questionnaire")
  catalog_dir <- .nhanes_cache_subdir("variable_catalog")

  parts <- lapply(components, function(comp) {
    rds_path <- file.path(catalog_dir, paste0(comp, ".rds"))
    if (file.exists(rds_path)) readRDS(rds_path) else NULL
  })
  catalog <- do.call(rbind, Filter(Negate(is.null), parts))

  if (is.null(catalog) || nrow(catalog) == 0L) {
    cli::cli_inform(
      c(
        "!" = "No variable catalog found in cache.",
        "i" = "Run {.code nhanes_search_variables(\"any term\")} once to \\
               populate it, then call {.fn NH_label} or {.fn NH_describe} again."
      )
    )
    return(NULL)
  }

  # One description per variable name (consistent across cycles)
  catalog <- catalog[!duplicated(catalog$variable_name), , drop = FALSE]
  setNames(catalog$variable_desc, catalog$variable_name)
}
