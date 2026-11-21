# Multi-Format Document Export
#
# Composable export pipelines serializing tables and reports to Word (.docx),
# PowerPoint (.pptx), and multi-sheet Excel (.xlsx) workbooks with styles.

#########
# EXPORT VALIDATION AND FILE TRANSACTION PIPELINE
# Safe file replacement, path normalization, and backend dependency guards.

#' Ensure required export backend package is installed
#' @keywords internal
#' @noRd
.require_export_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    simtab_abort_dependency(
      sprintf("Package '%s' required. Install with: install.packages('%s')", pkg, pkg)
    )
  }
  invisible(TRUE)
}

#' Reject export inputs that are neither SimtablR objects nor data frames
#' @keywords internal
#' @noRd
.check_exportable <- function(x) {
  if (inherits(x, "simtab") || is.data.frame(x)) {
    return(invisible(x))
  }
  simtab_abort_input(c(
    "{.arg x} must be a SimtablR result, specification, report, or data frame.",
    "i" = "Received an object of class {.cls {class(x)[[1]]}}.",
    "v" = "Export the result of {.fn table1}, {.fn tb}, {.fn regtab}, or another SimtablR function."
  ))
}

#' Validate and normalize a user-selected export path
#'
#' @keywords internal
#' @noRd
.prepare_export_path <- function(
    path,
    extensions,
    default_extension = extensions[[1]],
    overwrite = FALSE
) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    simtab_abort_export(c(
      "{.arg path} must be one non-empty file path.",
      "i" = "The export destination was missing or was not a scalar string.",
      "v" = "Pass an explicit writable file path."
    ))
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    simtab_abort_export(c(
      "{.arg overwrite} must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received {.val {overwrite}}.",
      "v" = "Use {.code overwrite = TRUE} only when replacing the destination is intended."
    ))
  }

  extensions <- tolower(sub("^[.]", "", extensions))
  default_extension <- tolower(sub("^[.]", "", default_extension))
  supplied_extension <- tolower(tools::file_ext(basename(path)))
  if (!nzchar(supplied_extension)) {
    path <- paste0(path, ".", default_extension)
    supplied_extension <- default_extension
  } else if (!supplied_extension %in% extensions) {
    expected <- paste0(".", extensions)
    simtab_abort_export(c(
      "Incompatible export suffix {.val {paste0('.', supplied_extension)}}.",
      "i" = "This writer accepts: {.val {expected}}.",
      "v" = "Remove the suffix to use {.val {paste0('.', default_extension)}}, or supply a compatible suffix."
    ))
  }

  if (dir.exists(path)) {
    simtab_abort_export(c(
      "Export destination is a directory: {.path {path}}.",
      "i" = "A file path is required.",
      "v" = "Append a file name, or choose another destination."
    ))
  }
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    simtab_abort_export(c(
      "Export directory does not exist: {.path {parent}}.",
      "i" = "No file or temporary artifact was written.",
      "v" = "Create the directory first, or choose an existing directory."
    ))
  }
  if (file.exists(path) && !overwrite) {
    simtab_abort_export(c(
      "File already exists: {.file {path}}.",
      "i" = "The existing file was left untouched.",
      "v" = "Choose another path, or pass {.code overwrite = TRUE}."
    ))
  }

  list(
    path = path,
    extension = supplied_extension,
    overwrite = overwrite
  )
}

#' Write an export to a temporary file and publish it safely
#' @keywords internal
#' @noRd
.write_export_transaction <- function(target, writer) {
  path <- target$path
  temporary <- tempfile(
    pattern = ".simtablr-export-",
    tmpdir = dirname(path),
    fileext = paste0(".", target$extension)
  )
  backup <- NULL

  on.exit({
    if (file.exists(temporary)) {
      unlink(temporary)
    }
    if (!is.null(backup) && file.exists(backup)) {
      if (!file.exists(path)) {
        file.rename(backup, path)
      } else {
        unlink(backup)
      }
    }
  }, add = TRUE)

  tryCatch(
    writer(temporary),
    error = function(error) {
      simtab_abort_export(c(
        "Could not write export to {.file {path}}.",
        "i" = "The backend reported: {conditionMessage(error)}",
        "v" = "Check the destination and export options; the destination was not changed."
      ))
    }
  )
  if (!file.exists(temporary)) {
    simtab_abort_export(c(
      "The export backend did not create {.file {path}}.",
      "i" = "No destination file was published.",
      "v" = "Check the export backend and supplied options."
    ))
  }

  if (file.exists(path)) {
    backup <- tempfile(
      pattern = ".simtablr-backup-",
      tmpdir = dirname(path),
      fileext = paste0(".", target$extension)
    )
    if (!file.rename(path, backup)) {
      simtab_abort_export(c(
        "Could not replace the existing file {.file {path}}.",
        "i" = "The file may be open or protected; it remains untouched.",
        "v" = "Close the file, or choose another destination."
      ))
    }
  }

  if (!file.rename(temporary, path)) {
    if (!is.null(backup) && file.exists(backup)) {
      file.rename(backup, path)
      backup <- NULL
    }
    simtab_abort_export(c(
      "Could not publish the completed export to {.file {path}}.",
      "i" = "The temporary output was removed and any prior file was restored.",
      "v" = "Choose a writable destination."
    ))
  }
  if (!is.null(backup) && file.exists(backup)) {
    unlink(backup)
    backup <- NULL
  }

  invisible(path)
}

#########
# WORD AND POWERPOINT EXPORT WRAPPERS
# Render tables to flextable objects and save into Office documents.

#' Export a SimtablR table to a Word (.docx) file
#'
#' @param x A SimtablR object with an `as_flextable()` method (e.g. from
#'   [table1()] or [tb()]).
#' @param path Output file path. A missing `.docx` suffix is added; any other
#'   suffix is rejected.
#' @param footnotes Optional character vector passed to table renderers that
#'   support footnotes.
#' @param methods Logical; when `TRUE`, append recorded [as_methods()] prose
#'   after the exported table(s).
#' @param overwrite Logical. Existing files are protected by default; pass
#'   `TRUE` to replace the destination explicitly.
#' @param ... Passed to `flextable::save_as_docx()` (e.g. page properties).
#' @return Invisibly returns the normalized output path.
#' @details Exports are written to a temporary file in the destination
#'   directory and published only after the backend succeeds. Existing files
#'   are never changed unless `overwrite = TRUE`.
#' @seealso [export_pptx()], [export_xlsx()], [table1()]
#' @examples
#' \dontrun{
#' data(epitabl)
#' table1(epitabl, c("age", "sex"), by = "adjudicated_acs") |>
#'   export_docx(tempfile(fileext = ".docx"))
#' }
#' @export
export_docx <- function(x, path, footnotes = NULL, methods = FALSE, overwrite = FALSE, ...) {
  UseMethod("export_docx")
}

#' @export
export_docx.default <- function(x, path, footnotes = NULL, methods = FALSE, overwrite = FALSE, ...) {
  .check_exportable(x)
  target <- .prepare_export_path(path, "docx", overwrite = overwrite)
  .require_export_pkg("flextable")
  .require_export_pkg("officer")
  ft <- flextable::as_flextable(x, footnotes = footnotes)
  .write_export_transaction(target, function(temporary) {
    if (isTRUE(methods)) {
      doc <- officer::read_docx()
      doc <- flextable::body_add_flextable(doc, ft)
      doc <- officer::body_add_par(doc, as_methods(x), style = "Normal")
      print(doc, target = temporary)
    } else {
      flextable::save_as_docx(ft, path = temporary, ...)
    }
  })
  message(sprintf("Table exported to: %s", target$path))
  invisible(target$path)
}

#' @export
export_docx.simtab_spec <- function(x, path, footnotes = NULL, methods = FALSE, overwrite = FALSE, ...) {
  export_docx(
    evaluate(x), path = path, footnotes = footnotes, methods = methods,
    overwrite = overwrite, ...
  )
}

#' @export
export_docx.simtab_report <- function(x, path, footnotes = NULL, methods = FALSE, overwrite = FALSE, ...) {
  target <- .prepare_export_path(path, "docx", overwrite = overwrite)
  .require_export_pkg("flextable")
  .require_export_pkg("officer")
  fts <- flextable::as_flextable(x, footnotes = footnotes)
  .write_export_transaction(target, function(temporary) {
    if (isTRUE(methods)) {
      doc <- officer::read_docx()
      for (ft in fts) {
        doc <- flextable::body_add_flextable(doc, ft)
      }
      doc <- officer::body_add_par(doc, as_methods(x), style = "Normal")
      print(doc, target = temporary)
    } else {
      flextable::save_as_docx(values = fts, path = temporary, ...)
    }
  })
  message(sprintf("Report exported to: %s", target$path))
  invisible(target$path)
}

#' Export a SimtablR table to a PowerPoint (.pptx) file
#'
#' @param x A SimtablR object with an `as_flextable()` method.
#' @param path Output file path. A missing `.pptx` suffix is added; any other
#'   suffix is rejected.
#' @param font_size Font size applied before export. Default `14`.
#' @inheritParams export_docx
#' @param ... Passed to `flextable::save_as_pptx()`.
#' @return Invisibly returns the normalized output path.
#' @seealso [export_docx()], [export_xlsx()]
#' @examples
#' \dontrun{
#' data(epitabl)
#' table1(epitabl, c("age", "sex"), by = "adjudicated_acs") |>
#'   export_pptx(tempfile(fileext = ".pptx"))
#' }
#' @export
export_pptx <- function(x, path, font_size = 14, overwrite = FALSE, ...) {
  UseMethod("export_pptx")
}

#' @export
export_pptx.default <- function(x, path, font_size = 14, overwrite = FALSE, ...) {
  .check_exportable(x)
  target <- .prepare_export_path(path, "pptx", overwrite = overwrite)
  .require_export_pkg("flextable")
  .require_export_pkg("officer")
  ft <- flextable::as_flextable(x)
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::autofit(ft)
  .write_export_transaction(target, function(temporary) {
    flextable::save_as_pptx(ft, path = temporary, ...)
  })
  message(sprintf("Table exported to: %s", target$path))
  invisible(target$path)
}

#' @export
export_pptx.simtab_spec <- function(x, path, font_size = 14, overwrite = FALSE, ...) {
  export_pptx(
    evaluate(x), path = path, font_size = font_size,
    overwrite = overwrite, ...
  )
}

#' @export
export_pptx.simtab_report <- function(x, path, font_size = 14, overwrite = FALSE, ...) {
  target <- .prepare_export_path(path, "pptx", overwrite = overwrite)
  .require_export_pkg("flextable")
  .require_export_pkg("officer")
  fts <- flextable::as_flextable(x)
  fts <- lapply(fts, function(ft) {
    ft <- flextable::fontsize(ft, size = font_size, part = "all")
    flextable::autofit(ft)
  })
  .write_export_transaction(target, function(temporary) {
    flextable::save_as_pptx(values = fts, path = temporary, ...)
  })
  message(sprintf("Report exported to: %s", target$path))
  invisible(target$path)
}

#########
# EXCEL WORKBOOK EXPORT AND DATA DICTIONARY WRITERS
# Multi-sheet workbook serialization with display, machine-readable, and dictionary tabs.

#' Export a SimtablR table to an Excel (.xlsx) file
#'
#' Writes three worksheets: a long machine-readable tidy sheet with numeric
#' statistic cells, the formatted display sheet, and a data dictionary, named
#' `machine-readable`, `display`, and `data-dictionary`. Reports write one
#' display sheet per item instead of a single `display` sheet.
#'
#' @param x A SimtablR object with an `as.data.frame()` method.
#' @param path Output file path. A missing `.xlsx` suffix is added; any other
#'   suffix is rejected.
#' @inheritParams export_docx
#' @param ... Passed to `openxlsx::writeData()`.
#' @return Invisibly returns the normalized output path.
#' @seealso [export_docx()], [export_pptx()]
#' @examples
#' \dontrun{
#' data(epitabl)
#' table1(epitabl, c("age", "sex"), by = "adjudicated_acs") |>
#'   export_xlsx(tempfile(fileext = ".xlsx"))
#' }
#' @export
export_xlsx <- function(
  x,
  path,
  overwrite = FALSE,
  ...
) {
  UseMethod("export_xlsx")
}

#' @export
export_xlsx.default <- function(
  x,
  path,
  overwrite = FALSE,
  ...
) {
  .check_exportable(x)
  target <- .prepare_export_path(path, "xlsx", overwrite = overwrite)
  .require_export_pkg("openxlsx")
  machine_sheet <- "machine-readable"
  sheet <- "display"
  dict_sheet <- "data-dictionary"
  x <- .compute_if_spec(x)
  display <- as.data.frame(x, tidy = FALSE)
  machine <- .export_machine_frame(x)
  dictionary <- .export_data_dictionary(x, machine)

  attr(display, "row_type") <- NULL
  attr(display, "row_var") <- NULL
  attr(display, "row_level") <- NULL
  attr(machine, "confusion_matrix") <- NULL

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, machine_sheet)
  openxlsx::writeData(wb, machine_sheet, machine, ...)
  openxlsx::freezePane(wb, machine_sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, machine_sheet, cols = seq_len(ncol(machine)), widths = "auto")

  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, display, ...)
  hs <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::addStyle(wb, sheet, hs, rows = 1, cols = seq_len(ncol(display)), gridExpand = TRUE)
  openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(display)), widths = "auto")

  openxlsx::addWorksheet(wb, dict_sheet)
  openxlsx::writeData(wb, dict_sheet, dictionary, ...)
  openxlsx::addStyle(wb, dict_sheet, hs, rows = 1, cols = seq_len(ncol(dictionary)), gridExpand = TRUE)
  openxlsx::freezePane(wb, dict_sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, dict_sheet, cols = seq_len(ncol(dictionary)), widths = "auto")

  .write_export_transaction(target, function(temporary) {
    openxlsx::saveWorkbook(wb, temporary, overwrite = TRUE)
  })
  message(sprintf("Table exported to: %s", target$path))
  invisible(target$path)
}

#' @export
export_xlsx.simtab_spec <- function(
  x,
  path,
  overwrite = FALSE,
  ...
) {
  export_xlsx(
    evaluate(x),
    path = path,
    overwrite = overwrite,
    ...
  )
}

#' @export
export_xlsx.simtab_report <- function(
  x,
  path,
  overwrite = FALSE,
  ...
) {
  target <- .prepare_export_path(path, "xlsx", overwrite = overwrite)
  .require_export_pkg("openxlsx")
  x <- validate_simtab_report(x)
  machine_sheet <- "machine-readable"
  dict_sheet <- "data-dictionary"

  wb <- openxlsx::createWorkbook()
  hs <- openxlsx::createStyle(textDecoration = "bold")
  used_sheets <- c(machine_sheet, dict_sheet)

  for (nm in names(x$items)) {
    sheet_name <- .report_sheet_name(nm, used = used_sheets)
    used_sheets <- c(used_sheets, sheet_name)
    display <- as.data.frame(x$items[[nm]], tidy = FALSE)
    attr(display, "row_type") <- NULL
    attr(display, "row_var") <- NULL
    attr(display, "row_level") <- NULL

    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, display, ...)
    openxlsx::addStyle(wb, sheet_name, hs, rows = 1, cols = seq_len(ncol(display)), gridExpand = TRUE)
    openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(display)), widths = "auto")
  }

  machine <- .report_bind_item_frames(x$items, .export_machine_frame)
  dictionary <- .report_bind_item_frames(x$items, function(item) {
    machine_item <- .export_machine_frame(item)
    .export_data_dictionary(item, machine_item)
  })

  openxlsx::addWorksheet(wb, machine_sheet)
  openxlsx::writeData(wb, machine_sheet, machine, ...)
  openxlsx::addStyle(wb, machine_sheet, hs, rows = 1, cols = seq_len(ncol(machine)), gridExpand = TRUE)
  openxlsx::freezePane(wb, machine_sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, machine_sheet, cols = seq_len(ncol(machine)), widths = "auto")

  openxlsx::addWorksheet(wb, dict_sheet)
  openxlsx::writeData(wb, dict_sheet, dictionary, ...)
  openxlsx::addStyle(wb, dict_sheet, hs, rows = 1, cols = seq_len(ncol(dictionary)), gridExpand = TRUE)
  openxlsx::freezePane(wb, dict_sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, dict_sheet, cols = seq_len(ncol(dictionary)), widths = "auto")

  .write_export_transaction(target, function(temporary) {
    openxlsx::saveWorkbook(wb, temporary, overwrite = TRUE)
  })
  message(sprintf("Report exported to: %s", target$path))
  invisible(target$path)
}

#' Extract tidy machine-readable data frame for workbook export
#' @keywords internal
#' @noRd
.export_machine_frame <- function(x) {
  out <- as.data.frame(x, tidy = TRUE)
  infinite_numeric <- vapply(
    out,
    function(column) is.numeric(column) && any(is.infinite(column)),
    logical(1)
  )
  out[infinite_numeric] <- lapply(out[infinite_numeric], as.character)
  rownames(out) <- NULL
  out
}

#' Construct export data dictionary mapping variables, labels, and summaries
#' @keywords internal
#' @noRd
.export_data_dictionary <- function(x, machine) {
  variable <- if ("variable" %in% names(machine)) {
    machine$variable
  } else if ("term" %in% names(machine)) {
    machine$term
  } else if ("metric" %in% names(machine)) {
    machine$metric
  } else {
    rep(NA_character_, nrow(machine))
  }

  level <- if ("level" %in% names(machine)) {
    machine$level
  } else if ("group" %in% names(machine)) {
    machine$group
  } else if ("outcome" %in% names(machine)) {
    machine$outcome
  } else {
    rep(NA_character_, nrow(machine))
  }

  measure <- if ("stat" %in% names(machine)) {
    machine$stat
  } else if ("metric" %in% names(machine)) {
    "estimate"
  } else {
    rep("estimate", nrow(machine))
  }

  labels <- variable
  if (!is.null(x$meta$labels)) {
    idx <- match(variable, names(x$meta$labels))
    labels[!is.na(idx)] <- unname(x$meta$labels[idx[!is.na(idx)]])
  }
  if (!is.null(x$meta$predictor_labels)) {
    idx <- match(variable, names(x$meta$predictor_labels))
    labels[!is.na(idx)] <- unname(x$meta$predictor_labels[idx[!is.na(idx)]])
  }
  if (inherits(x, "simtab_diag") && "metric" %in% names(machine)) {
    labels <- machine$metric
  }

  out <- unique(data.frame(
    variable = as.character(variable),
    level = as.character(level),
    label = as.character(labels),
    measure = as.character(measure)
  ))
  dictionary <- tryCatch(
    .codebook_frame(x$used$ref$data, labels = .codebook_labels(x)),
    error = function(e) NULL
  )
  if (!is.null(dictionary)) {
    idx <- match(out$variable, dictionary$variable)
    for (col in c("type", "levels/units", "n_missing (%)", "summary")) {
      out[[col]] <- ""
      matched <- !is.na(idx)
      out[[col]][matched] <- dictionary[[col]][idx[matched]]
    }
  }
  rownames(out) <- NULL
  out
}
