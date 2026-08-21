apa7_packages_available <- function() {
  requireNamespace("officer", quietly = TRUE) &&
    requireNamespace("flextable", quietly = TRUE)
}

require_apa7_packages <- function() {
  if (!apa7_packages_available()) {
    stop(
      paste0(
        "APA 7 Word export requires the optional 'officer' and 'flextable' packages. ",
        "Install them with install.packages(c('officer', 'flextable'))."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

apa7_table_data <- function(value, language = "en") {
  if (is.null(value)) return(NULL)
  table_data <- as.data.frame(value, stringsAsFactors = FALSE, check.names = FALSE)

  if (!nrow(table_data)) {
    message_text <- fafa_text(language, "No results were available.", "Kullan\u0131labilir sonu\u00e7 yok.")
    return(data.frame(Message = message_text, check.names = FALSE))
  }

  current_rownames <- rownames(table_data)
  default_rownames <- as.character(seq_len(nrow(table_data)))
  if (!is.null(current_rownames) && !identical(current_rownames, default_rownames)) {
    row_label <- fafa_text(language, "Result", "Sonu\u00e7")
    table_data <- cbind(stats::setNames(data.frame(current_rownames), row_label), table_data)
  }

  table_data[] <- lapply(table_data, function(column) {
    if (is.list(column)) {
      return(vapply(column, function(item) paste(item, collapse = ", "), character(1)))
    }
    if (is.numeric(column)) return(round(column, 3))
    as.character(column)
  })
  names(table_data)[!nzchar(names(table_data))] <- paste0(
    fafa_text(language, "Column", "S\u00fctun"),
    seq_len(sum(!nzchar(names(table_data))))
  )
  table_data
}

apa7_text_run <- function(text, bold = FALSE, italic = FALSE,
                          font_size = 12) {
  officer::ftext(
    as.character(text),
    prop = officer::fp_text(
      font.family = "Times New Roman",
      font.size = font_size,
      bold = bold,
      italic = italic,
      color = "#000000"
    )
  )
}

apa7_paragraph <- function(text, align = "left", bold = FALSE,
                           italic = FALSE, first_line = 0,
                           font_size = 12, keep_with_next = FALSE) {
  officer::fpar(
    apa7_text_run(text, bold = bold, italic = italic, font_size = font_size),
    fp_p = officer::fp_par(
      text.align = align,
      line_spacing = 2,
      padding.top = 0,
      padding.bottom = 0,
      first_line = first_line,
      keep_with_next = keep_with_next
    )
  )
}

apa7_flextable <- function(value, language = "en") {
  table_data <- apa7_table_data(value, language)
  if (is.null(table_data)) return(NULL)

  result <- flextable::flextable(table_data)
  result <- flextable::theme_booktabs(result)
  result <- flextable::font(
    result,
    fontname = "Times New Roman",
    part = "all"
  )
  result <- flextable::fontsize(result, size = 10, part = "all")
  result <- flextable::bold(result, part = "header")
  result <- flextable::align(result, align = "center", part = "header")

  character_columns <- names(table_data)[vapply(table_data, function(column) {
    is.character(column) || is.factor(column)
  }, logical(1))]
  numeric_columns <- setdiff(names(table_data), character_columns)
  if (length(character_columns)) {
    result <- flextable::align(result, j = character_columns, align = "left", part = "body")
  }
  if (length(numeric_columns)) {
    result <- flextable::align(result, j = numeric_columns, align = "center", part = "body")
  }

  result <- flextable::padding(result, padding = 4, part = "all")
  result <- flextable::valign(result, valign = "center", part = "all")
  result <- flextable::autofit(result)
  result <- flextable::fit_to_width(result, max_width = 6.25)
  flextable::set_table_properties(result, layout = "autofit", width = 1)
}

#' Write an APA 7 Word report
#'
#' @param file Output `.docx` path.
#' @param title Report title.
#' @param sections List of report sections. Each section may contain `title`,
#'   `text`, `table`, and `note` fields.
#' @param subtitle Optional subtitle.
#' @param language Interface language (`"en"` or `"tr"`).
#' @return The output path, invisibly.
#' @keywords internal
write_apa7_report <- function(file, title, sections, subtitle = NULL,
                              language = "en") {
  require_apa7_packages()
  language <- fafa_language(language)

  output_file <- file
  temporary_docx <- NULL
  if (!grepl("[.]docx$", output_file, ignore.case = TRUE)) {
    temporary_docx <- tempfile(fileext = ".docx")
    output_file <- temporary_docx
    on.exit(unlink(temporary_docx, force = TRUE), add = TRUE)
  }

  document <- officer::read_docx()
  document <- officer::body_add_fpar(
    document,
    apa7_paragraph(title, align = "center", bold = TRUE, keep_with_next = TRUE)
  )
  if (!is.null(subtitle) && nzchar(trimws(subtitle))) {
    document <- officer::body_add_fpar(
      document,
      apa7_paragraph(subtitle, align = "center")
    )
  }
  document <- officer::body_add_par(document, "", style = "Normal")

  table_number <- 0L
  for (section in sections) {
    if (is.null(section) || !is.list(section)) next

    section_title <- section$title %||% ""
    if (nzchar(section_title)) {
      document <- officer::body_add_fpar(
        document,
        apa7_paragraph(
          section_title,
          align = "center",
          bold = TRUE,
          keep_with_next = TRUE
        )
      )
    }

    section_text <- section$text %||% character(0)
    section_text <- as.character(section_text)
    section_text <- section_text[nzchar(trimws(section_text))]
    for (paragraph_text in section_text) {
      document <- officer::body_add_fpar(
        document,
        apa7_paragraph(paragraph_text, first_line = 0.5)
      )
    }

    if (!is.null(section$table)) {
      table_number <- table_number + 1L
      table_label <- paste(
        fafa_text(language, "Table", "Tablo"),
        table_number
      )
      table_title <- section$table_title %||% section_title

      document <- officer::body_add_fpar(
        document,
        apa7_paragraph(table_label, bold = TRUE, keep_with_next = TRUE)
      )
      if (nzchar(table_title)) {
        document <- officer::body_add_fpar(
          document,
          apa7_paragraph(table_title, italic = TRUE, keep_with_next = TRUE)
        )
      }
      document <- flextable::body_add_flextable(
        document,
        value = apa7_flextable(section$table, language),
        align = "center",
        split = TRUE
      )
    }

    section_note <- section$note %||% ""
    if (nzchar(trimws(section_note))) {
      note_label <- fafa_text(language, "Note.", "Not.")
      document <- officer::body_add_fpar(
        document,
        officer::fpar(
          apa7_text_run(note_label, italic = TRUE, font_size = 10),
          apa7_text_run(paste0(" ", section_note), font_size = 10),
          fp_p = officer::fp_par(
            text.align = "left",
            line_spacing = 2,
            padding.top = 0,
            padding.bottom = 0
          )
        )
      )
    }
    document <- officer::body_add_par(document, "", style = "Normal")
  }

  page_number <- officer::fpar(
    officer::run_word_field(
      "PAGE",
      prop = officer::fp_text(
        font.family = "Times New Roman",
        font.size = 12
      )
    ),
    fp_p = officer::fp_par(text.align = "right")
  )
  apa_section <- officer::prop_section(
    page_size = officer::page_size(width = 8.5, height = 11, orient = "portrait"),
    page_margins = officer::page_mar(
      top = 1,
      bottom = 1,
      left = 1,
      right = 1,
      header = 0.5,
      footer = 0.5
    ),
    header_default = officer::block_list(page_number)
  )
  document <- officer::body_set_default_section(document, apa_section)
  print(document, target = output_file)

  if (!is.null(temporary_docx)) {
    copied <- file.copy(output_file, file, overwrite = TRUE, copy.mode = FALSE)
    if (!isTRUE(copied)) {
      stop("The Word report could not be copied to the download file.", call. = FALSE)
    }
  }

  invisible(file)
}

fit_measure_value <- function(fit_table, measure) {
  if (!is.data.frame(fit_table) || !all(c("Measure", "Value") %in% names(fit_table))) {
    return(NA_character_)
  }
  index <- match(measure, fit_table$Measure)
  if (is.na(index)) NA_character_ else as.character(fit_table$Value[[index]])
}

apa7_cfa_narrative <- function(fit_table, language = "en") {
  value <- function(measure) fit_measure_value(fit_table, measure)
  chi <- value("Chi-Square")
  degrees <- value("Degrees of Freedom (df)")
  p_value <- value("p-value")
  cfi <- value("CFI")
  tli <- value("TLI (NNFI)")
  rmsea <- value("RMSEA")
  lower <- value("RMSEA 90% CI Lower")
  upper <- value("RMSEA 90% CI Upper")
  srmr <- value("SRMR")

  if (identical(fafa_language(language), "tr")) {
    return(paste0(
      "Do\u011frulay\u0131c\u0131 fakt\u00f6r analizi sonu\u00e7lar\u0131 \u03c7\u00b2(", degrees,
      ") = ", chi, ", p = ", p_value, ", CFI = ", cfi,
      ", TLI = ", tli, ", RMSEA = ", rmsea,
      ", %90 GA [", lower, ", ", upper, "], SRMR = ", srmr,
      " olarak bulunmu\u015ftur."
    ))
  }
  paste0(
    "The confirmatory factor analysis yielded \u03c7\u00b2(", degrees,
    ") = ", chi, ", p = ", p_value, ", CFI = ", cfi,
    ", TLI = ", tli, ", RMSEA = ", rmsea,
    ", 90% CI [", lower, ", ", upper, "], and SRMR = ", srmr, "."
  )
}

conventional_fit_summary <- function(fit_table, language = "en") {
  specifications <- data.frame(
    Index = c("CFI", "TLI", "RMSEA", "SRMR"),
    Measure = c("CFI", "TLI (NNFI)", "RMSEA", "SRMR"),
    Direction = c(">=", ">=", "<=", "<="),
    Cutoff = c(0.95, 0.95, 0.06, 0.08),
    stringsAsFactors = FALSE
  )
  observed <- suppressWarnings(as.numeric(vapply(
    specifications$Measure,
    function(measure) fit_measure_value(fit_table, measure),
    character(1)
  )))
  meets <- ifelse(
    specifications$Direction == ">=",
    observed >= specifications$Cutoff,
    observed <= specifications$Cutoff
  )
  result <- data.frame(
    Index = specifications$Index,
    Observed = round(observed, 3),
    Criterion = paste(specifications$Direction, format(specifications$Cutoff, nsmall = 2)),
    Result = ifelse(
      is.na(meets),
      fafa_text(language, "Not available", "Kullan\u0131lam\u0131yor"),
      ifelse(
        meets,
        fafa_text(language, "Meets", "Kar\u015f\u0131l\u0131yor"),
        fafa_text(language, "Does not meet", "Kar\u015f\u0131lam\u0131yor")
      )
    ),
    check.names = FALSE
  )
  if (identical(fafa_language(language), "tr")) {
    names(result) <- c("\u0130ndeks", "G\u00f6zlenen", "\u00d6l\u00e7\u00fct", "Sonu\u00e7")
  }
  result
}
