

#' Write Thai LaTeX Preamble to Specified Path
#'
#' @param path_abs Absolute path to write to
#' @param thai_font Thai font
#' @param line_spacing Line Spacing
#'
#' @return path of a written file
#' @noRd
#'
write_thai_preamble <- function(path_abs,
                                thai_font = "TH Sarabun New",
                                line_spacing = 1.5
) {

  # Validate Metadata
  thaipdf_config_validate(thai_font = thai_font, line_spacing = line_spacing)
  # Metadata: Named List as Pandoc Var
  metadata <- list(thai_font = thai_font, line_spacing = line_spacing)

  # All relevant file paths in PKG
  paths <- thaipdf_paths()

  # Main Engine:
  ## Render to Output at Specified Location
  write_path <- rmarkdown::pandoc_template(
    metadata = metadata,
    template = paths[["path_temp"]], # thai-preamble template path
    output = path_abs
  )

  write_path
}
