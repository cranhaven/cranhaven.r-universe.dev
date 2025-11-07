

#' Create a \LaTeX Preamble for Thai Language in R Markdown
#'
#' @description
#' This function creates \LaTeX preamble file needed to render Thai language in R Markdown to a PDF document.
#'
#' @details
#' Here is the steps:
#'
#' 1. Call `use_thai_preamble()`. You can set other file name by `name` argument and Thai font to use by `thai_font` argument.
#'
#' 2. Follow the instructions printed to R console.
#'
#'     *   File `thai-preamble.tex` should be created in the current working directory (by default).
#'     *  (If not already) Create R Markdown file with `pdf_document:` or `bookdown::pdf_document2` format.
#'     *  Modify YAML header  in `pdf_document:` or `bookdown::pdf_document2` option. Set `latex_engine` to `xelatex` and set to include path to the \LaTeX preamble file.
#'     *  Add \LaTeX macro `\sloppy` to the beginning of the body of R Markdown (just after YAML header).
#'
#' 3. Write some Thai language in R Markdown then knit to PDF. It's DONE!
#'
#'
#'
#' @param name (Character) Thai \LaTeX preamble file name or path of file to create, which can be relative path or absolute path. Default value is `thai-preamble.tex`.
#' @param thai_font (Character) Name of the Thai font to use. Default font is "TH Sarabun New". It can be any Thai font that installed in your system.
#' @param line_spacing (Numeric) Spacing between each line. Line spacing 1.5 is recommended for Thai language (default).
#' @param open (Logical) Open the newly created file for editing? Using default editor of `.tex` to open.
#' @param overwrite (Logical) If file already exist, do you want to overwrite?
#'
#' @return (Invisible) A path to \LaTeX preamble being created.
#' @export
#'
#' @examples
#' \dontrun{
#'  # Running this will write `thai-preamble.tex` to your working directory
#'  use_thai_preamble()
#'  # Write `thai-preamble.tex` under pre-tex/ directory (a directory must exist)
#'  use_thai_preamble(name = "pre-tex/thai-preamble.tex")
#'  # Specify Thai font to use
#'  use_thai_preamble(thai_font = "Laksaman")
#' }
#'
#'  # Example
#'  .old_wd <- setwd(tempdir())
#'  use_thai_preamble()
#'  setwd(.old_wd)
use_thai_preamble <- function(name = "thai-preamble.tex",
                              thai_font = "TH Sarabun New",
                              line_spacing = 1.5,
                              open = FALSE,
                              overwrite = FALSE
){

  ## out path relative (set extension to .tex)
  out_path_rel <- fs::path(fs::path_ext_remove(name), ext = "tex")
  ## out path absolute
  out_path_abs <- fs::path_abs(out_path_rel)
  ## File name
  out_name <- fs::path_file(out_path_abs)
  ### If file already exist, not overwrite unless instructed to do so.
  is_file_exist <- fs::file_exists(out_path_abs)
  if (is_file_exist && !overwrite) {
    cli::cli_alert_danger("file {.val {out_path_abs}} already exist, to overwrite set {.code overwrite = TRUE}")
    return(invisible(NA_character_))
  }

  write_path <- write_thai_preamble(path_abs = out_path_abs,
                      thai_font = thai_font,
                      line_spacing = line_spacing)

  # Info to Console
  cli::cli_alert_success("Writing {.val {out_name}} at {.file {write_path}}")
  cli::cli_alert_success("Thai font was set to {.val {thai_font}} in the preamble.")
  cli::cli_alert_success("Line spacing was set to {.val {line_spacing}} in the preamble.")

  # Inform what TODO
  ui_inform_yaml(out_path_rel)

  if (open) {
    fs::file_show(write_path)
  }
  # Return Path of Output file
  invisible(write_path)

}


# Inform YAML -------------------------------------------------------------



#' Print what TODO about R Markdown YAML to the R console
#'
#' @param in_header Text to `in_header` fields of YAML
#'
#' @return A character
#' @noRd
ui_inform_yaml <- function(in_header = "path-to-preamble.tex") {

  par_end <- function(){ cli::cli_par(); cli::cli_end() }

  cli::cli_h1("TODO")

  cli::cli_text("For YAML header of R Markdown in {.code pdf_document:} or {.code bookdown::pdf_document2:}")
  cli::cli_li("Set {.code latex_engine} to {.code xelatex}")
  cli::cli_li("Set to include the path to LaTeX preamble")

  par_end()

  cli::cli_rule(left = "Like This")

  message("    latex_engine: xelatex
    includes:
      in_header: ", in_header)
  message("\n")

  cli::cli_rule()

  par_end()

  cli::cli_li("Add LaTeX macro {.code \\sloppy} to the beginning of the body of R Markdown (just after YAML header).")

  par_end()

  cli::cli_text("{.strong For more details see}")
  cli::cli_li("How to include preamble in R Markdown {.url https://bookdown.org/yihui/rmarkdown-cookbook/latex-preamble.html}")
  cli::cli_li("LaTeX setting in Thai {.url http://pioneer.netserv.chula.ac.th/~wdittaya/LaTeX/LaTeXThai.pdf}")

}



