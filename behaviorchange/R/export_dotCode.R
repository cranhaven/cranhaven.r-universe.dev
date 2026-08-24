### Taken from DiagrammeR's DiagrammeR::export_graph()
export_dotCode <- function (dot_code,
                            file_name = NULL,
                            file_type = tools::file_ext(file_name),
                            width = 1600,
                            height = NULL) {

  ### Cleaned up considerably since the situation is simpler and we won't
  ### export this function

  if  (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
    stop("You need the `DiagrammeRsvg` package, please install it using:\n\n",
         "    install.packages('DiagrammeRsvg');\n");
  }

  if  (!requireNamespace("rsvg", quietly = TRUE)) {
    stop("You need the `rsvg` package, please install it using:\n\n",
         "    install.packages('rsvg');\n");
  }

  if (tolower(file_type) == "png") {
    rsvg::rsvg_png(charToRaw(DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot_code))),
                   file = file_name, width = width, height = height);
  } else if (tolower(file_type) == "pdf") {
    rsvg::rsvg_pdf(charToRaw(DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot_code))),
                   file = file_name, width = width, height = height);
  } else if (tolower(file_type) == "svg") {
    rsvg::rsvg_svg(charToRaw(DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot_code))),
                   file = file_name, width = width, height = height);
  } else if (tolower(file_type) == "ps") {
    rsvg::rsvg_ps(charToRaw(DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot_code))),
                  file = file_name, width = width, height = height);
  } else {
    stop("I cannot export to the extension you specified (", file_type, ").");
  }

}
