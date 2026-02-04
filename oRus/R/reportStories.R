#' Reporting Stories
#'
#' @family Simplified Process
#'
#' @description This function allows you to write the reports for the user stories
#'     if you didn't write them before on the analysis function. The key input
#'     is the output of `oRus::analyseStories()`.#'
#'
#'
#' @param stories The stories produced by the analysis function.
#' @param sheetFilePath The path and filename of the Excel sheet that will be
#'     stored; must include the `*.xlsx` extension. If no value is passed, the
#'     file will not be written.
#' @param reportFilePath The path where the extensive report will be stored. It
#'     must include the correct extension (according to the type selected in the
#'     following argument). If no value is passed, the report won't be generated.
#' @param outputType The type of document to be generated (from an RMarkdown). By default
#'     it is a PDF file. Options are: `html_output` or `pdf_output`.
#'
#'
#' @import dplyr
#' @importFrom rmarkdown render
#' @importFrom xlsx write.xlsx
#'
#'
#' @export
#'
#'
reportStories <- function(stories, sheetFilePath = NULL, reportFilePath = NULL,
                          outputType = "html_document") {

  # 5) Write, if there is a location
  if(!is.null(sheetFilePath)) {
    # Write the stories first
    write.xlsx(stories[[2]], file = sheetFilePath,
               sheetName = "Stories", row.names = FALSE)

    # Then add the grouping information
    write.xlsx(stories[[1]] %>% as.data.frame(), file = sheetFilePath,
               sheetName = "TopWords", append = TRUE, row.names = FALSE)
  }


  # 6) Report, if there is a location
  if(!is.null(reportFilePath)) {
    # Print the render
    render(input = "inst/extdata/storiesReport.Rmd", output_dir = reportFilePath,
           output_format = outputType, params = list(storiesdf = stories[[2]]))
  }

}
