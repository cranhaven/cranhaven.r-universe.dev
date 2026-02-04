#' Analysing Stories
#'
#' @export
#'
#' @family Simplified Process
#'
#' @description Main function that fully automates the process of analysing a
#'     dataset of user stories. It can also write results as an Excel sheet in
#'     a given directory, and generate an advanced report with highlights of
#'     missing features.
#'
#' @param storiesFile The path an name to a text file containing one user story
#'     per line. They need to be written in English.
#' @param groupsNumber The number of groups you want to generate between user stories.
#' @param topGroups How many groups per stories you want to keep. The default is 1.
#' @param sheetFilePath The path and filename of the Excel sheet that will be
#'     stored; must include the `*.xlsx` extension. If no value is passed, the
#'     file will not be written.
#' @param reportFilePath The path where the extensive report will be stored. It
#'     must include the correct extension (according to the type selected in the
#'     following argument). If no value is passed, the report won't be generated.
#' @param outputType The type of document to be generated (from an RMarkdown). By default
#'     it is a PDF file.
#' @param ignoreWordsList The list of words that you want to avoid using during
#'     the grouping of user stories. If nothing is passed, a default list
#'     will be used.
#'
#' @return A list of two datasets: the first one contains the stories split up,
#'     classified in types, analysed and grouped. Second dataframe contains top
#'     words per group and the belonging value of the word.
#'
#'
#' @import dplyr
#'
#' @importFrom rmarkdown render
#' @importFrom xlsx write.xlsx
#'
#' @importFrom tidytext unnest_tokens
#' @importFrom tidytext cast_dtm
#' @importFrom tidytext tidy
#'
#' @importFrom topicmodels LDA
#'
#' @examples
#' # Libraries for the example
#' library(reshape2)
#'
#' # Transform the stories
#' fileUrl <- example_stories()
#' stories <- analyseStories(fileUrl, 7)
#'
#' # Print some information
#' head(dplyr::as_tibble(stories[[2]]))
#'
#' head(stories[[1]])
#'
analyseStories <- function(storiesFile, groupsNumber, topGroups = 1,
                           sheetFilePath = NULL, reportFilePath = NULL,
                           outputType = "pdf_document", ignoreWordsList = NULL) {

  # 1) read the stories
  stories <- readStories(storiesFile)

  # 2) Group them on input, output, constraint, objective, scenario
  stories_grouped <- groupStories(stories)

  # 3) Process the stories, especially, input, output, objectives
  stories_proc <- processStories(stories_grouped)

  # 4) Relate the stories using LDA
  stories_related <- relateStories(stories_proc, groupsNumber, topGroups, ignoreWordsList)


  # 5) Write, if there is a location
  if(!is.null(sheetFilePath)) {
    # Write the stories first
    write.xlsx(stories_related[[2]], file = sheetFilePath,
               sheetName = "Stories", row.names = FALSE)

    # Then add the grouping information
    write.xlsx(stories_related[[1]] %>% as.data.frame(), file = sheetFilePath,
               sheetName = "TopWords", append = TRUE, row.names = FALSE)
  }


  # 6) Report, if there is a location
  if(!is.null(reportFilePath)) {
    # Print the render
    rmarkdown::render(input = "inst/extdata/storiesReport.Rmd", output_dir = reportFilePath,
           output_format = outputType, params = list(storiesdf = stories_related[[2]]))
  }

  # 7) Return the generated datasets
  return(stories_related)
}
