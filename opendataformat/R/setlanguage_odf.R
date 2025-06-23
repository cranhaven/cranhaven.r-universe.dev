#' @title Change language of dataframe metadata
#'
#' @description
#' Changes the active language of a dataframe with metadata for the
#' docu_odf function.
#'
#' @param dataframe R data frame (df) enriched with metadata in the
#' odf-format.
#'
#' @param language
#' Select the language to which you want to switch the metadata.
#'
#' @return Dataframe
#'
#' @examples
#' # get example data from the opendataformat package
#' df  <-  get(data("data_odf"))
#'
#' # Switch dataset df to language "en"
#' df  <-  setlanguage_odf(df, language = "en")
#'
#' # Display dataset information for dataset df in language "en"
#' docu_odf(df)
#'
#' @export

setlanguage_odf <- function(dataframe, language) {
  
  if (!("data.frame" %in% class(dataframe))){
    stop("x must be a tibble or data.frame")
  }
  df_languages <-  attr(dataframe, "languages")
  col_languages <- unique(unlist(lapply(dataframe, function(x) attr(x, "languages"))))
  #check if language is available for the dataframe
  if (language %in% c(df_languages, col_languages)) {
    attr(dataframe, "lang") <- language
    attributes(dataframe)[["label"]] <- attr(dataframe,
                                             paste0("label_", language))
    for (var in names(dataframe)){
      attr(dataframe[[var]], "lang") <- language
      attr(dataframe[[var]], "label") <-
        attributes(dataframe[[var]])[[paste0("label_", language)]]
    }
  } else {
    stop(paste0("Language '", language, "' not available for the dataset."))
  }
  return(dataframe)
}
