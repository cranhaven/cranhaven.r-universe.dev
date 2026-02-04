#' Initial User Story Parse
#'
#' @family Basic Functions
#'
#' @description This function will help you parse a set of stories into a dataframe, where
#'     one row is each user story; The user story gets splitted into who, what
#'     and why sections, according to the use of keywords. The file must be a
#'     text file written in English, with one user story per row.
#'
#' @param url The URL of the text file to be parsed. Every user story must be
#'     in a single line, and written in English. Punctuation is irrelevant as
#'     it is processed out.
#'     For this to work, user stories should follow the who, what, why
#'     template, with keywords: \code{"as a/an "}, \code{" I want to "},
#'     \code{" so that "}, respectively.
#'
#'
#' @return A dataframe of three colums, representing sections who, what, why
#'    of the user stories. There is one row per user story, and they may not
#'    have the "why" part if it wasn't added. Using incorrect keywords means
#'    incorrect parsing, so be careful.
#'
#'
#' @export
#'
#'
#' @importFrom dplyr %>%
#' @import stringr
#' @importFrom tm removePunctuation
#' @importFrom utils read.delim
#'
#' @examples
#' # Analyse without reports
#' dataPath <- example_stories()
#' stories <- readStories(dataPath)
#'
#' # Print some information
#' head(dplyr::as_tibble(stories))
#'
readStories <- function(url) {
  # Read original stories
  stories <- read.delim(url, header = FALSE, stringsAsFactors  = FALSE)[[1]]

  # Clean the stories
  # all to lower case, and remove punctuation
  clean.stories <- tolower(stories) %>% removePunctuation()

  # Convert to a data-frame with three columns, WHO-WHAT-WHY
  split.stories <- lapply(clean.stories, FUN = splitUserStory)
  frame.stories <- as.data.frame(do.call(rbind, split.stories))
  colnames(frame.stories) <- c("who", "what", "why")

  # Remove the levels
  frame.stories$who <- as.factor(frame.stories$who)
  frame.stories$what <- as.character(frame.stories$what)
  frame.stories$why <- as.character(frame.stories$why)

  frame.stories$storyId <- seq.int(nrow(frame.stories))


  # Return the data
  return( frame.stories[,c("storyId", "who", "what", "why")] )
}










splitUserStory <- function(userStory) {
  # Start splitting the WHO
  split <- str_split(userStory, " i want to ", simplify = TRUE)


  # Check if it contains an or a
  template1 <- "as a "
  template2 <- "as an "
  who <- split[1,1]
  who <- if(startsWith(who, template1)) str_split(who, template1, simplify = TRUE) else str_split(who, template2, simplify = TRUE)
  who <- who[1,2]


  # Now get the WHAT
  split2 <- str_split(split[1,2], " so that ", simplify = TRUE)
  what <- split2[1,1]


  # Now the WHY
  why <- if(ncol(split2) > 1) split2[1,2] else NA


  # Delete
  rm(template1, template2)
  # Return the data
  return( c(who, what, why) )
}










