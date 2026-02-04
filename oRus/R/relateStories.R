relateStories <- function(groupedStories, groupsNumber = 4, topGroups = 1, ignoreWordsList = NULL) {
  # Call the other function
  parsedStories <- parseRelatedStories(groupedStories, groupsNumber, topGroups, ignoreWordsList)
  parsedStories[[2]] <- parsedStories[[2]][,c("storyId", "who",
                                              "what", "why", "type",
                                              "goal", "location",
                                              "group", "belonging")]

  # Return it
  return( parsedStories )
}






parseRelatedStories <- function(stories, groupsNumber, topGroups, ignoreWordsList) {
  # Get stop words
  stop_words <- stop_words
  # Get words to ignore
  if(is.null(ignoreWordsList))
    ignoreWordsList <- ignoreWords

  # First of all, get all the whats
  whats <- preProcessWhats(stories)
  colnames(whats) <- c("id", "text")


  # Tidy text now
  tidy.whats <- whats %>% dplyr::as_tibble()
  # Unnest tokens
  clean.whats <- tidy.whats %>% tidytext::unnest_tokens(word, text)
  # Remove stop words and count words
  clean.whats <- clean.whats %>%
    dplyr::anti_join(stop_words) %>%
    dplyr::anti_join(ignoreWordsList) %>%
    count(id, word, sort = TRUE) %>%
    ungroup()
  rm(stop_words)

  # Cast as DTM
  dtm.whats <- clean.whats %>% tidytext::cast_dtm(id, word, n)

  # Create the model
  lda.whats <- topicmodels::LDA(dtm.whats, k = groupsNumber, control = list(seed = 1234))


  # Get top-words
  topWords <- getTopWords(lda.whats)
  classification <- classifyStories(lda.whats, whats, stories, topGroups)

  # Return this for now

  return( list(topWords, classification) )
}





preProcessWhats <- function(stories) {
  # Isolate the whats
  whats <- stories[, c("storyId", "what")]

  # Now read the keywords and unlist them
  keywords <- rle(unlist(readKeywords()))$values

  # For every what
  for(w in 1:nrow(whats)) {
    # For every keyword
    for(k in keywords) {
      # If the what starts with this one
      if(whats[w, "what"] %>% startsWith(k)) {
        # Replace it
        whats[w, "what"] <- sub(k, "", whats[w, "what"]) %>% trimws()
        break
      }
    }
  }

  # Return this thing
  return(whats)
}







getTopWords <- function(lda.whats) {
  # This allows us to see top terms in each topic, may be useful
  beta.whats <- tidytext::tidy(lda.whats, matrix = "beta")

  # Get the top terms
  topWords <- beta.whats %>%
    group_by(topic) %>%
    top_n(3, beta) %>%
    ungroup() %>%
    dplyr::arrange(topic -beta)

  # Now return
  return(topWords)
}





classifyStories <- function(lda.whats, whats, stories, topGroups) {
  # Now topic per user story what
  gamma.whats <- tidytext::tidy(lda.whats, matrix = "gamma")
  gamma.best <- gamma.whats %>% group_by(document) %>% dplyr::top_n(topGroups, gamma)
  colnames(gamma.best) <- c("id", "topic", "belonging")

  # Now merge back together
  merge.step1 <- merge(whats, gamma.best, by = "id")
  colnames(merge.step1) <- c("storyId", "what", "group", "belonging")
  merge.step2 <- merge(stories, merge.step1, by = "storyId")

  merge.step2$"what.y" <- NULL
  colnames(merge.step2)[3] <- "what"

  # Return it
  return(merge.step2)
}
