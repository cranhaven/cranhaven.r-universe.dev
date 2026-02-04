readKeywords <- function() {
  # Read all keywords
  objkw <- objKeywords$keywords %>% as.character()
  outkw <- outputKeywords$keyword %>% as.character()
  inkw <- inputKeywords
  conskw <- constraintKeywords
  scenskw <- scenariosKeywords

  return(list(objkw, outkw, inkw, conskw, scenskw))
}
