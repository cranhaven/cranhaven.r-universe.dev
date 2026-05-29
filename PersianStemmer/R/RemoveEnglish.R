RemoveEnglish <-
function(texts){
  texts <- gsub('[A-Z]|[a-z]','',texts)
  # removes extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}
