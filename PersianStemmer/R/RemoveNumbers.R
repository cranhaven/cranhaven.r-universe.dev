RemoveNumbers <-
function(texts){
  texts <- gsub('[\u06f0-\u06f9]',' ',texts)
  # removes extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}
