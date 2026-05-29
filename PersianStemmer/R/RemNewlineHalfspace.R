RemNewlineHalfspace <-
function(texts){
  # removes \n \r \t \f \v
  texts <- gsub('\n|\r|\t|\f|\v',' ',texts)
  # fixes Half Spaces
  texts <- gsub('\u200C',' ',texts)
  # removes extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}
