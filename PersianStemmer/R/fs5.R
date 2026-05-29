fs5 <-
function(word){
  if(length(word) >3){
    if(paste(word[length(word)],collapse="") == '\u06CC'){
      word <- word[1: length(word)-1]}}; return(word)}
