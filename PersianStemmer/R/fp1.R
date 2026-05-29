fp1 <-
function(word){
  p1 <- c('\u0627\u0628\u0631', # abar
          '\u067E\u0633\u0627') # pasa
  if(length(word) > 4){
    if(paste(word[1:3],collapse="") %in% p1){
      word <- word[4:length(word)]}}; return(word)}
